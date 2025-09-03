#' Heatmap del puntaje General por región (múltiples ediciones)
#'
#' Dibuja un mapa de calor donde las **filas** son regiones y las **columnas**
#' son ediciones. El color representa el puntaje **General** (0–10) del INCORE.
#' Solo usa las filas cuyo `pilar` comienza con *"Índice de Competitividad Regional"*.
#'
#' @param ediciones Vector numérico con 2+ ediciones (p.ej., `2019:2025`). **Obligatorio**.
#' @param regiones Vector de regiones a incluir (códigos o nombres). Use `"ALL"` para todas.
#' @param usar_codigos Si `TRUE`, traduce códigos de regiones a nombres oficiales.
#' @param ordenar Criterio para ordenar las filas (regiones):
#'   `"ninguno"` (alfabético), `"por_ultimo"` (por puntaje de la última edición),
#'   `"por_promedio"` (promedio en el rango).
#' @param paleta Paleta de relleno: `"blues"` (default), `"viridis"`, `"cividis"`, `"magma"`.
#' @param anotar Si `TRUE`, escribe el valor dentro de cada celda.
#' @param mostrar_leyenda Si `TRUE`, muestra la leyenda de colores.
#'
#' @return Un objeto `ggplot2`.
#'
#' @examples
#' # Básico (todas las regiones), orden alfabético:
#' # general_heatmap(2020:2025)
#'
#' # Ordenar por el puntaje de la última edición:
#' # general_heatmap(2020:2025, ordenar = "por_ultimo")
#'
#' # Subconjunto de regiones y anotando valores:
#' # general_heatmap(2021:2025, regiones = c("MOQ","Lima*","Arequipa","Cusco"), anotar = TRUE)
#'
#' @export
general_heatmap <- function(ediciones,
                            regiones = "ALL",
                            usar_codigos = TRUE,
                            ordenar = c("ninguno","por_ultimo","por_promedio"),
                            paleta = c("blues","viridis","cividis","magma"),
                            anotar = FALSE,
                            mostrar_leyenda = TRUE) {

  if (missing(ediciones) || length(ediciones) < 2L || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico con al menos 2 años (p. ej., 2020:2025).")
  }
  ordenar  <- match.arg(ordenar)
  paleta   <- match.arg(paleta)
  ediciones <- sort(unique(as.integer(ediciones)))

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("forcats", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE)) {
    stop("Faltan 'dplyr', 'tidyr', 'forcats' o 'stringr'.")
  }

  # 1) Traer datos y filtrar al índice general (no Perú)
  datos <- leer_incore(
    edicion      = ediciones,
    usar_codigos = usar_codigos,
    verbose      = FALSE
  ) |>
    dplyr::filter(stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"),
                  .data$region != "Perú") |>
    dplyr::mutate(
      region  = stringr::str_squish(.data$region),
      edicion = as.integer(.data$edicion)
    )

  # 2) Filtrar regiones ———— (AQUÍ ESTABA EL BUG: usar `datos`, no `df`)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    datos <- dplyr::filter(datos, .data$region %in% regiones_filtrar)
  }

  # 3) Consolidar a UN valor por (región, edición)
  datos <- datos |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      .by   = c(.data$region, .data$edicion)
    ) |>
    dplyr::mutate(valor = round(.data$valor, 2))

  if (nrow(datos) == 0L) stop("No hay datos para graficar tras los filtros.")

  # 4) Orden de filas (regiones)
  if (ordenar == "por_ultimo") {
    ult <- max(ediciones, na.rm = TRUE)
    orden_reg <- datos |>
      dplyr::filter(.data$edicion == ult) |>
      dplyr::arrange(dplyr::desc(.data$valor), .data$region) |>
      dplyr::pull(.data$region)
  } else if (ordenar == "por_promedio") {
    orden_reg <- datos |>
      dplyr::summarise(prom = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
      dplyr::arrange(dplyr::desc(.data$prom), .data$region) |>
      dplyr::pull(.data$region)
  } else {
    orden_reg <- sort(unique(datos$region))
  }

  datos <- dplyr::mutate(
    datos,
    region  = factor(.data$region, levels = unique(orden_reg)),
    edicion = factor(.data$edicion, levels = ediciones)
  )

  # 5) Paletas continuas (0–10)
  pal_blues <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C")
  pal_virid <- c("#440154","#30678D","#35B778","#FDE725")
  pal_civid <- c("#00204D","#2C728E","#95D840","#FDE725")
  pal_magma <- c("#000004","#3B0F70","#8C2981","#DE4968","#FCA636","#FCFDBF")
  fill_scale <- switch(
    paleta,
    "blues"   = ggplot2::scale_fill_gradientn(colors = pal_blues,  limits = c(0,10), na.value = "grey90"),
    "viridis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_virid)(256), limits = c(0,10), na.value = "grey90"),
    "cividis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_civid)(256), limits = c(0,10), na.value = "grey90"),
    "magma"   = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_magma)(256), limits = c(0,10), na.value = "grey90")
  )

  # 6) Plot
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = .data$edicion, y = .data$region, fill = .data$valor)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    fill_scale +
    { if (isTRUE(anotar))
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$valor)), size = 2.9, color = "grey10")
      else NULL } +
    ggplot2::labs(
      title    = "Índice de Competitividad Regional",
      subtitle = paste0("Evolución de ", min(ediciones), " a ", max(ediciones)),
      x = "Edición", y = "Región", fill = "Puntaje",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = if (mostrar_leyenda) "right" else "none",
      panel.grid.major    = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  # Sugerencias de legibilidad
  n_reg <- length(levels(datos$region))
  if (n_reg < 5)  warning("Solo ", n_reg, " regiones: la interpretación puede ser limitada.")
  if (n_reg > 30) warning(n_reg, " regiones: considera filtrar o no anotar valores por legibilidad.")

  p
}
