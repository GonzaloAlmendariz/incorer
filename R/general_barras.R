#' Barras del puntaje General (total o por pilar) por región (una edición)
#'
#' Grafica barras (coord_flip) del puntaje General (0–10) para **una** edición.
#' Puedes elegir el **general total** (por defecto) o el **general de un pilar**
#' (por ejemplo, "Salud"). Ordena de mayor a menor y colorea por región con
#' paletas profesionales (~25 regiones).
#'
#' @param edicion integer(1). Edición a graficar (p. ej., 2025).
#' @param pilar NULL para el índice general total (default), o nombre/código
#'   de un pilar para graficar su **General** (p. ej., "SAL", "Salud").
#' @param regiones Vector de regiones a incluir (códigos, nombres o grupos `gr_*`). Use `"ALL"` para todas.
#' @param usar_codigos logical. Si `TRUE`, traduce códigos a nombres oficiales.
#' @param incluir_peru logical. Si `TRUE`, incluye la fila `"Perú"`. Por defecto `FALSE`.
#' @param paleta character(1). Paleta cualitativa: `"ipe"` (default), `"okabe_ito"`, `"viridis"`.
#' @param mostrar_leyenda logical. Mostrar/ocultar leyenda (por defecto `FALSE`).
#'
#' @return Un objeto `ggplot2`.
#' @export
general_barras <- function(edicion,
                           pilar = NULL,
                           regiones = "ALL",
                           usar_codigos = TRUE,
                           incluir_peru = FALSE,
                           paleta = c("ipe", "okabe_ito", "viridis"),
                           mostrar_leyenda = FALSE) {

  # ---- Validaciones básicas + rango INCORE (helpers) ----
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser un número de longitud 1 (p. ej., 2025).")
  }
  edicion <- as.integer(edicion)
  valor_assert_rango_fechas(edicion = edicion)   # 2016–2025 duro

  paleta <- match.arg(paleta)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Necesitas 'ggplot2'. Instálalo con install.packages('ggplot2').")
  }

  # ---- Leer datos de esa edición ----
  df <- leer_incore(edicion = edicion, usar_codigos = usar_codigos, verbose = FALSE)

  # ---- Filtrar "General" total o "General" de un pilar ----
  if (is.null(pilar)) {
    # Índice general total por edición (los que comienzan con “Índice ...”)
    df <- dplyr::filter(df, stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"))
  } else {
    # Traducir código de pilar si es necesario (SAL -> Salud, etc.)
    pilar_objetivo <- pilar
    if (isTRUE(usar_codigos)) {
      posible <- try(suppressWarnings(traducir_codigo(pilar_objetivo, catalogo_pilar())), silent = TRUE)
      if (!inherits(posible, "try-error") && !is.na(posible)) pilar_objetivo <- posible
    }
    df <- dplyr::filter(df, .data$pilar == !!pilar_objetivo, .data$indicador == "General")
    if (nrow(df) == 0L) {
      stop("No hay filas para el pilar especificado. Revisa 'pilar' (nombre o código).")
    }
  }

  # ---- Excluir "Perú" si no se desea ----
  if (!isTRUE(incluir_peru)) {
    df <- dplyr::filter(df, .data$region != "Perú")
  }

  # ---- Filtrar regiones (grupos + exclusiones + códigos) ----
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    df <- dplyr::filter(df, .data$region %in% regiones_filtrar)
  }

  # ---- Consolidar a UN valor por región (seguridad) ----
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::mutate(valor = round(.data$valor, 2))

  # ---- Sin datos -> gráfico vacío informativo ----
  if (nrow(df) == 0L) {
    warning("No hay datos después de filtrar. Revisa edición, pilar y/o regiones.")
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title = if (is.null(pilar)) {
            paste0("Índice de Competitividad Regional (", edicion, ")")
          } else {
            paste0("Índice de Competitividad Regional (", edicion, ") — Pilar: ", pilar)
          },
          subtitle = "Sin datos disponibles para los filtros seleccionados",
          x = NULL, y = NULL
        ) +
        ggplot2::theme_minimal()
    )
  }

  # ---- Ordenar de mayor a menor (clave para coord_flip) ----
  df <- df |>
    dplyr::mutate(region = forcats::fct_reorder(.data$region, .data$valor))

  # ---- Paletas cualitativas (mismo estilo del paquete) ----
  build_palette <- function(n, which = "ipe") {
    which <- match.arg(which, c("ipe", "okabe_ito", "viridis"))
    if (which == "ipe") {
      base_cols <- c(
        "#355C7D","#6C5B7B","#C06C84","#F67280","#F8B195",
        "#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FEE090",
        "#FDAE61","#F46D43","#D73027","#66BD63","#1A9850",
        "#006837","#8C510A","#BF812D","#DFC27D","#80CDC1",
        "#018571","#35978F","#A6CEE3","#1F78B4","#B2DF8A","#33A02C"
      )
      if (n <= length(base_cols)) base_cols[seq_len(n)] else grDevices::colorRampPalette(base_cols)(n)
    } else if (which == "okabe_ito") {
      oi <- c("#000000","#E69F00","#56B4E9","#009E73",
              "#F0E442","#0072B2","#D55E00","#CC79A7")
      if (n <= length(oi)) oi[seq_len(n)] else grDevices::colorRampPalette(oi)(n)
    } else {
      anchors <- c("#440154","#3B528B","#21918C","#5DC863","#FDE725")
      grDevices::colorRampPalette(anchors)(n)
    }
  }
  regiones_levels <- levels(df$region)
  pal <- build_palette(length(regiones_levels), which = paleta)
  names(pal) <- regiones_levels

  # ---- Título ----
  titulo <- if (is.null(pilar)) {
    paste0("Índice de Competitividad Regional (", edicion, ")")
  } else {
    paste0("Índice de Competitividad Regional (", edicion, ") | Pilar: ", pilar)
  }

  # ---- Gráfico ----
  ggplot2::ggplot(df, ggplot2::aes(x = .data$region, y = .data$valor, fill = .data$region)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$valor)),
      hjust = -0.2, size = 3
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::expand_limits(y = max(10, max(df$valor, na.rm = TRUE) + 0.5)) +
    ggplot2::scale_fill_manual(values = pal, guide = if (mostrar_leyenda) "legend" else "none") +
    ggplot2::labs(
      title   = titulo,
      subtitle = NULL,
      x = NULL, y = "Puntaje (0–10)",
      fill = "Región",
      caption = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (mostrar_leyenda) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(10, 18, 10, 10)
    )
}
