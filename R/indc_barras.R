#' Barras de un indicador por región (una edición)
#'
#' Grafica barras (coord_flip) del **puntaje (0–10)** de un **indicador**
#' específico del INCORE para **una** edición. Ordena de mayor a menor y
#' colorea por región con paletas profesionales. Soporta:
#'   - Filtros de regiones con grupos `gr_*` y exclusiones con prefijo `-`
#'   - Códigos o nombres para pilar/indicador (vía catálogos)
#'
#' @param edicion integer(1). Año de edición a graficar (p. ej., 2025). Obligatorio.
#' @param pilar character(1). Código o nombre del pilar (p.ej. "LAB" o "Laboral"). Opcional
#'   si `indicador` es un código/nombre único.
#' @param indicador character(1). Código o nombre completo del indicador
#'   (p.ej. "LAB2" o "2.2 Formalidad laboral"). **Recomendable especificarlo**.
#' @param regiones character(). Vector de regiones o grupos; use `"ALL"` para todas.
#' @param usar_codigos logical. Si `TRUE`, traduce códigos de regiones/pilar/indicador.
#' @param incluir_peru logical. Si `TRUE`, incluye "Perú" (promedio nacional). Default `FALSE`.
#' @param paleta character(1). Paleta cualitativa: `"ipe"` (default), `"okabe_ito"`, `"viridis"`.
#' @param mostrar_leyenda logical. Mostrar/ocultar leyenda (default `FALSE`).
#'
#' @return Un objeto `ggplot2`.
#' @export
indc_barras <- function(edicion,
                        pilar = NULL,
                        indicador = NULL,
                        regiones = "ALL",
                        usar_codigos = TRUE,
                        incluir_peru = FALSE,
                        paleta = c("ipe", "okabe_ito", "viridis"),
                        mostrar_leyenda = FALSE) {

  # --- Validaciones básicas ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica y de longitud 1 (p. ej., 2025).")
  }
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Necesitas 'ggplot2'. Instálalo con install.packages('ggplot2').")
  }

  # (Rango duro por si tu base lo usa; ajusta si cambia)
  .RANGO_INCORE_MIN <- 2016L
  .RANGO_INCORE_MAX <- 2025L
  if (edicion < .RANGO_INCORE_MIN || edicion > .RANGO_INCORE_MAX) {
    stop(sprintf("'edicion' fuera de rango INCORE [%d–%d].", .RANGO_INCORE_MIN, .RANGO_INCORE_MAX))
  }

  # --- Leer data de la edición ---
  df <- leer_incore(edicion = edicion, usar_codigos = usar_codigos, verbose = FALSE)

  # Mantener SOLO puntajes 0–10 y normalizar strings clave
  df <- df |>
    dplyr::mutate(
      unidad     = if ("unidad"     %in% names(df)) .data$unidad     else NA_character_,
      pilar      = if ("pilar"      %in% names(df)) stringr::str_squish(.data$pilar) else NA_character_,
      indicador  = if ("indicador"  %in% names(df)) stringr::str_squish(.data$indicador) else NA_character_,
      region     = if ("region"     %in% names(df)) stringr::str_squish(.data$region) else NA_character_
    ) |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10")

  # --- Resolver pilar/indicador (acepta código o nombre) ---
  # 1) Resolver 'pilar' si viene como código
  if (!is.null(pilar)) {
    if (isTRUE(usar_codigos)) {
      pilar_tr <- try(suppressWarnings(traducir_codigo(pilar, catalogo_pilar())), silent = TRUE)
      if (!inherits(pilar_tr, "try-error") && length(pilar_tr) == 1L && !is.na(pilar_tr)) {
        pilar <- pilar_tr
      }
    }
  }

  # 2) Resolver 'indicador' (código o nombre); no exigimos 'pilar' si el indicador ya es único
  ind_etq <- indicador
  if (!is.null(indicador)) {
    ind_fil <- indicador
    if (isTRUE(usar_codigos)) {
      ind_tr <- try(suppressWarnings(traducir_codigo(indicador, catalogo_indicador())), silent = TRUE)
      if (!inherits(ind_tr, "try-error") && length(ind_tr) == 1L && !is.na(ind_tr)) {
        ind_fil <- ind_tr
      }
    }
    # Filtramos por indicador por nombre pleno (si traducido) o por el que vino si ya era nombre
    df <- dplyr::filter(df, .data$indicador == !!ind_fil | .data$indicador == !!indicador)
    # Guardar etiqueta limpia para el título
    un_ind <- df |> dplyr::distinct(.data$indicador) |> dplyr::pull(.data$indicador)
    if (length(un_ind) >= 1L) ind_etq <- un_ind[1]

  } else {
    # Si NO se pasa 'indicador', y hay pilar, restringimos a su pilar
    if (!is.null(pilar)) df <- dplyr::filter(df, .data$pilar == !!pilar)
    # Si aún quedan múltiples indicadores, pedimos que especifique
    unicos <- df |> dplyr::distinct(.data$indicador) |> dplyr::pull(.data$indicador)
    if (length(unicos) != 1L) {
      stop("Debes especificar 'indicador' (código o nombre). Opciones detectadas: ",
           paste(sort(unicos), collapse = "; "))
    }
    ind_etq <- unicos[1]
    df <- dplyr::filter(df, .data$indicador == !!ind_etq)
  }

  # Restringir a pilar si se pasó y no se aplicó antes
  if (!is.null(pilar)) {
    df <- dplyr::filter(df, .data$pilar == !!pilar)
    # Si al aplicar pilar nos quedamos sin filas, avisamos con mensaje claro
    if (nrow(df) == 0L) stop("El indicador solicitado no pertenece al pilar '", pilar, "'.")
  }

  # Excluir "Perú" si no se desea promedio nacional
  if (!isTRUE(incluir_peru)) df <- dplyr::filter(df, .data$region != "Perú")

  # --- Filtrar regiones (grupos + exclusiones + códigos) con tu helper ---
  if (!identical(regiones, "ALL")) {
    regs_ok <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regs_ok)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    df <- dplyr::filter(df, .data$region %in% regs_ok)
  }

  # Consolidar a UN valor por región (por seguridad) y redondear
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::mutate(valor = round(.data$valor, 2))

  # Si no hay datos, plot vacío con aviso amable
  if (nrow(df) == 0L) {
    warning("No hay datos después de filtrar. Revisa edición/pilar/indicador/regiones.")
    return(
      ggplot2::ggplot() +
        ggplot2::labs(
          title    = paste0("INCORE — ", ind_etq, " (", edicion, ")"),
          subtitle = "Sin datos disponibles para los filtros seleccionados",
          x = NULL, y = NULL
        ) +
        ggplot2::theme_minimal()
    )
  }

  # --- Ordenar de mayor a menor (clave para coord_flip) ---
  df <- df |>
    dplyr::arrange(dplyr::desc(.data$valor), .data$region) |>
    dplyr::mutate(region = factor(.data$region, levels = rev(.data$region)))
  # (usamos rev(levels) para que al hacer coord_flip queden de mayor a menor arriba)

  # --- Paletas cualitativas coherentes con general_* ---
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

  levs <- levels(df$region)
  pal  <- build_palette(length(levs), which = paleta); names(pal) <- levs

  # --- Títulos limpios ---
  titulo    <- paste0("INCORE — ", ind_etq, " (", edicion, ")")
  subtitulo <- NULL

  # --- Gráfico ---
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$region, y = .data$valor, fill = .data$region)
  ) +
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
      title    = titulo,
      subtitle = subtitulo,
      x = NULL, y = "Puntaje (0–10)",
      fill     = "Región",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = if (mostrar_leyenda) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin      = ggplot2::margin(10, 18, 10, 10)
    )

  p
}
