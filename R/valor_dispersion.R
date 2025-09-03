#' Dispersión de valores por indicador dentro de un pilar (una edición)
#'
#' Para una edición y un pilar, dibuja un diagrama de dispersión donde el eje X
#' son los CÓDIGOS de indicadores (p.ej. LAB2, EDU5, …) y cada punto es el
#' **valor original** (no puntaje 0–10) de una región en ese indicador.
#' Excluye `region == "Perú"` y el indicador `"General"`.
#'
#' Facetea por `unidad` cuando hay múltiples unidades (ejes Y libres). Si hay
#' una sola unidad, la muestra en el subtítulo.
#'
#' @param edicion integer(1)
#' @param pilar character(1) Código o nombre del pilar (p.ej. "LAB" o "Laboral")
#' @param regiones "ALL" o vector de regiones (códigos o nombres)
#' @param usar_codigos logical
#' @param paleta "ipe","okabe_ito","viridis"
#' @param jitter_width,jitter_height numeric, dispersión de puntos
#' @param mostrar_leyenda logical
#' @return ggplot
#' @export
valor_dispersion <- function(
    edicion,
    pilar,
    regiones = "ALL",
    usar_codigos = TRUE,
    paleta = c("ipe","okabe_ito","viridis"),
    jitter_width = 0.08,
    jitter_height = 0.0,
    mostrar_leyenda = TRUE
) {
  stopifnot(length(edicion) == 1, is.numeric(edicion))
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Necesitas 'ggplot2'.")

  # 1) Valida rango 2016–2025
  if (exists("valor_assert_rango_fechas", mode = "function")) {
    valor_assert_rango_fechas(edicion = edicion)
  }

  # 2) Traer base de VALORES (no fija unidad)
  df <- valor_traer_base_valores(
    edicion      = edicion,
    pilar        = pilar,
    regiones     = regiones,
    usar_codigos = usar_codigos,
    unidad       = "ALL",
    verbose      = FALSE
  )
  if (!nrow(df)) stop("No hay datos para los filtros indicados.")

  # 3) Normalización + filtros base
  df <- df |>
    dplyr::mutate(
      region    = stringr::str_squish(.data$region),
      pilar     = stringr::str_squish(.data$pilar),
      indicador = stringr::str_squish(.data$indicador),
      valor     = suppressWarnings(as.numeric(.data$valor))
    ) |>
    dplyr::filter(.data$region != "Perú", .data$indicador != "General")

  if (!nrow(df)) stop("No hay datos tras filtrar (se excluye Perú y 'General').")

  # 4) Resolver NOMBRE del pilar (si llegó código)
  pil_nom <- if (exists("valor_resolver_pilar", mode = "function")) {
    valor_resolver_pilar(pilar, usar_codigos = usar_codigos)
  } else pilar

  df <- dplyr::filter(df, .data$pilar == !!pil_nom)
  if (!nrow(df)) stop("No hay datos para el pilar '", pilar, "' en la edición ", edicion, ".")

  # 5) Obtener CÓDIGO de indicador (si no viene en la base)
  if (!"ind_cod" %in% names(df) || all(is.na(df$ind_cod))) {
    ind_dic <- try(catalogo_indicador(), silent = TRUE)
    if (!inherits(ind_dic, "try-error") && is.data.frame(ind_dic) && nrow(ind_dic)) {
      c_cod <- intersect(c("codigo","code"), names(ind_dic))[1]
      c_nom <- intersect(c("nombre","name","indicador"), names(ind_dic))[1]
      if (!is.na(c_cod) && !is.na(c_nom)) {
        ind_dic2 <- dplyr::select(ind_dic,
                                  ind_cod = dplyr::all_of(c_cod),
                                  indicador = dplyr::all_of(c_nom))
        df <- dplyr::left_join(df, ind_dic2, by = "indicador")
      }
    }
    # fallback si aún falta
    if (!"ind_cod" %in% names(df) || all(is.na(df$ind_cod))) {
      if (!requireNamespace("janitor", quietly = TRUE)) stop("Falta 'janitor' para crear códigos limpios.")
      df$ind_cod <- janitor::make_clean_names(df$indicador)
    }
  }

  # 6) Consolidar a un valor por (region, ind_cod) y capturar unidad por indicador
  df <- df |>
    dplyr::summarise(
      valor  = mean(.data$valor, na.rm = TRUE),
      unidad = dplyr::first(.data$unidad),
      .by = c(.data$region, .data$ind_cod, .data$indicador)
    )

  if (!nrow(df)) stop("No hay datos luego de consolidar por región/indicador.")

  # 7) Orden eje X por código de indicador
  df <- df |>
    dplyr::arrange(.data$ind_cod) |>
    dplyr::mutate(ind_cod = factor(.data$ind_cod, levels = unique(.data$ind_cod)))


  # --- Etiquetas X con unidad abreviada por indicador ---
  if (!exists("valor_unidad_short_label", mode = "function")) {
    stop("Falta helper 'valor_unidad_short_label()' para abreviar unidades.")
  }

  lab_map <- df |>
    dplyr::group_by(.data$ind_cod) |>
    dplyr::summarise(unid = dplyr::first(.data$unidad), .groups = "drop") |>
    dplyr::mutate(
      unid_short = valor_unidad_short_label(unid),
      lab = ifelse(
        is.na(unid_short) | !nzchar(unid_short),
        as.character(.data$ind_cod),
        paste0(as.character(.data$ind_cod), "\n(", unid_short, ")")
      )
    )

  # named vector: nombres = niveles de ind_cod, valores = etiqueta
  x_labels <- stats::setNames(lab_map$lab, as.character(lab_map$ind_cod))

  # 8) Paleta por región
  if (!exists("indc_paleta_discreta", mode = "function")) {
    stop("Falta helper 'indc_paleta_discreta()' para paletas discretas por región.")
  }
  reg_levels <- sort(unique(df$region))
  pal_vals <- indc_paleta_discreta(length(reg_levels), which = paleta)
  names(pal_vals) <- reg_levels

  # 9) ¿Una o varias unidades?
  unidades <- unique(stats::na.omit(df$unidad))
  multi_u  <- length(unidades) > 1

  sub_lab <- if (multi_u) {
    "Valores por indicador (unidades variadas)"
  } else {
    paste0("Unidad: ", if (length(unidades)) unidades[1] else "—")
  }

  # 10) Plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$ind_cod, y = .data$valor, color = .data$region)) +
    ggplot2::geom_jitter(width = jitter_width, height = jitter_height, size = 2.6, alpha = 0.9,
                         show.legend = mostrar_leyenda) +
    ggplot2::scale_color_manual(name = "Región", values = pal_vals) +
    ggplot2::scale_x_discrete(labels = x_labels) +
    ggplot2::labs(
      title    = paste0("INCORE | ", pil_nom, " (", edicion, ")"),
      subtitle = sub_lab,
      x = NULL, y = NULL,
      caption  = if (exists("valor_fuente_caption", mode = "function")) valor_fuente_caption(df) else NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = if (isTRUE(mostrar_leyenda)) "right" else "none",
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  # Facet por unidad si hay más de una (ejes Y libres)
  if (multi_u) {
    p <- p + ggplot2::facet_wrap(~ unidad, scales = "free_y")
  }

  p
}
