#' Telaraña (radar) por INDICADORES de un pilar (una edición)
#'
#' Dibuja un radar con los **indicadores** de un **pilar** del INCORE para
#' una **edición**. Permite comparar varias regiones; si son demasiadas, avisa por
#' legibilidad. Usa ejes en **código** (LAB2, EDU5, …) o en **nombre** completo.
#'
#' Siempre filtra a `Unidad == "Puntaje del 0 al 10"`, excluye `region == "Perú"`
#' y `indicador == "General"`. Requiere `indc_leer_incore(agregar_codigos = TRUE)`
#' para disponer de códigos.
#'
#' @param edicion integer(1). Edición (p. ej., 2025). **Obligatorio**.
#' @param pilar character(1). Código o nombre del pilar (p. ej., "LAB" o "Laboral"). **Obligatorio**.
#' @param indicadores "ALL" o vector de códigos/nombres de indicadores a incluir.
#' @param regiones "AUTO3", "ALL" o vector de regiones (códigos o nombres).
#'   `"AUTO3"` selecciona automáticamente 3 (baja, media, alta).
#' @param usar_codigos logical. Si `TRUE`, traduce códigos en filtros.
#' @param etiquetas_indicadores "codigo" (default) o "nombre" para rotular los ejes.
#' @param max_regiones entero. Umbral para advertir por legibilidad (default 6).
#' @param alpha numeric(0–1). Relleno semitransparente de los polígonos (default 0.25).
#' @param paleta "ipe", "okabe_ito" o "viridis" (paleta cualitativa para regiones).
#' @param mostrar_puntos logical. `TRUE/FALSE` para puntos en vértices.
#'
#' @return ggplot2
#' @export
indc_radar <- function(edicion,
                       pilar,
                       indicadores = "ALL",
                       regiones = "AUTO3",
                       usar_codigos = TRUE,
                       etiquetas_indicadores = c("codigo","nombre"),
                       max_regiones = 6,
                       alpha = 0.25,
                       paleta = c("ipe","okabe_ito","viridis"),
                       mostrar_puntos = TRUE) {

  # -------- Validaciones y dependencias --------
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica de longitud 1.")
  }
  if (missing(pilar) || !is.character(pilar) || length(pilar) != 1L) {
    stop("'pilar' es obligatorio (código o nombre).")
  }
  edicion <- as.integer(edicion)
  etiquetas_indicadores <- match.arg(etiquetas_indicadores)
  paleta  <- match.arg(paleta)

  req <- function(x) if (!requireNamespace(x, quietly = TRUE)) stop("Falta '", x, "'.")
  req("ggplot2"); req("dplyr"); req("tidyr"); req("stringr"); req("forcats"); req("purrr"); req("tibble")

  # rango duro
  indc_validar_rango(edicion = edicion)

  # -------- Lectura base (con códigos) --------
  df <- indc_leer_incore(
    edicion         = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  ) |>
    indc_normalizar_columnas() |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$region != "Perú",
                  .data$indicador != "General")

  # Resolver pilar (acepta código o nombre → nombre oficial)
  pilar_nom <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)
  df <- dplyr::filter(df, .data$pilar == !!pilar_nom |
                        (!is.na(.data$pilar_cod) & .data$pilar_cod == !!pilar))
  if (nrow(df) == 0L) stop("No hay datos para el pilar '", pilar, "' en ", edicion, ".")

  # Asegurar códigos/nombres de indicador (ind_cod / ind_name)
  # Si faltan, intenta mapear con catálogo; si no, deriva a partir del nombre.
  if (!"ind_cod" %in% names(df) || all(is.na(df$ind_cod))) {
    cat_ind <- try(catalogo_indicador(), silent = TRUE)
    if (!inherits(cat_ind, "try-error") && nrow(cat_ind)) {
      # tolerancia de nombres de columnas del catálogo
      cat_ind <- dplyr::rename(
        cat_ind,
        codigo = dplyr::any_of(c("codigo","code")),
        nombre = dplyr::any_of(c("nombre","name"))
      )
      df <- dplyr::left_join(df,
                             dplyr::select(cat_ind, ind_cod = .data$codigo, ind_name = .data$nombre),
                             by = c("indicador" = "ind_name"))
    }
    if (!"ind_cod" %in% names(df) || all(is.na(df$ind_cod))) {
      # derivar prefijo por pilar y número "d.d"
      pref <- dplyr::case_when(
        grepl("Laboral", pilar_nom, ignore.case = TRUE) ~ "LAB",
        grepl("Entorno", pilar_nom, ignore.case = TRUE) ~ "ECO",
        grepl("Infraestructura", pilar_nom, ignore.case = TRUE) ~ "INF",
        grepl("Salud", pilar_nom, ignore.case = TRUE) ~ "SAL",
        grepl("Educación", pilar_nom, ignore.case = TRUE) ~ "EDU",
        grepl("Instituciones", pilar_nom, ignore.case = TRUE) ~ "INS",
        TRUE ~ "IND"
      )
      num <- stringr::str_match(df$indicador, "^(\\d+\\.\\d+)")[,2]
      df$ind_cod <- ifelse(!is.na(num), paste0(pref, sub("\\.", "", num)), df$indicador)
    }
  }
  if (!"ind_name" %in% names(df) || all(is.na(df$ind_name))) {
    df$ind_name <- df$indicador
  }

  # -------- Subconjunto de indicadores (códigos o nombres) --------
  if (!identical(indicadores, "ALL")) {
    ind_in <- unique(indicadores)
    # traducir mezcla códigos/nombres → códigos usando catálogo si existe
    cat_ind2 <- try(catalogo_indicador(), silent = TRUE)
    if (!inherits(cat_ind2, "try-error") && nrow(cat_ind2)) {
      cat_ind2 <- dplyr::rename(
        cat_ind2, codigo = dplyr::any_of(c("codigo","code")),
        nombre = dplyr::any_of(c("nombre","name"))
      )
      tr_codes <- c(
        ind_in[ind_in %in% cat_ind2$codigo],
        cat_ind2$codigo[match(ind_in[ind_in %in% cat_ind2$nombre], cat_ind2$nombre)]
      )
      tr_codes <- unique(stats::na.omit(tr_codes))
      if (length(tr_codes)) ind_in <- tr_codes
    }
    df <- dplyr::filter(df, .data$ind_cod %in% ind_in | .data$ind_name %in% ind_in | .data$indicador %in% ind_in)
    if (nrow(df) == 0L) stop("No hay datos tras filtrar indicadores.")
  }

  # Consolidar (seguridad) a un valor por (region, ind_cod) y recortar 0–10
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     ind_name = dplyr::first(.data$ind_name),
                     .by = c(.data$region, .data$ind_cod)) |>
    dplyr::mutate(valor = pmax(pmin(valor, 10), 0))

  # -------- Regiones --------
  if (!identical(regiones, "ALL") && !identical(regiones, "AUTO3")) {
    df <- indc_filtrar_regiones(df, regiones = regiones, usar_codigos = usar_codigos)
    if (nrow(df) == 0L) stop("No hay datos tras filtrar regiones.")
  }
  if (identical(regiones, "AUTO3")) {
    medios <- df |>
      dplyr::summarise(m = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
      dplyr::arrange(.data$m)
    if (nrow(medios) >= 3) {
      pick <- unique(c(medios$region[1],
                       medios$region[ceiling(nrow(medios)/2)],
                       medios$region[nrow(medios)]))
      message("Selección automática de 3 regiones: ", paste(pick, collapse = ", "))
      df <- dplyr::filter(df, .data$region %in% pick)
    }
  }

  n_reg <- dplyr::n_distinct(df$region)
  if (n_reg == 0) stop("No hay datos para las regiones solicitadas.")
  if (n_reg > max_regiones) {
    warning("Estás comparando ", n_reg, " regiones. Para un radar legible, considera ≤ ", max_regiones, ".")
  }

  # -------- Geometría del radar --------
  # Orden de indicadores en el borde (por código ascendente)
  ind_levels <- df |>
    dplyr::summarise(.by = .data$ind_cod, .rows = dplyr::n()) |>
    dplyr::arrange(.data$ind_cod) |>
    dplyr::pull(.data$ind_cod)

  # Etiquetas de ejes
  if (etiquetas_indicadores == "codigo") {
    eje_lab <- stats::setNames(ind_levels, ind_levels)
  } else {
    look <- df |> dplyr::distinct(ind_cod, ind_name) |> dplyr::arrange(ind_cod)
    nm_map <- look$ind_name; names(nm_map) <- look$ind_cod
    eje_lab <- nm_map[ind_levels]
    eje_lab[is.na(eje_lab)] <- names(eje_lab)[is.na(eje_lab)]
  }

  k <- length(ind_levels)
  theta0 <- -pi/2
  angle_tbl <- tibble::tibble(
    ind_cod = factor(ind_levels, levels = ind_levels),
    angle   = theta0 + 2*pi*(seq_len(k)-1)/k
  )

  df_xy <- df |>
    dplyr::mutate(ind_cod = factor(.data$ind_cod, levels = ind_levels)) |>
    dplyr::left_join(angle_tbl, by = "ind_cod") |>
    dplyr::mutate(
      x = .data$valor * cos(.data$angle),
      y = .data$valor * sin(.data$angle)
    )

  # Polígonos por región (cerrar)
  df_poly <- df_xy |>
    dplyr::arrange(.data$region, .data$ind_cod) |>
    dplyr::group_split(.data$region, .keep = TRUE) |>
    purrr::map_dfr(function(g) {
      g <- dplyr::mutate(g, .row_id = dplyr::row_number())
      rbind(g, transform(g[1, ], .row_id = nrow(g) + 1))
    })

  # Rejilla (niveles 2,4,6,8,10)
  niveles <- c(2,4,6,8,10)
  grid_df <- lapply(niveles, function(r) {
    lvl <- tibble::tibble(ind_cod = factor(ind_levels, levels = ind_levels)) |>
      dplyr::left_join(angle_tbl, by = "ind_cod") |>
      dplyr::mutate(
        nivel = r,
        x = r * cos(.data$angle),
        y = r * sin(.data$angle)
      ) |>
      dplyr::arrange(.data$ind_cod)
    rbind(lvl, lvl[1, ])
  }) |> dplyr::bind_rows()

  # Paleta por región (usando helper)
  reg_levels <- sort(unique(df_poly$region))
  pal_cols <- indc_paleta_discreta(length(reg_levels), which = paleta); names(pal_cols) <- reg_levels

  # Ejes, etiquetas
  label_ring <- 10.7
  lab_axes <- angle_tbl |>
    dplyr::mutate(
      eje = unname(eje_lab[as.character(.data$ind_cod)]),
      x = label_ring * cos(.data$angle),
      y = label_ring * sin(.data$angle)
    )

  axes_df <- angle_tbl |>
    dplyr::mutate(
      x0 = 0, y0 = 0,
      x1 = 10 * cos(.data$angle),
      y1 = 10 * sin(.data$angle)
    )

  level_lab <- tibble::tibble(
    nivel = niveles,
    x = niveles,
    y = 0
  )

  # -------- Plot --------
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = grid_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$nivel),
      fill = NA, color = "grey80", linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      data = axes_df,
      ggplot2::aes(x = .data$x0, y = .data$y0, xend = .data$x1, yend = .data$y1),
      color = "grey85", linewidth = 0.4
    ) +
    ggplot2::geom_polygon(
      data = df_poly,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$region, fill = .data$region),
      color = NA, alpha = alpha
    ) +
    ggplot2::geom_path(
      data = df_poly,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$region, color = .data$region),
      linewidth = 0.9
    ) +
    { if (isTRUE(mostrar_puntos))
      ggplot2::geom_point(
        data = df_xy,
        ggplot2::aes(x = .data$x, y = .data$y, color = .data$region),
        size = 2
      ) else NULL } +
    ggplot2::geom_text(
      data = lab_axes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$eje),
      size = 3.4, fontface = "bold", lineheight = 0.95
    ) +
    ggplot2::geom_text(
      data = level_lab,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$nivel),
      size = 3, color = "grey40", vjust = -0.2
    ) +
    ggplot2::coord_equal(xlim = c(-11.5, 11.5), ylim = c(-11.5, 11.5), expand = FALSE) +
    ggplot2::scale_color_manual(values = pal_cols, guide = "legend") +
    ggplot2::scale_fill_manual(values = pal_cols, guide = "legend") +
    ggplot2::labs(
      title    = paste0("Índice de Competitividad Regional (", edicion, ")"),
      subtitle = paste0("Puntajes por indicador | Pilar: ", pilar_nom),
      x = NULL, y = NULL, color = "Región", fill = "Región",
      caption = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = "right",
      panel.grid          = ggplot2::element_blank(),
      axis.text           = ggplot2::element_blank(),
      axis.ticks          = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  p
}
