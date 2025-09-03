#' Evolución del puntaje General (líneas) por región y/o por pilar
#'
#' Grafica series de tiempo del puntaje **General** (0–10) del INCORE
#' para **dos o más ediciones**.
#' - `modo = "general"`: una línea por **región** con el índice general
#'   (filas donde `pilar` empieza con "Índice de Competitividad Regional"
#'    e `indicador == "General"`).
#' - `modo = "pilares"`: líneas por **región**, facetadas por **pilar**,
#'   usando sólo `indicador == "General"` en los 6 pilares.
#'
#' Soporta filtros de regiones por **grupos** (p. ej. `"gr_costa"`) y
#' **exclusiones** (p. ej. `"-Lima*"`), vía `resolver_regiones_grupos()`.
#'
#' @param ediciones integer(n>=2)
#' @param regiones "ALL" o vector (códigos, nombres o grupos)
#' @param usar_codigos logical
#' @param modo "general" o "pilares"
#' @param pilares (sólo modo="pilares") vector por nombre o código; por defecto los 6 pilares
#' @param paleta "ipe","okabe_ito","viridis"
#' @param mostrar_puntos logical
#' @return ggplot2
#' @export
general_largo <- function(ediciones,
                          regiones = "ALL",
                          usar_codigos = TRUE,
                          modo = c("general","pilares"),
                          pilares = NULL,
                          paleta = c("ipe","okabe_ito","viridis"),
                          mostrar_puntos = TRUE) {

  # --- dependencias ---
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("forcats", quietly = TRUE)) {
    stop("Faltan 'dplyr', 'tidyr', 'stringr', 'forcats'.")
  }

  # --- validar rango INCORE explícito (defensivo) ---
  .RANGO_INCORE_MIN <- 2016L; .RANGO_INCORE_MAX <- 2025L
  if (exists("edicion", inherits = FALSE)) {
    .ed_i <- suppressWarnings(as.integer(get("edicion", inherits = FALSE)))
    if (length(.ed_i) && any(is.na(.ed_i) | .ed_i < .RANGO_INCORE_MIN | .ed_i > .RANGO_INCORE_MAX)) {
      stop(sprintf("Argumento 'edicion' fuera de rango INCORE [%d–%d].", .RANGO_INCORE_MIN, .RANGO_INCORE_MAX))
    }
  }
  if (exists("ediciones", inherits = FALSE)) {
    .eds_i <- suppressWarnings(as.integer(get("ediciones", inherits = FALSE)))
    if (length(.eds_i) && any(is.na(.eds_i) | .eds_i < .RANGO_INCORE_MIN | .eds_i > .RANGO_INCORE_MAX)) {
      stop(sprintf("Argumento 'ediciones' fuera de rango INCORE [%d–%d].", .RANGO_INCORE_MIN, .RANGO_INCORE_MAX))
    }
  }

  # --- validar ediciones ---
  if (missing(ediciones) || length(ediciones) < 2) {
    stop("'ediciones' debe contener al menos 2 años (p.ej., 2020:2025).")
  }
  if (!is.numeric(ediciones)) stop("'ediciones' debe ser numérico.")
  ediciones <- sort(unique(as.integer(ediciones)))

  modo   <- match.arg(modo)
  paleta <- match.arg(paleta)

  # --- paletas ---
  build_palette <- function(n, which = "ipe") {
    which <- match.arg(which, c("ipe","okabe_ito","viridis"))
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

  # --- leer una vez ---
  df <- leer_incore(edicion = ediciones, usar_codigos = usar_codigos, verbose = FALSE)

  # --- filtrar regiones con grupos/exclusiones/códigos ---
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    df <- dplyr::filter(df, .data$region %in% regiones_filtrar)
  }

  # -----------------
  # MODO: GENERAL
  # -----------------
  if (modo == "general") {
    df_g <- df |>
      dplyr::filter(
        .data$indicador == "General",
        stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"),
        .data$region != "Perú"
      ) |>
      dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                       .by = c(.data$region, .data$edicion)) |>
      dplyr::arrange(.data$region, .data$edicion)

    if (nrow(df_g) == 0) stop("No hay datos 'general' para esas ediciones/regiones.")

    # orden por última edición (estable ante empates)
    ult <- max(ediciones, na.rm = TRUE)
    ult_tab <- df_g |> dplyr::filter(.data$edicion == ult) |>
      dplyr::arrange(dplyr::desc(.data$valor), .data$region)
    regiones_ord <- ult_tab$region

    df_g <- dplyr::mutate(df_g, region = factor(.data$region, levels = regiones_ord))

    pal <- build_palette(length(levels(df_g$region)), which = paleta)
    names(pal) <- levels(df_g$region)

    # avisos legibilidad
    n_reg <- length(levels(df_g$region))
    if (n_reg > 26) warning("Muchas regiones (", n_reg, "). Puede afectar la legibilidad.")

    p <- ggplot2::ggplot(df_g, ggplot2::aes(x = .data$edicion, y = .data$valor,
                                            group = .data$region, color = .data$region)) +
      ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
      { if (isTRUE(mostrar_puntos)) ggplot2::geom_point(size = 2) else NULL } +
      ggplot2::scale_color_manual(values = pal, guide = "legend") +
      ggplot2::scale_x_continuous(breaks = ediciones) +
      ggplot2::scale_y_continuous(limits = c(0, 10)) +
      ggplot2::labs(
        title    = "Índice de Competitividad Regional",
        subtitle = paste0("Evolución del ", min(ediciones), " al ", max(ediciones)),
        x = NULL, y = "Puntaje (0–10)", color = "Región",
        caption  = "Fuente: Instituto Peruano de Economía (IPE)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot",
        plot.margin = ggplot2::margin(8, 12, 8, 12)
      )

    return(p)
  }

  # -----------------
  # MODO: PILARES
  # -----------------
  pilares_validos <- c("Entorno económico","Laboral","Infraestructura",
                       "Salud","Educación","Instituciones")

  if (!is.null(pilares) && length(pilares) > 0) {
    pi_trad <- tryCatch(traducir_codigo(pilares, catalogo_pilar()),
                        error = function(e) pilares)
    pilares <- ifelse(is.na(pi_trad), pilares, pi_trad)
    pilares <- intersect(pilares_validos, pilares)
    if (!length(pilares)) {
      warning("Ningún 'pilar' solicitado coincide con los 6 pilares; usaré todos.")
      pilares <- pilares_validos
    }
  } else {
    pilares <- pilares_validos
  }

  df_p <- df |>
    dplyr::filter(
      .data$indicador == "General",
      .data$pilar %in% pilares,
      .data$region != "Perú"
    ) |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     .by = c(.data$region, .data$pilar, .data$edicion)) |>
    dplyr::arrange(.data$region, .data$pilar, .data$edicion) |>
    dplyr::mutate(
      pilar  = factor(.data$pilar, levels = pilares_validos),
      region = factor(.data$region, levels = sort(unique(.data$region)))
    )

  if (nrow(df_p) == 0) stop("No hay datos de pilares para esas ediciones/regiones.")

  pal <- build_palette(length(levels(df_p$region)), which = paleta)
  names(pal) <- levels(df_p$region)

  n_reg <- length(levels(df_p$region))
  if (n_reg > 26) warning("Muchas regiones (", n_reg, "). Puede afectar la legibilidad.")

  ggplot2::ggplot(df_p, ggplot2::aes(x = .data$edicion, y = .data$valor,
                                     group = .data$region, color = .data$region)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    { if (isTRUE(mostrar_puntos)) ggplot2::geom_point(size = 2) else NULL } +
    ggplot2::scale_color_manual(values = pal, guide = "legend") +
    ggplot2::scale_x_continuous(breaks = ediciones) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::facet_wrap(~ pilar, ncol = 3) +
    ggplot2::labs(
      title    = "Índice de Competitividad Regional",
      subtitle = paste0("Evolución del ", min(ediciones), " al ", max(ediciones)),
      x = NULL, y = "Puntaje (0–10)", color = "Región",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    )
}
