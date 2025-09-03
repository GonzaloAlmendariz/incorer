#' Telaraña (radar) del puntaje General por pilares (una edición)
#'
#' Dibuja un radar regular (hexágono) con los 6 pilares del INCORE para **una edición**.
#' Permite comparar varias regiones (mejor 3–6). Si `regiones = "AUTO3"` (default),
#' se eligen 3 regiones representativas (promedio de pilares más bajo, medio y alto).
#'
#' Internamente usa `ind_tabla(ediciones = edicion, modo = "pilares", largo = TRUE)`,
#' y convierte a coordenadas (x, y) para lados rectos (no arcos).
#'
#' @param edicion integer(1). Edición (p. ej., 2025).
#' @param regiones Vector de regiones (códigos o nombres).
#'   Use `"AUTO3"` para la selección automática bajo/medio/alto; `NULL` muestra todas.
#' @param usar_codigos logical. Si TRUE, traduce códigos de región antes de filtrar.
#' @param max_regiones Máximo recomendado antes de advertir (por defecto 6).
#' @param alpha Relleno de polígonos (0–1). Default 0.25.
#' @param paleta "ipe","okabe_ito","viridis".
#' @param mostrar_puntos logical. Puntos en los vértices.
#' @param etiquetas_pilares "corto" o "largo".
#'
#' @return ggplot
#' @export
general_radar <- function(edicion,
                          regiones = "AUTO3",
                          usar_codigos = TRUE,
                          max_regiones = 6,
                          alpha = 0.25,
                          paleta = c("ipe","okabe_ito","viridis"),
                          mostrar_puntos = TRUE,
                          etiquetas_pilares = c("corto","largo")) {

  # Dependencias
  req <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Falta '", p, "'.")
  req("ggplot2"); req("dplyr"); req("tidyr"); req("stringr"); req("forcats"); req("purrr"); req("tibble")

  stopifnot(length(edicion) == 1, is.numeric(edicion))
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)
  etiquetas_pilares <- match.arg(etiquetas_pilares)

  # 1) Traer datos de pilares (formato largo)
  df_long <- ind_tabla(
    ediciones    = edicion,
    modo         = "pilares",
    largo        = TRUE,
    regiones     = if (is.null(regiones) || identical(regiones, "AUTO3")) "ALL" else regiones,
    usar_codigos = usar_codigos,
    gt           = FALSE,
    verbose      = FALSE
  )

  # Orden canónico de pilares
  orden_pilares <- c("Entorno económico","Laboral","Infraestructura","Salud","Educación","Instituciones")
  df_long <- dplyr::mutate(df_long, pilar = factor(.data$pilar, levels = orden_pilares))

  # 2) Selección de regiones
  if (is.null(regiones) || identical(regiones, "AUTO3")) {
    medios <- df_long |>
      dplyr::summarise(m = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
      dplyr::arrange(.data$m)

    if (nrow(medios) >= 3) {
      pick <- unique(c(
        medios$region[1],
        medios$region[ceiling(nrow(medios) / 2)],
        medios$region[nrow(medios)]
      ))
      message("Selección automática de 3 regiones (AUTO3): ", paste(pick, collapse = ", "))
      df_long <- dplyr::filter(df_long, .data$region %in% pick)
    } else {
      stop("No hay suficientes regiones para seleccionar AUTO3.")
    }
  } else {
    regs_ok <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regs_ok)) stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    df_long <- dplyr::filter(df_long, .data$region %in% regs_ok)
  }

  # Chequeo de legibilidad
  n_reg <- dplyr::n_distinct(df_long$region)
  if (n_reg == 0) stop("No hay datos para las regiones solicitadas.")
  if (n_reg > max_regiones) {
    warning("Estás comparando ", n_reg, " regiones. Para un radar legible, considera ≤ ",
            max_regiones, " (ideal 3–4).")
  }

  # 3) Etiquetas de ejes
  label_corto <- c(
    "Entorno económico" = "Entorno\neconómico",
    "Laboral"           = "Laboral",
    "Infraestructura"   = "Infraestructura",
    "Salud"             = "Salud",
    "Educación"         = "Educación",
    "Instituciones"     = "Instituciones"
  )
  label_largo <- c(
    "Entorno económico" = "Entorno económico",
    "Laboral"           = "Laboral",
    "Infraestructura"   = "Infraestructura",
    "Salud"             = "Salud",
    "Educación"         = "Educación",
    "Instituciones"     = "Instituciones"
  )
  eje_lab <- if (etiquetas_pilares == "corto") label_corto else label_largo

  # 4) Geometría (x, y)
  k <- length(orden_pilares)
  theta0 <- -pi/2
  angle_tbl <- tibble::tibble(
    pilar = factor(orden_pilares, levels = orden_pilares),
    angle = theta0 + 2*pi*(seq_len(k)-1)/k
  )

  df_xy <- df_long |>
    dplyr::left_join(angle_tbl, by = "pilar") |>
    dplyr::mutate(
      x = .data$valor * cos(.data$angle),
      y = .data$valor * sin(.data$angle)
    )

  # Polígonos cerrados por región
  df_poly <- df_xy |>
    dplyr::arrange(.data$region, .data$pilar) |>
    dplyr::group_split(.data$region, .keep = TRUE) |>
    purrr::map_dfr(function(g) {
      g <- dplyr::mutate(g, .row_id = dplyr::row_number())
      rbind(g, transform(g[1, ], .row_id = nrow(g) + 1))
    })

  # Rejilla (2,4,6,8,10)
  niveles <- c(2,4,6,8,10)
  grid_df <- lapply(niveles, function(r) {
    lvl <- tibble::tibble(pilar = factor(orden_pilares, levels = orden_pilares)) |>
      dplyr::left_join(angle_tbl, by = "pilar") |>
      dplyr::mutate(
        nivel = r,
        x = r * cos(.data$angle),
        y = r * sin(.data$angle)
      ) |>
      dplyr::arrange(.data$pilar)
    rbind(lvl, lvl[1, ])
  }) |> dplyr::bind_rows()

  # Paleta
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
  regiones_ord <- sort(unique(df_poly$region))
  pal <- build_palette(length(regiones_ord), which = paleta)
  names(pal) <- regiones_ord

  # Ejes, etiquetas y niveles
  label_ring <- 10.5
  lab_axes <- angle_tbl |>
    dplyr::mutate(
      eje = eje_lab[as.character(.data$pilar)],
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

  # Plot
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
    {
      if (isTRUE(mostrar_puntos)) {
        ggplot2::geom_point(
          data = df_xy,
          ggplot2::aes(x = .data$x, y = .data$y, color = .data$region),
          size = 2
        )
      } else NULL
    } +
    ggplot2::geom_text(
      data = lab_axes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$eje),
      size = 3.6, fontface = "bold", lineheight = 0.95
    ) +
    ggplot2::geom_text(
      data = level_lab,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$nivel),
      size = 3, color = "grey40", vjust = -0.2
    ) +
    ggplot2::coord_equal(xlim = c(-11.2, 11.2), ylim = c(-11.2, 11.2), expand = FALSE) +
    ggplot2::scale_color_manual(values = pal, guide = "legend") +
    ggplot2::scale_fill_manual(values = pal, guide = "legend") +
    ggplot2::labs(
      title    = paste0("Índice de Competitividad Regional (", edicion, ")"),
      subtitle = "Puntajes por pilar (0–10)",
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
