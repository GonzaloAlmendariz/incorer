#' Dispersión del puntaje General por pilar (una edición)
#'
#' Muestra, para una edición, un diagrama de dispersión donde **cada eje X es un pilar**
#' y **cada punto es el puntaje general de una región** en ese pilar (0–10).
#' Soporta filtros de regiones por grupos (p. ej. `"gr_costa"`, exclusiones con `"-Lima*"`),
#' y opcionalmente marca el **promedio nacional** por pilar.
#'
#' @param edicion integer(1). Edición a graficar.
#' @param regiones character(). Vector de regiones o grupos; `"ALL"` para todas.
#' @param pilares character(). Vector de pilares por nombre o código (o `"ALL"`).
#' @param usar_codigos logical. Si TRUE, traduce códigos de región/pilar.
#' @param paleta "ipe","okabe_ito","viridis".
#' @param mostrar_promedio logical. Si TRUE, agrega el promedio nacional por pilar.
#' @param promedio_shape, promedio_size, promedio_color, promedio_fill Estética del promedio.
#' @param jitter_width, jitter_height Dispersión para separar puntos.
#'
#' @return ggplot
#' @export
general_dispersion_pilares <- function(
    edicion,
    regiones = "ALL",
    pilares  = "ALL",
    usar_codigos = TRUE,
    paleta = c("ipe","okabe_ito","viridis"),
    mostrar_promedio = TRUE,
    promedio_shape = 23,
    promedio_size  = 3.5,
    promedio_color = "black",
    promedio_fill  = "white",
    jitter_width = 0.08,
    jitter_height = 0.0
) {
  stopifnot(length(edicion) == 1, is.numeric(edicion))
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Necesitas 'ggplot2'. Instálalo con install.packages('ggplot2').")
  }

  # 1) Traer data de la edición (solo puntajes)
  dat <- leer_incore(edicion = edicion, usar_codigos = usar_codigos, verbose = FALSE)

  # Pilares canónicos (excluye el índice agregado de portada)
  pilares_all <- c("Entorno económico","Laboral","Infraestructura","Salud","Educación","Instituciones")

  dat <- dat |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$indicador == "General",
                  .data$pilar %in% pilares_all,
                  .data$region != "Perú") |>
    dplyr::mutate(
      region = stringr::str_squish(.data$region),
      pilar  = stringr::str_squish(.data$pilar)
    )

  # 2) Filtrar pilares (acepta nombres o códigos si usar_codigos = TRUE)
  if (!identical(pilares, "ALL")) {
    pil_in <- as.character(pilares)
    if (isTRUE(usar_codigos)) {
      tr <- try(suppressWarnings(traducir_codigo(pil_in, catalogo_pilar())), silent = TRUE)
      if (!inherits(tr, "try-error")) {
        pil_in <- ifelse(is.na(tr), pil_in, tr)
      }
    }
    dat <- dplyr::filter(dat, .data$pilar %in% pil_in)
  }

  # 3) Filtrar regiones (grupos + exclusiones + códigos)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    dat <- dplyr::filter(dat, .data$region %in% regiones_filtrar)
  }

  # 4) Un valor por (region, pilar)
  dat <- dat |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      .by = c(.data$region, .data$pilar)
    ) |>
    dplyr::mutate(
      valor = round(.data$valor, 2),
      etiqueta = scales::number(.data$valor, accuracy = 0.01),
      pilar = factor(.data$pilar,
                     levels = pilares_all[pilares_all %in% unique(.data$pilar)])
    )

  if (nrow(dat) == 0) stop("Sin datos tras aplicar filtros de regiones/pilares.")

  # 5) Paleta por región
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
  reg_levels <- sort(unique(dat$region))
  pal <- build_palette(length(reg_levels), which = paleta); names(pal) <- reg_levels

  # 6) Promedio nacional por pilar (sobre regiones)
  prom <- dat |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$pilar)

  # 7) Plot
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$pilar, y = .data$valor, color = .data$region)) +
    ggplot2::geom_jitter(width = jitter_width, height = jitter_height, size = 2.6, alpha = 0.9) +
    { if (isTRUE(mostrar_promedio)) {
      ggplot2::geom_point(
        data = prom,
        mapping = ggplot2::aes(x = .data$pilar, y = .data$valor),
        inherit.aes = FALSE,
        shape = promedio_shape, size = promedio_size,
        color = promedio_color, fill = promedio_fill, stroke = 0.6
      )
    } else NULL } +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_y_continuous(limits = c(0,10), breaks = 0:10) +
    ggplot2::labs(
      title    = paste0("Índice de Competitividad Regional | Edición ", edicion),
      subtitle = "Puntaje General por pilar (0–10)",
      x = NULL, y = NULL, color = "Región",
      caption = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    )

  p
}
