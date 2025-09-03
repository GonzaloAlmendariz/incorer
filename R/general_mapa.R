#' Mapa coroplético del puntaje General por región (una edición)
#'
#' Dibuja el mapa del Perú coloreando cada región según su puntaje **General** (0–10)
#' del INCORE para **una** edición. El subtítulo muestra: "Puntaje General (YYYY)".
#' Filtra **solo** los registros cuyo `pilar` comienza con
#' "Índice de Competitividad Regional".
#'
#' @param edicion Entero de longitud 1 (p. ej., 2025). **Obligatorio**.
#' @param regiones Vector de regiones a incluir (códigos o nombres). `"ALL"` = todas.
#' @param usar_codigos Si `TRUE`, traduce códigos de regiones a nombres oficiales.
#' @param mapa_sf Objeto `sf` con `region` y `geometry`. Si `NULL`, se crea con `mapa_peru()`.
#' @param paleta Paleta para relleno: `"blues"` (default), `"viridis"`, `"cividis"`.
#' @param simplificar Tolerancia de simplificación al construir `mapa_peru()` (0 = sin simplificar).
#' @param zoom Si `TRUE` y hay `regiones` selectas, se hace zoom a su *bounding box*.
#' @param expand_zoom Margen proporcional del *bbox* cuando `zoom=TRUE`. Default `0.04`.
#' @param etiquetas `"repel"` (default), `"texto"`, `"ninguna"`.
#' @return Objeto `ggplot2`.
#' @export
#'
general_mapa <- function(edicion,
                         regiones = "ALL",
                         usar_codigos = TRUE,
                         mapa_sf = NULL,
                         paleta = c("blues","greens","viridis","cividis","divergente"),
                         simplificar = 0,
                         zoom = FALSE,
                         expand_zoom = 0.04,
                         etiquetas = c("repel", "texto", "ninguna")) {
  # --- Validaciones ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser un número de longitud 1 (p. ej., 2025).")
  }
  edicion   <- as.integer(edicion)
  paleta    <- match.arg(paleta)
  etiquetas <- match.arg(etiquetas)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("sf", quietly = TRUE))       stop("Falta 'sf'.")

  # --- 1) Datos INCORE (solo índice general de esa edición) ---
  datos <- leer_incore(
    edicion      = edicion,
    usar_codigos = usar_codigos,
    verbose      = FALSE
  ) |>
    dplyr::filter(stringr::str_starts(.data$pilar, "Índice de Competitividad Regional")) |>
    dplyr::mutate(region = stringr::str_squish(.data$region)) |>
    dplyr::filter(.data$region != "Perú")

  # Filtrar regiones (grupos + exclusiones + códigos)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    datos <- dplyr::filter(datos, .data$region %in% regiones_filtrar)
  }

  # Consolidar a un valor por región
  datos <- datos |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::mutate(
      region = stringr::str_squish(.data$region),
      valor  = round(.data$valor, 2)
    )

  if (nrow(datos) == 0L) stop("No hay datos para graficar tras los filtros.")

  # --- 2) Geometría (sf) ---
  # --- Geometría (sf): usar el mapa embebido por defecto ---
  if (is.null(mapa_sf)) {
    if (exists("peru_mapa", envir = asNamespace(utils::packageName()))) {
      mapa_sf <- get("peru_mapa", envir = asNamespace(utils::packageName()))
    } else if (exists("peru_mapa", inherits = TRUE)) {
      mapa_sf <- get("peru_mapa", inherits = TRUE)
    } else {
      # Fallback: solo si realmente no existe el embebido
      if (!isTRUE(getOption("incorer.allow_download_map", FALSE))) {
        stop("No se encontró el mapa embebido 'peru_mapa'. Genera y guarda 'peru_mapa' en data/ o habilita descarga con options(incorer.allow_download_map=TRUE).")
      }
      mapa_sf <- mapa_peru(simplificar = simplificar)
    }
  }

  # --- 3) Unión datos + mapa ---
  sf_join <- dplyr::left_join(mapa_sf, datos, by = "region")

  # Si el usuario señaló regiones, dejar el resto en NA (gris)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    sf_join <- dplyr::mutate(
      sf_join,
      valor = ifelse(.data$region %in% regiones_filtrar, .data$valor, NA_real_)
    )
  }

  # --- 4) Paletas ---
  pal_blues  <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C")
  pal_greens <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#31A354","#006D2C")
  pal_virid  <- c("#440154","#30678D","#35B778","#FDE725")
  pal_civid  <- c("#00204D","#2C728E","#95D840","#FDE725")

  fill_scale <- switch(
    paleta,
    "blues"   = ggplot2::scale_fill_gradientn(colors = pal_blues,  limits = c(0,10), na.value = "grey90"),
    "greens"  = ggplot2::scale_fill_gradientn(colors = pal_greens, limits = c(0,10), na.value = "grey90"),
    "viridis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_virid)(256), limits = c(0,10), na.value = "grey90"),
    "cividis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_civid)(256), limits = c(0,10), na.value = "grey90"),
    "divergente" = ggplot2::scale_fill_gradient2(low = "#D7301F", mid = "grey85", high = "#08519C",
                                                 midpoint = 5, limits = c(0,10), na.value = "grey90")
  )

  # --- 5) Etiquetas de valor ---
  centers <- sf::st_point_on_surface(sf_join$geometry)
  coords  <- sf::st_coordinates(centers)
  lab_df  <- data.frame(
    region = sf_join$region,
    valor  = sf_join$valor,
    x      = coords[, 1],
    y      = coords[, 2]
  )

  # --- 6) Mapa ---
  p <- ggplot2::ggplot(sf_join) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$valor), color = "white", linewidth = 0.3) +
    fill_scale +
    {
      if (etiquetas == "repel" && requireNamespace("ggrepel", quietly = TRUE)) {
        list(
          ggrepel::geom_text_repel(
            data = lab_df[!is.na(lab_df$valor), ],
            ggplot2::aes(x = .data$x, y = .data$y, label = sprintf("%.2f", .data$valor)),
            size = 3, seed = 123, box.padding = 0.2, point.padding = 0.2,
            color = "grey10", min.segment.length = Inf
          ),
          ggplot2::geom_point(
            data = lab_df[!is.na(lab_df$valor), ],
            ggplot2::aes(x = .data$x, y = .data$y),
            shape = 1, size = 2, color = "grey50", inherit.aes = FALSE
          )
        )
      } else if (etiquetas == "texto") {
        ggplot2::geom_text(
          data = lab_df[!is.na(lab_df$valor), ],
          ggplot2::aes(x = .data$x, y = .data$y, label = sprintf("%.2f", .data$valor)),
          size = 3, color = "grey10"
        )
      } else NULL
    } +
    ggplot2::labs(
      title    = "Índice de Competitividad Regional",
      subtitle = paste0("Puntaje General (", edicion, ")"),
      x = NULL, y = NULL,
      fill     = "Puntaje",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    ) +
    ggplot2::coord_sf(datum = NA)

  # --- 7) Zoom opcional ---
  if (isTRUE(zoom) && !identical(regiones, "ALL")) {
    sel <- sf_join[!is.na(sf_join$valor) & !sf::st_is_empty(sf_join), ]
    if (nrow(sel) > 0) {
      bbox <- sf::st_bbox(sel$geometry)
      xlim <- c(unname(bbox["xmin"]), unname(bbox["xmax"]))
      ylim <- c(unname(bbox["ymin"]), unname(bbox["ymax"]))
      dx <- diff(xlim) * expand_zoom
      dy <- diff(ylim) * expand_zoom
      xlim <- c(xlim[1] - dx, xlim[2] + dx)
      ylim <- c(ylim[1] - dy, ylim[2] + dy)
      p <- p + ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA)
    }
  }

  p
}
