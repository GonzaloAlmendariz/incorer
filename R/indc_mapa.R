#' Mapa coroplético del puntaje de un INDICADOR por región (una edición)
#'
#' Colorea cada región según el puntaje (0–10) de un **indicador** (p.ej. "EDU2")
#' en **una** edición. Usa `indc_leer_incore(..., agregar_codigos = TRUE)`,
#' excluye `region == "Perú"` e `indicador == "General"`.
#'
#' @param edicion integer(1)
#' @param indicador character(1) Código (p.ej. "EDU2") o nombre completo
#' @param pilar character(1) Opcional (si se pasa, se valida coherencia)
#' @param regiones "ALL" o vector (códigos/nombres o grupos `gr_*`, admite exclusiones `-`)
#' @param usar_codigos logical (traduce en filtros de regiones/pilar)
#' @param mapa_sf sf con `region` y `geometry` (si NULL, `mapa_peru()`)
#' @param paleta "blues","greens","viridis","cividis","magma","divergente"
#' @param simplificar numeric, tolerancia para `mapa_peru()`
#' @param zoom logical
#' @param expand_zoom numeric
#' @param etiquetas "repel","texto","ninguna"
#' @param subtitulo_nombre logical (TRUE: usa “d.d nombre” si aplica; FALSE: usa código)
#' @return ggplot2
#' @export
indc_mapa <- function(edicion,
                      indicador,
                      pilar = NULL,
                      regiones = "ALL",
                      usar_codigos = TRUE,
                      mapa_sf = NULL,
                      paleta = c("blues","greens","viridis","cividis","magma","divergente"),
                      simplificar = 0,
                      zoom = FALSE,
                      expand_zoom = 0.04,
                      etiquetas = c("repel","texto","ninguna"),
                      subtitulo_nombre = TRUE) {

  # --- validaciones y args ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica (p.ej., 2024).")
  }
  if (missing(indicador) || length(indicador) != 1L) {
    stop("'indicador' es obligatorio (código o nombre).")
  }
  indc_validar_rango(edicion = edicion)
  edicion   <- as.integer(edicion)
  paleta    <- match.arg(paleta)
  etiquetas <- match.arg(etiquetas)

  req <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Falta '", p, "'.")
  req("ggplot2"); req("dplyr"); req("stringr"); req("sf")

  # --- resolver indicador (código ↔ nombre, y pilar al que pertenece) ---
  ind <- indc_resolver_indicador(indicador)  # list: codigo, nombre, pilar_cod, pilar_nom, numero

  # Si el usuario pasó pilar, validar coherencia (acepta código o nombre)
  if (!is.null(pilar) && !is.na(ind$pilar_nom)) {
    pilar_req <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)
    if (!identical(pilar_req, ind$pilar_nom)) {
      stop("El indicador '", ind$codigo, "' pertenece al pilar '", ind$pilar_nom,
           "', no a '", pilar, "'.")
    }
  }

  # --- leer base (UNA edición) con códigos agregados ---
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

  # Filtrar por indicador (por código preferido si existe, si no por nombre)
  if ("ind_cod" %in% names(df) && !is.na(ind$codigo) && ind$codigo %in% df$ind_cod) {
    df <- dplyr::filter(df, .data$ind_cod == !!ind$codigo)
  } else {
    df <- dplyr::filter(df, .data$indicador == !!ind$nombre)
  }
  if (!is.na(ind$pilar_nom)) {
    df <- dplyr::filter(df, .data$pilar == !!ind$pilar_nom)
  }
  if (nrow(df) == 0L) stop("No hay datos para el indicador '", ind$codigo, "' en ", edicion, ".")

  # --- filtro de regiones (grupos/códigos/exclusiones) ---
  df <- indc_filtrar_regiones(df, regiones = regiones, usar_codigos = usar_codigos)
  if (nrow(df) == 0L) stop("No hay datos tras filtrar regiones.")

  # --- consolidar a UN valor por región ---
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::mutate(valor = round(.data$valor, 2))

  # --- geometría sf ---
  if (is.null(mapa_sf)) mapa_sf <- mapa_peru(simplificar = simplificar)
  if (inherits(mapa_sf, "SpatVector")) mapa_sf <- sf::st_as_sf(mapa_sf)
  if (!"region" %in% names(mapa_sf)) stop("'mapa_sf' debe tener columna 'region'.")
  mapa_sf <- dplyr::mutate(mapa_sf, region = stringr::str_squish(.data$region))
  sf_join <- dplyr::left_join(mapa_sf, df, by = "region")

  # --- etiquetas en centroides ---
  centers <- sf::st_point_on_surface(sf_join$geometry)
  coords  <- sf::st_coordinates(centers)
  lab_df  <- data.frame(
    region = sf_join$region,
    valor  = sf_join$valor,
    x = coords[,1],
    y = coords[,2]
  )

  # --- subtítulo bonito ---
  subt <- if (isTRUE(subtitulo_nombre)) {
    nm <- if (!is.null(ind$numero) && !is.na(ind$numero)) ind$nombre else ind$nombre
    paste0("Indicador: ", nm, " (", edicion, ")")
  } else {
    paste0("Indicador: ", ind$codigo, " (", edicion, ")")
  }

  # --- plot ---
  p <- ggplot2::ggplot(sf_join) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$valor), color = "white", linewidth = 0.3) +
    indc_paleta_continua(paleta = paleta, na_value = "grey90") +
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
      title    = paste0("Índice de Competitividad Regional"),
      subtitle = subt,
      x = NULL, y = NULL,
      fill     = "Puntaje (0–10)",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    ) +
    ggplot2::coord_sf(datum = NA)

  # zoom opcional
  if (isTRUE(zoom) && !identical(regiones, "ALL")) {
    sel <- sf_join[!is.na(sf_join$valor), ]
    if (nrow(sel) > 0) {
      bb <- sf::st_bbox(sel$geometry)
      dx <- (bb["xmax"] - bb["xmin"]) * expand_zoom
      dy <- (bb["ymax"] - bb["ymin"]) * expand_zoom
      p <- p + ggplot2::coord_sf(
        xlim = c(bb["xmin"] - dx, bb["xmax"] + dx),
        ylim = c(bb["ymin"] - dy, bb["ymax"] + dy),
        expand = FALSE, datum = NA
      )
    }
  }

  p
}
