#' Mapa coroplético del VALOR de un indicador por región (una edición)
#'
#' Colorea cada región según el **valor** (no puntaje) de un indicador
#' específico en una edición. Excluye "Perú" y todo registro cuya
#' \code{unidad} sea "Puntaje del 0 al 10".
#'
#' @param edicion integer(1). Edición a graficar (2016–2025).
#' @param indicador character(1). Código (p.ej., "EDU2"/"ECO3") o nombre del indicador.
#' @param pilar character(1). Código o nombre del pilar. Opcional; se usa para validar/cofiltrar.
#' @param regiones "ALL" o vector de regiones (códigos o nombres).
#' @param usar_codigos logical(1). Si TRUE, traduce códigos en filtros.
#' @param mapa_sf Objeto sf con columnas `region` y `geometry`. Si NULL, usa `mapa_peru()`.
#' @param paleta "viridis","cividis","magma","blues" (para escala continua).
#' @param simplificar numeric. Tolerancia para `mapa_peru()`; 0 = sin simplificar.
#' @param zoom logical. Si TRUE y hay regiones selectas, hace zoom al bounding box.
#' @param expand_zoom numeric. Margen del bbox cuando `zoom=TRUE` (default 0.04).
#' @param etiquetas "repel","texto","ninguna". Cómo rotular los valores en el mapa.
#' @param subtitulo_nombre logical(1). Si TRUE (default) muestra el **nombre completo**
#'   del indicador en el subtítulo; si FALSE, muestra el **código**.
#'
#' @return ggplot2
#' @export
valor_mapa <- function(edicion,
                       indicador,
                       pilar = NULL,
                       regiones = "ALL",
                       usar_codigos = TRUE,
                       mapa_sf = NULL,
                       paleta = c("blues", "viridis","cividis","magma"),
                       simplificar = 0,
                       zoom = FALSE,
                       expand_zoom = 0.04,
                       etiquetas = c("repel","texto","ninguna"),
                       subtitulo_nombre = TRUE) {

  # --- validaciones básicas ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica y de longitud 1 (p.ej., 2024).")
  }
  if (missing(indicador) || !is.character(indicador) || length(indicador) != 1L) {
    stop("'indicador' es obligatorio (código o nombre).")
  }
  valor_assert_rango_fechas(edicion = edicion)
  edicion   <- as.integer(edicion)
  paleta    <- match.arg(paleta)
  etiquetas <- match.arg(etiquetas)

  # deps
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("sf", quietly = TRUE))       stop("Falta 'sf'.")

  # --- resolver indicador con catálogo (acepta cod/codigo/code + nombre/name) ---
  norm_cat <- function(tbl) {
    if (is.null(tbl) || !is.data.frame(tbl)) return(NULL)
    cn <- names(tbl)
    code_col <- intersect(c("cod","codigo","code"), cn)[1]
    name_col <- intersect(c("nombre","name"), cn)[1]
    if (is.na(code_col) || is.na(name_col)) return(NULL)
    out <- tbl
    names(out)[names(out) == code_col] <- "code"
    names(out)[names(out) == name_col] <- "name"
    out$code <- toupper(stringr::str_squish(out$code))
    out$name <- stringr::str_squish(out$name)
    out
  }

  cat_ind <- try(norm_cat(catalogo_indicador()), silent = TRUE)
  if (inherits(cat_ind, "try-error") || is.null(cat_ind) ||
      !all(c("code","name") %in% names(cat_ind))) {
    stop("No se pudo leer 'catalogo_indicador()' con columnas cod/codigo/code y nombre/name.")
  }

  # Si viene un pilar, resolvemos su nombre oficial (para validar/cofiltrar)
  pilar_nom <- valor_resolver_pilar(pilar, usar_codigos = usar_codigos)

  # Resolver indicador -> (code, name) usando el catálogo (intenta por código y por nombre)
  ind_in <- stringr::str_squish(indicador)
  ind_code <- ind_name <- NA_character_
  if (toupper(gsub("\\s+","",ind_in)) %in% cat_ind$code) {
    ind_code <- toupper(gsub("\\s+","",ind_in))
    ind_name <- cat_ind$name[match(ind_code, cat_ind$code)]
  } else if (ind_in %in% cat_ind$name) {
    ind_name <- ind_in
    ind_code <- cat_ind$code[match(ind_name, cat_ind$name)]
  } else {
    stop("Indicador no reconocido en el catálogo: '", indicador, "'.")
  }

  # Si hay pilar indicado, validamos coherencia usando el prefijo del código
  if (!is.null(pilar_nom)) {
    # prefijo del código del pilar (ECO/LAB/EDU/…)
    pref_pilar <- toupper(substr(cat_ind$code[match(ind_name, cat_ind$name)], 1, 3))
    # del catálogo de pilares tomamos el code que corresponde al nombre
    cat_pil <- try(norm_cat(catalogo_pilar()), silent = TRUE)
    if (!inherits(cat_pil, "try-error") && !is.null(cat_pil)) {
      pil_row <- which(cat_pil$name == pilar_nom)
      if (length(pil_row) == 1L) {
        pref_ok <- toupper(substr(cat_pil$code[pil_row], 1, 3))
        if (!identical(pref_ok, pref_pilar)) {
          stop("El indicador '", ind_code, "' pertenece al pilar '",
               cat_pil$name[cat_pil$code == pref_pilar], "'.")
        }
      }
    }
  }

  # --- traer base (sin forzar unidad), filtrar a VALORES y una edición ---
  df <- valor_traer_base_valores(
    edicion      = edicion,
    pilar        = pilar_nom,
    indicador    = ind_name,     # los datos traen nombres en 'indicador'
    regiones     = regiones,
    unidad       = "ALL",
    usar_codigos = usar_codigos,
    verbose      = FALSE
  )

  # excluir puntaje y "Perú"
  df <- df |>
    dplyr::filter(.data$region != "Perú",
                  .data$unidad != "Puntaje del 0 al 10")

  if (nrow(df) == 0L) {
    stop("No hay valores (no-puntaje) para el indicador '", ind_code, "' en ", edicion, ".")
  }

  # por si se mezclan registros (promedio por región)
  df <- df |>
    dplyr::summarise(
      valor  = mean(.data$valor, na.rm = TRUE),
      unidad = dplyr::first(.data$unidad),
      fuente = dplyr::first(.data$fuente),
      .by    = .data$region
    )

  # --- geometría del mapa ---
  if (is.null(mapa_sf)) mapa_sf <- mapa_peru(simplificar = simplificar)
  if (inherits(mapa_sf, "SpatVector")) mapa_sf <- sf::st_as_sf(mapa_sf)
  if (!"region" %in% names(mapa_sf)) stop("'mapa_sf' debe tener columna 'region'.")
  mapa_sf <- dplyr::mutate(mapa_sf, region = stringr::str_squish(.data$region))

  sf_join <- dplyr::left_join(mapa_sf, df, by = "region")

  # --- escala continua (rango real del indicador) ---
  rng <- range(sf_join$valor, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2])) {
    stop("No hay valores numéricos para mapear.")
  }
  fill_scale <- valor_scale_fill_continua(paleta = paleta, limits = rng, na_value = "grey90")

  # --- etiquetas (centroides) ---
  centers <- sf::st_point_on_surface(sf_join$geometry)
  coords  <- sf::st_coordinates(centers)
  lab_df  <- data.frame(
    region = sf_join$region,
    valor  = sf_join$valor,
    x      = coords[,1],
    y      = coords[,2]
  )

  # --- subtítulo: indicador (nombre o código) + UNIDAD completa ---
  unidad_full <- df$unidad[which.max(!is.na(df$unidad))] %||% ""
  ind_label   <- if (isTRUE(subtitulo_nombre)) ind_name else ind_code
  subt        <- paste0("Indicador: ", ind_label, " — Unidad: ", unidad_full, " (", edicion, ")")

  # --- caption (fuentes) ---
  cap <- valor_fuente_caption(df)

  # === Derivar etiquetas para el título, subtítulo y leyenda ===

  # 1) Código y nombre del indicador (sin crear columnas en df)
  ind_res <- valor_resolver_indicador(indicador, usar_codigos = TRUE)
  ind_cod <- if (!is.null(ind_res$code)) ind_res$code else {
    # si vino por nombre, intenta mapear contra catálogo del pilar ya filtrado
    cat_ind <- catalogo_indicador()
    if (all(c("codigo","nombre") %in% names(cat_ind))) {
      cat_ind <- dplyr::rename(cat_ind, cod = .data$codigo, nom = .data$nombre)
    } else {
      cat_ind <- dplyr::rename(cat_ind, cod = .data$code, nom = .data$name)
    }
    nm <- unique(df$indicador)[1]
    cat_ind$cod[match(nm, cat_ind$nom)]
  }
  ind_name <- {
    if (!is.null(ind_res$name)) ind_res$name else unique(df$indicador)[1]
  }

  # 2) Unidad completa y unidad corta
  unidad_full <- unique(stats::na.omit(df$unidad))[1]
  unidad_short <- .vh_unidad_map(unidad_full)$unidad_short[1]

  # --- plot ---
  p <- ggplot2::ggplot(sf_join) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$valor), color = "white", linewidth = 0.3) +
    fill_scale +
    {
      if (etiquetas == "repel" && requireNamespace("ggrepel", quietly = TRUE)) {
        list(
          ggrepel::geom_text_repel(
            data = lab_df[!is.na(lab_df$valor), ],
            ggplot2::aes(x = .data$x, y = .data$y,
                         label = valor_formato_valor(.data$valor, unidad = unidad_full)),
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
          ggplot2::aes(x = .data$x, y = .data$y,
                       label = valor_formato_valor(.data$valor, unidad = unidad_full)),
          size = 3, color = "grey10"
        )
      } else NULL
    } +
    ggplot2::labs(
      title    = paste0("INCORE | ", ind_name, " (", edicion, ")"),
      subtitle = paste0("Unidad: ", unidad_full),
      x = NULL, y = NULL,
      fill     = unidad_short,
      caption  = cap  # si usas valor_fuente_caption(df); si no, deja tu caption actual
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = "right",
      panel.grid.major    = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
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
