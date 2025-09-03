#' Evolución del puntaje por indicadores de un pilar (líneas)
#'
#' Faceta por indicador y rotula cada facet según `facet_label_nombre`:
#' - TRUE  -> "d.d nombre completo"
#' - FALSE -> "EDU3" (código)
#'
#' @param ediciones integer(n>=2)
#' @param pilar character(1) (código o nombre)
#' @param indicadores "ALL" o vector de códigos/nombres
#' @param regiones "ALL" o vector de regiones
#' @param usar_codigos logical
#' @param facetear logical
#' @param facet_label_nombre logical (rotular facets con nombre completo)
#' @param paleta "ipe","okabe_ito","viridis"
#' @param mostrar_puntos logical
#' @export
indc_largo <- function(ediciones,
                       pilar,
                       indicadores = "ALL",
                       regiones = "ALL",
                       usar_codigos = TRUE,
                       facetear = TRUE,
                       facet_label_nombre = TRUE,
                       paleta = c("ipe","okabe_ito","viridis"),
                       mostrar_puntos = TRUE) {

  # --- validaciones básicas ---
  if (missing(ediciones) || length(ediciones) < 2 || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico con ≥2 años.")
  }
  if (missing(pilar) || length(pilar) != 1L) stop("'pilar' es obligatorio.")
  paleta    <- match.arg(paleta)
  ediciones <- sort(unique(as.integer(ediciones)))

  # dependencias
  req <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Falta '", p, "'.")
  req("ggplot2"); req("dplyr"); req("tidyr"); req("stringr"); req("forcats")

  # rango duro (coincide con tus helpers)
  indc_validar_rango(ediciones = ediciones)

  # --- resolver pilar (usa helper; acepta código o nombre) ---
  pilar_nom <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)

  # --- leer base (múltiples ediciones) ---
  df <- indc_leer_incore(
    edicion         = ediciones,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  ) |>
    indc_normalizar_columnas() |>
    dplyr::filter(
      .data$unidad == "Puntaje del 0 al 10",
      .data$region != "Perú",
      .data$indicador != "General",
      # filtrar por pilar por nombre (y por código si existe)
      (.data$pilar == !!pilar_nom) | (!is.na(.data$pilar_cod) & .data$pilar_cod == !!pilar)
    )

  if (nrow(df) == 0L) {
    stop("No hay datos para el pilar '", pilar, "' en las ediciones seleccionadas.")
  }

  # --- catálogo de indicadores del pilar detectado (desde la misma base) ---
  # Construimos un mapa código→nombre usando los datos filtrados
  mapa_ind <- df |>
    dplyr::filter(!is.na(.data$ind_cod)) |>
    dplyr::summarise(nombre = dplyr::first(.data$indicador), .by = .data$ind_cod)
  name_by_code <- stats::setNames(mapa_ind$nombre, mapa_ind$ind_cod)

  # --- seleccionar indicadores (ALL o subconjunto) ---
  if (identical(indicadores, "ALL")) {
    ind_set <- sort(unique(df$ind_cod))
  } else {
    # acepta códigos o nombres; normalizamos a CÓDIGOS presentes en df
    # 1) códigos presentes tal cual
    ind_cod_ok <- intersect(indicadores, unique(df$ind_cod))
    # 2) nombres → códigos (usando name_by_code inverso)
    code_by_name <- stats::setNames(names(name_by_code), unname(name_by_code))
    ind_cod_from_name <- unname(code_by_name[intersect(indicadores, names(code_by_name))])
    ind_set <- unique(c(ind_cod_ok, ind_cod_from_name))
    if (!length(ind_set)) {
      stop("Ningún indicador solicitado se encontró en el pilar '", pilar_nom, "'.")
    }
  }
  df <- dplyr::filter(df, .data$ind_cod %in% ind_set)

  # --- filtro de regiones (grupos + exclusiones + códigos) ---
  if (!identical(regiones, "ALL")) {
    regs <- indc_resolver_regiones(regiones, usar_codigos = usar_codigos)
    if (!length(regs)) stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    df <- dplyr::filter(df, .data$region %in% regs)
  }
  if (nrow(df) == 0L) stop("No hay datos tras filtrar regiones.")

  # --- consolidar seguridad y limpieza ---
  df <- df |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      .by = c(.data$region, .data$edicion, .data$ind_cod)
    ) |>
    dplyr::mutate(valor = as.numeric(.data$valor)) |>
    dplyr::filter(!is.na(.data$valor), .data$valor >= 0, .data$valor <= 10)

  # --- etiquetas de facet (código o nombre) ---
  df <- dplyr::mutate(
    df,
    facet_lab = if (facet_label_nombre) {
      # nombre completo si lo tenemos; si no, el código
      out <- name_by_code[as.character(.data$ind_cod)]
      out[is.na(out)] <- as.character(.data$ind_cod)[is.na(out)]
      out
    } else {
      as.character(.data$ind_cod)
    }
  )

  # --- ordenar regiones por la última edición (promedio sobre indicadores) ---
  ult <- max(ediciones, na.rm = TRUE)
  orden_reg <- df |>
    dplyr::filter(.data$edicion == ult) |>
    dplyr::summarise(prom = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::arrange(dplyr::desc(.data$prom)) |>
    dplyr::pull(.data$region)

  df <- dplyr::mutate(
    df,
    region    = factor(.data$region, levels = orden_reg),
    facet_lab = factor(.data$facet_lab, levels = sort(unique(.data$facet_lab)))
  )

  # --- paleta por región ---
  levs <- levels(df$region)
  pal  <- indc_paleta_discreta(length(levs), which = paleta)
  names(pal) <- levs

  # --- plot ---
  g <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$edicion, y = .data$valor, group = .data$region, color = .data$region)
  ) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    { if (isTRUE(mostrar_puntos)) ggplot2::geom_point(size = 2) else NULL } +
    ggplot2::scale_color_manual(values = pal, guide = "legend") +
    ggplot2::scale_x_continuous(breaks = ediciones) +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    { if (facetear && length(unique(df$facet_lab)) > 1) ggplot2::facet_wrap(~ facet_lab, ncol = 3) else NULL } +
    ggplot2::labs(
      title    = paste0("INCORE — Evolución por indicadores: ", pilar_nom),
      subtitle = paste0("De ", min(ediciones), " a ", max(ediciones)),
      x = NULL, y = "Puntaje (0–10)", color = "Región",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  g
}
