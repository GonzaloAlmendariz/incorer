#' Valores por indicador (líneas en varias ediciones)
#'
#' Grafica la evolución de **valores** (no puntajes) por indicador dentro
#' de un pilar para varias ediciones. Acepta indicadores por **código**
#' (p.ej. "ECO3") o por **nombre**; además, nombres que comienzan con
#' numeración tipo "3" o "3.1 ..." se reducen al código del pilar
#' (p.ej., dentro de ECO → "ECO3").
#'
#' @param ediciones integer(n>=2).
#' @param pilar character(1). Código o nombre (p.ej., "ECO" o "Entorno económico").
#' @param indicadores "ALL" o vector de códigos/nombres (se admiten "3", "3.1 ...").
#' @param regiones "ALL" o vector de regiones (códigos o nombres).
#' @param usar_codigos logical.
#' @param facetear logical.
#' @param paleta "ipe","okabe_ito","viridis".
#' @param mostrar_puntos logical.
#' @param label_indicador "codigo" (default) o "nombre".
#'
#' @return ggplot
#' @export
valor_largo <- function(ediciones,
                        pilar,
                        indicadores = "ALL",
                        regiones = "ALL",
                        usar_codigos = TRUE,
                        facetear = TRUE,
                        paleta = c("ipe","okabe_ito","viridis"),
                        mostrar_puntos = TRUE,
                        label_indicador = c("codigo","nombre")) {

  # ---- validaciones básicas ----
  if (missing(ediciones) || length(ediciones) < 2 || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico y contener al menos 2 años.")
  }
  if (missing(pilar) || !is.character(pilar) || length(pilar) != 1) {
    stop("'pilar' es obligatorio (código o nombre).")
  }
  valor_assert_rango_fechas(ediciones = ediciones)
  paleta          <- match.arg(paleta)
  label_indicador <- match.arg(label_indicador)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("forcats", quietly = TRUE)) {
    stop("Necesitas 'dplyr', 'tidyr', 'stringr' y 'forcats'.")
  }

  # ---- traer base completa (NO filtra por unidad) ----
  df <- valor_traer_base_valores(
    ediciones   = ediciones,
    pilar       = pilar,
    regiones    = regiones,
    unidad      = "ALL",
    usar_codigos = usar_codigos,
    verbose     = FALSE
  )

  if (nrow(df) == 0) stop("No hay registros para ese pilar/ediciones/regiones.")

  # quedarnos SOLO con NO–PUNTAJE
  df <- dplyr::filter(df, .data$unidad != "Puntaje del 0 al 10")
  if (nrow(df) == 0) {
    stop("No hay valores (no-puntaje) para esos filtros.")
  }

  # ---- catálogo de indicadores para mapear code<->name ----
  cat_ind <- try(catalogo_indicador(), silent = TRUE)
  if (inherits(cat_ind, "try-error") || !is.data.frame(cat_ind)) {
    stop("No se pudo leer 'catalogo_indicador()'.")
  }
  # normalizar columnas posibles
  if (all(c("codigo","nombre") %in% names(cat_ind))) {
    cat_ind <- dplyr::rename(cat_ind, code = .data$codigo, name = .data$nombre)
  }
  if (!all(c("code","name") %in% names(cat_ind))) {
    stop("El catálogo de indicadores debe tener columnas 'code' y 'name' (o 'codigo'/'nombre').")
  }
  cat_ind$code <- stringr::str_squish(toupper(cat_ind$code))
  cat_ind$name <- stringr::str_squish(cat_ind$name)

  # detectar prefijo del pilar (ECO/LAB/EDU/...)
  pilar_code <- {
    cat_pil <- try(catalogo_pilar(), silent = TRUE)
    if (!inherits(cat_pil, "try-error") && is.data.frame(cat_pil)) {
      if (all(c("codigo","nombre") %in% names(cat_pil))) {
        cat_pil <- dplyr::rename(cat_pil, code = .data$codigo, name = .data$nombre)
      }
      pc <- NULL
      if (pilar %in% cat_pil$code) pc <- pilar
      if (is.null(pc) && pilar %in% cat_pil$name) pc <- cat_pil$code[match(pilar, cat_pil$name)]
      if (!is.null(pc)) toupper(stringr::str_sub(pc, 1, 3)) else toupper(stringr::str_sub(pilar, 1, 3))
    } else {
      toupper(stringr::str_sub(pilar, 1, 3))
    }
  }

  # subset catálogo a este pilar (si el código empieza con el prefijo)
  cat_p <- dplyr::filter(cat_ind, stringr::str_starts(.data$code, paste0("^", pilar_code)))

  # ---- resolver 'indicadores' solicitado ----
  # admite: "ALL", códigos (ECO3), nombres completos, o "3", "3.1 ...."
  wanted_codes <- NULL
  not_found    <- character(0)

  if (!identical(indicadores, "ALL")) {
    raw <- unique(as.character(indicadores))
    raw <- stringr::str_squish(raw)

    map_one <- function(z) {
      Z <- toupper(gsub("\\s+", "", z))
      # 1) si ya es código del pilar (ECO3) y existe
      if (Z %in% cat_p$code) return(Z)
      # 2) si empieza por dígitos (ej. "3" o "3.1 ..." o "03")
      m <- stringr::str_match(z, "^\\s*(\\d{1,2})")
      if (!is.na(m[1,2])) {
        cand <- paste0(pilar_code, as.integer(m[1,2]))
        if (cand %in% cat_p$code) return(cand)
      }
      # 3) nombre completo
      nm <- stringr::str_squish(z)
      if (nm %in% cat_p$name) return(cat_p$code[match(nm, cat_p$name)])
      # no mapeó
      NA_character_
    }

    wanted_codes <- vapply(raw, map_one, character(1))
    not_found    <- raw[is.na(wanted_codes)]
    wanted_codes <- stats::na.omit(wanted_codes)
    wanted_codes <- unique(wanted_codes)

    if (length(not_found)) {
      warning("Indicadores no reconocidos y serán omitidos: ",
              paste(not_found, collapse = ", "))
    }
    if (!length(wanted_codes)) {
      stop("Ningún indicador reconocido para ese pilar.")
    }
  } else {
    wanted_codes <- cat_p$code
  }

  # limitar al catálogo reconocido Y a lo que realmente está en los datos
  # (en df los indicadores están por NOMBRE)
  df$indicador <- stringr::str_squish(df$indicador)
  code_to_name <- stats::setNames(cat_p$name, cat_p$code)
  name_to_code <- stats::setNames(cat_p$code, cat_p$name)

  have_names <- intersect(unique(df$indicador), cat_p$name)
  have_codes <- unique(name_to_code[have_names])
  keep_codes <- intersect(wanted_codes, have_codes)

  if (!length(keep_codes)) {
    stop("No hay valores (no-puntaje) para esos filtros.")
  }
  df <- dplyr::filter(df, .data$indicador %in% code_to_name[keep_codes])

  # ---- etiqueta por panel: código o nombre + unidad corta ----
  uinfo <- .vh_unidad_map(df$unidad)
  df <- dplyr::bind_cols(df, uinfo[, c("unidad_short","unidad_tipo")])

  df$ind_cod <- name_to_code[df$indicador]
  df$ind_name <- df$indicador

  if (label_indicador == "codigo") {
    df$panel_lab <- paste0(df$ind_cod, " — ", df$unidad_short)
  } else {
    # nombre compacto; si es muy largo, truncar un poco
    nm <- stringr::str_trunc(df$ind_name, 55)
    df$panel_lab <- paste0(nm, " — ", df$unidad_short)
  }

  # limpiar y ordenar
  df <- df |>
    dplyr::filter(!is.na(.data$valor)) |>
    dplyr::mutate(
      edicion = as.integer(.data$edicion),
      region  = stringr::str_squish(.data$region)
    )

  if (nrow(df) == 0) stop("No hay datos tras limpiar NA.")

  # ordenar regiones por valor en la última edición (promedio por indicador)
  ult <- max(ediciones, na.rm = TRUE)
  orden_reg <- df |>
    dplyr::filter(.data$edicion == ult) |>
    dplyr::group_by(.data$region) |>
    dplyr::summarise(m = mean(.data$valor, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$m)) |>
    dplyr::pull(.data$region)

  df <- dplyr::mutate(
    df,
    region   = factor(.data$region, levels = orden_reg),
    panel_lab = factor(.data$panel_lab,
                       levels = unique(df$panel_lab[order(df$ind_cod)]))
  )

  # paleta regiones
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
  pal <- build_palette(length(levels(df$region)), which = paleta)
  names(pal) <- levels(df$region)

  # caption (fuentes únicas presentes en df)
  cap <- valor_fuente_caption(df)


  # --- Subtítulo dinámico: si hay 1 indicador, mostrar la UNIDAD completa ---
  n_indicadores <- length(unique(df$ind_cod))
  if (n_indicadores == 1L) {
    u_vals <- unique(df$unidad)
    u_vals <- u_vals[!is.na(u_vals) & u_vals != ""]
    subt_text <- if (length(u_vals)) {
      paste0("Unidad: ", u_vals[1])
    } else {
      "Unidad: (no especificada)"
    }
  } else {
    subt_text <- paste0("Del ", min(ediciones), " al ", max(ediciones))
  }

  # ---- plot ----
  g <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$edicion, y = .data$valor, group = .data$region, color = .data$region)
  ) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    { if (isTRUE(mostrar_puntos)) ggplot2::geom_point(size = 2) else NULL } +
    ggplot2::scale_color_manual(values = pal, guide = "legend") +
    ggplot2::scale_x_continuous(breaks = sort(unique(df$edicion))) +
    ggplot2::labs(
      title    = paste0("INCORE | Indicador: ", valor_resolver_pilar(pilar, usar_codigos = TRUE)),
      subtitle = subt_text,
      x = NULL, y = NULL, color = "Región",
      caption  = cap
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin       = ggplot2::margin(8, 12, 8, 12)
    )

  if (facetear && length(levels(df$panel_lab)) > 1) {
    g <- g + ggplot2::facet_wrap(~ panel_lab, ncol = 3, scales = "free_y")
  }

  # mensajes de legibilidad
  if (length(levels(df$region)) > 26) {
    warning("Muchas regiones (", length(levels(df$region)), "). Puede afectar legibilidad.")
  }

  g
}
