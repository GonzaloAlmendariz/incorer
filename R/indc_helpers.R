# indc_helpers.R -------------------------------------------------------------

#' Paleta continua 0–10 para puntajes INCORE (helpers indc_)
#' @keywords internal
indc_paleta_continua <- function(paleta = c("blues","greens","viridis","cividis","magma","divergente"),
                                 na_value = "grey90") {
  paleta <- match.arg(paleta)
  pal_blues  <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C")
  pal_greens <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#31A354","#006D2C")
  pal_virid  <- c("#440154","#30678D","#35B778","#FDE725")
  pal_civid  <- c("#00204D","#2C728E","#95D840","#FDE725")
  pal_magma  <- c("#000004","#3B0F70","#8C2981","#DE4968","#FCA636","#FCFDBF")

  switch(
    paleta,
    "blues"   = ggplot2::scale_fill_gradientn(colors = pal_blues,  limits = c(0,10), na.value = na_value),
    "greens"  = ggplot2::scale_fill_gradientn(colors = pal_greens, limits = c(0,10), na.value = na_value),
    "viridis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_virid)(256), limits = c(0,10), na.value = na_value),
    "cividis" = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_civid)(256), limits = c(0,10), na.value = na_value),
    "magma"   = ggplot2::scale_fill_gradientn(colors = grDevices::colorRampPalette(pal_magma)(256), limits = c(0,10), na.value = na_value),
    "divergente" = ggplot2::scale_fill_gradient2(low = "#D7301F", mid = "grey85", high = "#08519C",
                                                 midpoint = 5, limits = c(0,10), na.value = na_value)
  )
}

#' Paleta cualitativa por región (helpers indc_)
#' @keywords internal
indc_paleta_discreta <- function(n, which = c("ipe","okabe_ito","viridis")) {
  which <- match.arg(which)
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
    oi <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
    if (n <= length(oi)) oi[seq_len(n)] else grDevices::colorRampPalette(oi)(n)
  } else {
    anchors <- c("#440154","#3B528B","#21918C","#5DC863","#FDE725")
    grDevices::colorRampPalette(anchors)(n)
  }
}

#' Valida rango duro de INCORE para edicion/es (2016–2025)
#' @keywords internal
indc_validar_rango <- function(edicion = NULL, ediciones = NULL) {
  miny <- 2016L; maxy <- 2025L
  if (!is.null(edicion)) {
    e <- suppressWarnings(as.integer(edicion))
    if (any(is.na(e) | e < miny | e > maxy)) {
      stop(sprintf("Argumento 'edicion' fuera de rango INCORE [%d–%d]: %s", miny, maxy, paste(edicion, collapse=", ")))
    }
  }
  if (!is.null(ediciones)) {
    es <- suppressWarnings(as.integer(ediciones))
    if (any(is.na(es) | es < miny | es > maxy)) {
      stop(sprintf("Argumento 'ediciones' fuera de rango INCORE [%d–%d]: %s", miny, maxy, paste(ediciones, collapse=", ")))
    }
  }
  invisible(TRUE)
}

#' Resolver pilar (código o nombre) a nombre oficial
#' @keywords internal
indc_resolver_pilar <- function(pilar, usar_codigos = TRUE) {
  if (is.null(pilar)) return(NULL)
  p <- stringr::str_squish(as.character(pilar)[1])
  if (isTRUE(usar_codigos)) {
    tr <- try(suppressWarnings(traducir_codigo(p, catalogo_pilar())), silent = TRUE)
    if (!inherits(tr, "try-error") && length(tr) == 1L && !is.na(tr)) return(tr)
  }
  p
}

#' Resolver indicador (código o nombre) → lista con código/nombre y pilar
#' @details Tolera catálogos con columnas `cod`, `codigo` o `code`; y `pilar_cod`, `pilar`, etc.
#' @return list(codigo, nombre, pilar_cod, pilar_nom, numero)
#' @keywords internal
indc_resolver_indicador <- function(x, dic = NULL) {
  if (is.null(dic)) dic <- try(catalogo_indicador(), silent = TRUE)
  in_str <- stringr::str_squish(as.character(x)[1])

  if (inherits(dic, "try-error") || is.null(dic) || !nrow(dic)) {
    return(list(codigo = in_str, nombre = in_str, pilar_cod = NA_character_,
                pilar_nom = NA_character_, numero = NA_real_))
  }

  # nombres flexibles
  cod_col <- c("cod","codigo","code")
  pilc_col <- c("pilar_cod","pilar_code","pilar_c")
  piln_col <- c("pilar","pilar_nom","pilar_nombre","nombre_pilar")
  num_col  <- c("numero","nro","idx")

  get_col <- function(df, opts) {
    hit <- intersect(opts, names(df))
    if (length(hit)) hit[1] else NULL
  }

  col_cod <- get_col(dic, cod_col);      if (is.null(col_cod)) col_cod <- "codigo"
  col_nom <- if ("nombre" %in% names(dic)) "nombre" else get_col(dic, c("indicador","nombre_indicador"))
  col_pc  <- get_col(dic, pilc_col)
  col_pn  <- get_col(dic, piln_col)
  col_num <- get_col(dic, num_col)

  # match por código
  by_cod <- which(toupper(dic[[col_cod]]) == toupper(in_str))
  if (length(by_cod) == 1) {
    r <- dic[by_cod, , drop = FALSE]
    return(list(
      codigo = as.character(r[[col_cod]]),
      nombre = as.character(r[[col_nom]]),
      pilar_cod = if (!is.null(col_pc)) as.character(r[[col_pc]]) else NA_character_,
      pilar_nom = if (!is.null(col_pn)) as.character(r[[col_pn]]) else NA_character_,
      numero    = if (!is.null(col_num)) suppressWarnings(as.numeric(r[[col_num]])) else NA_real_
    ))
  }

  # match por nombre
  by_nom <- which(tolower(dic[[col_nom]]) == tolower(in_str))
  if (length(by_nom) == 1) {
    r <- dic[by_nom, , drop = FALSE]
    return(list(
      codigo = as.character(r[[col_cod]]),
      nombre = as.character(r[[col_nom]]),
      pilar_cod = if (!is.null(col_pc)) as.character(r[[col_pc]]) else NA_character_,
      pilar_nom = if (!is.null(col_pn)) as.character(r[[col_pn]]) else NA_character_,
      numero    = if (!is.null(col_num)) suppressWarnings(as.numeric(r[[col_num]])) else NA_real_
    ))
  }

  # por si no encontró match exacto
  list(codigo = in_str, nombre = in_str, pilar_cod = NA_character_,
       pilar_nom = NA_character_, numero = NA_real_)
}

#' Resolver regiones (grupos + exclusiones + códigos) para indc_
#' @keywords internal
indc_resolver_regiones <- function(regiones = "ALL", usar_codigos = TRUE) {
  # Reutiliza el helper global si existe; si no, una versión mínima
  if (exists("resolver_regiones_grupos", mode = "function")) {
    return(resolver_regiones_grupos(regiones, usar_codigos = usar_codigos))
  }
  if (exists("valor_resolver_regiones", mode = "function")) {
    return(valor_resolver_regiones(regiones, usar_codigos = usar_codigos))
  }

  # Fallback muy básico (sin grupos)
  if (identical(regiones, "ALL")) return("ALL")
  v <- stringr::str_squish(as.character(regiones))
  if (isTRUE(usar_codigos) && exists("traducir_codigo", mode = "function") && exists("catalogo_region", mode = "function")) {
    tr <- try(suppressWarnings(traducir_codigo(v, catalogo_region())), silent = TRUE)
    if (!inherits(tr, "try-error")) v <- ifelse(is.na(tr), v, tr)
  }
  unique(v)
}

#' Traer base de PUNTAJES (0–10) para indicadores (excluye 'General' por defecto)
#' @keywords internal
indc_traer_base_puntajes <- function(ediciones,
                                     usar_codigos = TRUE,
                                     incluir_peru = FALSE,
                                     incluir_general = FALSE,
                                     agregar_codigos = TRUE,
                                     verbose = FALSE) {
  indc_validar_rango(ediciones = ediciones)
  base <- leer_incore(
    edicion         = sort(unique(as.integer(ediciones))),
    usar_codigos    = usar_codigos,
    agregar_codigos = agregar_codigos,
    verbose         = verbose
  ) |>
    dplyr::mutate(
      region    = stringr::str_squish(.data$region),
      pilar     = stringr::str_squish(.data$pilar),
      indicador = stringr::str_squish(.data$indicador),
      edicion   = as.integer(.data$edicion)
    ) |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10")

  if (!isTRUE(incluir_peru)) {
    base <- dplyr::filter(base, .data$region != "Perú")
  }
  if (!isTRUE(incluir_general)) {
    base <- dplyr::filter(base, .data$indicador != "General")
  }
  base
}

#' Facilidad: filtrar dataframe por regiones usando helper de grupos/códigos
#' @keywords internal
indc_filtrar_regiones <- function(df, regiones = "ALL", usar_codigos = TRUE) {
  if (identical(regiones, "ALL")) return(df)
  regs <- indc_resolver_regiones(regiones, usar_codigos = usar_codigos)
  if (!length(regs)) stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
  dplyr::filter(df, .data$region %in% regs)
}

#' Facilidad: asegurar un valor por (region, edicion, indicador)
#' @keywords internal
indc_consolidar_valor <- function(df) {
  df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     .by = c(.data$region, .data$edicion, .data$indicador)) |>
    dplyr::mutate(valor = round(.data$valor, 2))
}


#' @keywords internal
indc_normalizar_columnas <- function(df) {
  # Normaliza nombres y contenidos clave
  if (!is.data.frame(df)) stop("indc_normalizar_columnas: 'df' debe ser data.frame/tibble.")
  # Asegura columnas esperadas; si no existen, crea NA
  cols_needed <- c("region","pilar","indicador","edicion","unidad","valor",
                   "pilar_cod","ind_cod")
  for (cc in cols_needed) if (!cc %in% names(df)) df[[cc]] <- NA

  dplyr::mutate(
    df,
    region    = if ("region"    %in% names(df)) stringr::str_squish(.data$region) else NA_character_,
    pilar     = if ("pilar"     %in% names(df)) stringr::str_squish(.data$pilar) else NA_character_,
    indicador = if ("indicador" %in% names(df)) stringr::str_squish(.data$indicador) else NA_character_,
    unidad    = if ("unidad"    %in% names(df)) stringr::str_squish(.data$unidad) else NA_character_,
    edicion   = suppressWarnings(as.integer(.data$edicion)),
    valor     = suppressWarnings(as.numeric(.data$valor)),
    # si existen codigos, los dejamos; si no, quedan como NA
    pilar_cod = .data$pilar_cod,
    ind_cod   = .data$ind_cod
  )
}
#' Conjunto de nombres de pilares canónicos (nombre y código)
#' @keywords internal
indc_pilares_canonicos <- function(type = c("nombre","codigo")) {
  type <- match.arg(type)
  if (type == "nombre") {
    c("Entorno económico","Laboral","Infraestructura","Salud","Educación","Instituciones")
  } else {
    c("ECO","LAB","INF","SAL","EDU","INS")
  }
}


#' Leer base INCORE (alias interno para consistencia indc_)
#' @keywords internal
indc_leer_incore <- function(edicion,
                             usar_codigos    = TRUE,
                             agregar_codigos = TRUE,
                             verbose         = FALSE) {
  leer_incore(
    edicion         = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = agregar_codigos,
    verbose         = verbose
  )
}

