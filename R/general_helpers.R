#' Helpers internos para la familia general_*
#' @keywords internal
#' @noRd

# ---------------------------
# Utils básicos
# ---------------------------
.gh_require <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("Faltan paquetes: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Validación dura de rango (2016–2025)
general_assert_rango_fechas <- function(edicion = NULL,
                                        ediciones = NULL,
                                        rango = 2016:2025) {
  vals <- integer(0); tags <- character(0)
  if (!is.null(edicion))   { v <- suppressWarnings(as.integer(edicion));   vals <- c(vals, v); tags <- c(tags, rep("edicion",   length(v))) }
  if (!is.null(ediciones)) { v <- suppressWarnings(as.integer(ediciones)); vals <- c(vals, v); tags <- c(tags, rep("ediciones", length(v))) }
  if (!length(vals)) return(invisible(TRUE))
  bad <- is.na(vals) | !(vals %in% rango)
  if (any(bad)) {
    stop(
      "Fechas fuera de rango permitido (2016–2025) o no numéricas: ",
      paste(sprintf("%s=%s", tags[bad], vals[bad]), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ---------------------------
# Regiones (con grupos y exclusiones)
# ---------------------------

# Fallback interno por si no existen las funciones globales del paquete
.incorer_has_group_fns <- function() {
  exists("incorer_get_region_groups", mode = "function") &&
    exists("resolver_regiones_grupos", mode = "function")
}

# Resolver regiones con soporte a:
#  - "ALL"
#  - grupos "gr_*" definidos por el usuario (incorer_set_region_groups / incorer_get_region_groups)
#  - exclusiones con prefijo "-" (p.ej. c("gr_costa","-Lima*"))
#  - traducción de códigos a nombres cuando usar_codigos = TRUE
general_resolver_regiones <- function(regiones = "ALL",
                                      usar_codigos = TRUE) {
  if (identical(regiones, "ALL")) return("ALL")

  # Si el paquete ya expone el resolutor con grupos, úsalo.
  if (.incorer_has_group_fns()) {
    out <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    return(out)
  }

  # Fallback simple (sin grupos)
  regiones <- unique(as.character(regiones))
  inc_tokens <- regiones[!startsWith(regiones, "-")]
  exc_tokens <- sub("^-", "", regiones[startsWith(regiones, "-")])

  translate <- function(v) {
    v <- stringr::str_squish(v)
    if (isTRUE(usar_codigos)) {
      tr <- try(suppressWarnings(traducir_codigo(v, catalogo_region())), silent = TRUE)
      if (!inherits(tr, "try-error")) v <- ifelse(is.na(tr), v, tr)
    }
    unique(v)
  }

  inc <- translate(inc_tokens)
  exc <- translate(exc_tokens)
  out <- setdiff(inc, exc)
  out
}

# Atajo reutilizable dentro de cada general_* para filtrar regiones
general_filtrar_regiones <- function(df, regiones = "ALL", usar_codigos = TRUE) {
  if (identical(regiones, "ALL")) return(df)
  regs <- general_resolver_regiones(regiones, usar_codigos = usar_codigos)
  if (!length(regs)) stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.", call. = FALSE)
  dplyr::filter(df, .data$region %in% regs)
}

# ---------------------------
# Pilar y paletas
# ---------------------------

general_resolver_pilar <- function(pilar = NULL, usar_codigos = TRUE) {
  if (is.null(pilar)) return(NULL)
  p <- stringr::str_squish(as.character(pilar)[1])
  if (isTRUE(usar_codigos)) {
    tr <- try(suppressWarnings(traducir_codigo(p, catalogo_pilar())), silent = TRUE)
    if (!inherits(tr, "try-error") && length(tr) == 1L && !is.na(tr)) return(tr)
  }
  p
}

general_build_palette <- function(n, which = c("ipe","okabe_ito","viridis")) {
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
    oi <- c("#000000","#E69F00","#56B4E9","#009E73",
            "#F0E442","#0072B2","#D55E00","#CC79A7")
    if (n <= length(oi)) oi[seq_len(n)] else grDevices::colorRampPalette(oi)(n)
  } else {
    anchors <- c("#440154","#3B528B","#21918C","#5DC863","#FDE725")
    grDevices::colorRampPalette(anchors)(n)
  }
}

# ---------------------------
# Lectura estándar de puntajes
# ---------------------------

# Lee con leer_incore y deja:
# - sólo "Puntaje del 0 al 10"
# - por defecto excluye "Perú"
# - normaliza texto y tipos
general_leer_puntajes <- function(edicion = NULL,
                                  ediciones = NULL,
                                  pilar = NULL,
                                  usar_codigos = TRUE,
                                  incluir_peru = FALSE,
                                  verbose = FALSE) {
  .gh_require(c("dplyr","stringr"))

  general_assert_rango_fechas(edicion = edicion, ediciones = ediciones)

  pil_nom <- general_resolver_pilar(pilar, usar_codigos = usar_codigos)

  base <- leer_incore(
    edicion      = if (!is.null(edicion)) edicion else ediciones,
    usar_codigos = usar_codigos,
    verbose      = verbose
  )

  nm <- names(base)
  if (!"unidad"     %in% nm) base$unidad     <- NA_character_
  if (!"pilar"      %in% nm) base$pilar      <- NA_character_
  if (!"indicador"  %in% nm) base$indicador  <- NA_character_
  if (!"region"     %in% nm) base$region     <- NA_character_
  if (!"edicion"    %in% nm) base$edicion    <- NA_integer_

  base <- dplyr::mutate(
    base,
    unidad     = stringr::str_squish(.data$unidad),
    pilar      = stringr::str_squish(.data$pilar),
    indicador  = stringr::str_squish(.data$indicador),
    region     = stringr::str_squish(.data$region),
    edicion    = suppressWarnings(as.integer(.data$edicion)),
    valor      = suppressWarnings(as.numeric(.data$valor))
  )

  base <- dplyr::filter(base, .data$unidad == "Puntaje del 0 al 10")
  if (!isTRUE(incluir_peru)) base <- dplyr::filter(base, .data$region != "Perú")
  if (!is.null(pil_nom))     base <- dplyr::filter(base, .data$pilar == !!pil_nom)

  base
}

# ---------------------------
# Caption y formateo
# ---------------------------

# Para la familia general_* mantenemos caption IPE
general_caption <- function() "Fuente: Instituto Peruano de Economía (IPE)"

# Formato del puntaje (0–10) como etiqueta
general_fmt_puntaje <- function(x, accuracy = 0.01) {
  scales::number(x, accuracy = accuracy)
}
