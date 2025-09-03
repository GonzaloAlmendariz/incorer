#' Registrar y resolver grupos de regiones (uso global en el paquete)
#' @name region_grupos
#' @keywords internal

# Env para guardar grupos
.incorer_region_groups_env <- new.env(parent = emptyenv())
.incorer_region_groups_env$groups <- list()

#' Definir/actualizar grupos de regiones
#' @param groups named list. Cada elemento es un vector de nombres de regiones.
#' @param replace logical(1). Si TRUE, reemplaza todo; si FALSE (default), hace merge.
#' @export
incorer_set_region_groups <- function(groups, replace = FALSE) {
  stopifnot(is.list(groups), length(names(groups)) == length(groups))
  if (isTRUE(replace)) {
    .incorer_region_groups_env$groups <- groups
  } else {
    .incorer_region_groups_env$groups <- modifyList(.incorer_region_groups_env$groups, groups)
  }
  invisible(TRUE)
}

#' Obtener los grupos de regiones definidos
#' @return named list con los grupos
#' @export
incorer_get_region_groups <- function() {
  .incorer_region_groups_env$groups
}

#' Resolver regiones (vector, grupos gr_*, exclusiones) a nombres oficiales
#'
#' Acepta:
#' - "ALL"
#' - tokens de grupo: "gr_costa", "gr_sierra", etc. (definidos vía `incorer_set_region_groups`)
#' - exclusiones con prefijo "-": p.ej. c("gr_costa","-Lima*")
#'
#' @param regiones "ALL" o character()
#' @param usar_codigos logical(1) traducir códigos a nombres (vía `catalogo_region()`)
#' @return character(): nombres oficiales de regiones (o `character(0)` si quedó vacío)
#' @export
resolver_regiones_grupos <- function(regiones = "ALL", usar_codigos = TRUE) {
  if (identical(regiones, "ALL")) return("ALL")
  regiones <- unique(as.character(regiones))

  inc_tokens <- regiones[!startsWith(regiones, "-")]
  exc_tokens <- sub("^-", "", regiones[startsWith(regiones, "-")])

  expand_groups <- function(tokens) {
    if (!length(tokens)) return(character(0))
    groups <- incorer_get_region_groups()
    out <- unlist(lapply(tokens, function(tok) {
      if (startsWith(tok, "gr_") && tok %in% names(groups)) groups[[tok]] else tok
    }), use.names = FALSE)
    unique(out)
  }

  inc_exp <- expand_groups(inc_tokens)
  exc_exp <- expand_groups(exc_tokens)

  translate_if_needed <- function(vec) {
    v <- stringr::str_squish(vec)
    if (isTRUE(usar_codigos)) {
      tr <- try(suppressWarnings(traducir_codigo(v, catalogo_region())), silent = TRUE)
      if (!inherits(tr, "try-error")) v <- ifelse(is.na(tr), v, tr)
    }
    unique(v)
  }

  inc_fin <- translate_if_needed(inc_exp)
  exc_fin <- translate_if_needed(exc_exp)

  if (length(exc_fin)) inc_fin <- setdiff(inc_fin, exc_fin)
  if (!length(inc_fin)) character(0) else inc_fin
}
