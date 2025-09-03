#' Helpers internos para `bivalor_*`
#'
#' Funciones de apoyo para la familia bivariada de valores.
#' @keywords internal
#' @noRd


`%||%` <- function(x, y) if (is.null(x)) y else x


#' Validar entradas básicas para bivariados
#'
#' Chequea que exista al menos una referencia temporal (edicion o ediciones),
#' y que ambos indicadores (X e Y) estén provistos.
#'
#' @param edicion integer(1) o NULL.
#' @param ediciones integer() o NULL.
#' @param ind_x,ind_y character(1). Código o nombre de indicador.
#' @return invisible(TRUE) o error si falla.
bivalor_assert_inputs <- function(edicion = NULL,
                                  ediciones = NULL,
                                  ind_x,
                                  ind_y) {
  stop("Not implemented")
}


#' Resolver indicador (código o nombre) → lista(code, name, pilar)
#'
#' Utiliza `catalogo_indicador()` (y opcionalmente `catalogo_pilar()`)
#' para mapear un indicador a su código y nombre oficiales, y el pilar al que
#' pertenece.
#'
#' @param indicador character(1).
#' @param usar_codigos logical(1).
#' @return list(code, name, pilar_code, pilar_name).
bivalor_resolver_indicador <- function(indicador,
                                       usar_codigos = TRUE) {
  stop("Not implemented")
}


#' Traer base "valores" para un conjunto de indicadores
#'
#' Envoltorio de `leer_total()` **sin filtrar unidad** por defecto.
#' Devuelve registros para las ediciones solicitadas, indicadores pedidas
#' (por nombre), regiones y unidad (opcional).
#'
#' @param edicion,ediciones,anio Opcionales.
#' @param indicadores character(). Vector de **nombres** de indicador.
#' @param regiones "ALL" o vector (código/nombre).
#' @param unidad "ALL" (default) o texto de la columna `unidad`.
#' @param usar_codigos logical(1).
#' @param verbose logical(1).
#' @return tibble con columnas estandarizadas (region, edicion, indicador, unidad, valor, fuente, anio, pilar).
bivalor_traer_base_valores <- function(edicion = NULL,
                                       ediciones = NULL,
                                       anio = NULL,
                                       indicadores,
                                       regiones = "ALL",
                                       unidad = "ALL",
                                       usar_codigos = TRUE,
                                       verbose = FALSE) {
  stop("Not implemented")
}

#' Unificar/derivar etiquetas de unidad para dos indicadores
#'
#' Dada la unidad de X e Y, devuelve etiquetas **cortas** y **largas** para usar
#' en ejes/leyendas/subtítulos, además de una sugerencia de formato para cada eje.
#'
#' @param unidad_x,unidad_y character(1) o NA.
#' @return list(
#'   x_short, y_short, x_long, y_long,
#'   fmt_x = function(num) {...}, fmt_y = function(num) {...}
#' )
bivalor_unidades_info <- function(unidad_x, unidad_y) {
  stop("Not implemented")
}

#' Emparejar muestras X vs Y
#'
#' Empata las observaciones de X e Y (por edición y región, por defecto),
#' eliminando NA y devolviendo un tibble con columnas estandarizadas:
#' `edicion, region, valor_x, valor_y, unidad_x, unidad_y, fuente_x, fuente_y`.
#'
#' @param df_x,df_y tibbles de `bivalor_traer_base_valores()` para cada indicador.
#' @param by_vars vector de llaves (default c("edicion","region")).
#' @param join "inner" | "left" | "right" | "full".
#' @return tibble emparejado y limpio.
bivalor_match_xy <- function(df_x, df_y,
                             by_vars = c("edicion","region"),
                             join = c("inner","left","right","full")) {
  stop("Not implemented")
}

#' Paleta cualitativa para regiones (re-usa las de valor_*)
#'
#' @param n integer(1).
#' @param which "ipe","okabe_ito","viridis".
#' @return vector de colores.
bivalor_build_palette <- function(n, which = c("ipe","okabe_ito","viridis")) {
  stop("Not implemented")
}
