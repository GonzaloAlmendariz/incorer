#' Tabla del puntaje General por región (ediciones en columnas)
#'
#' Construye una tabla del puntaje **General** (0–10) del INCORE tomando
#' únicamente las filas cuyo `pilar` empieza con "Índice de Competitividad Regional".
#' Las filas son **regiones** (orden alfabético) y las columnas son las **ediciones**
#' con nombres tipo `puntaje_2025`. Si `gt = TRUE`, devuelve una tabla 'gt' con
#' encabezados "Puntaje 2025", etc.
#'
#' @param ediciones Vector numérico de una o más ediciones (p. ej. 2025 o 2019:2025).
#' @param regiones Vector de regiones a incluir (códigos o nombres) o "ALL" (por defecto).
#' @param usar_codigos Si \code{TRUE}, traduce códigos de regiones a nombres oficiales.
#' @param incluir_peru Incluir la fila "Perú" (promedio nacional). Por defecto \code{FALSE}.
#' @param gt Si \code{TRUE}, devuelve un objeto \code{gt}; en caso contrario, un \code{tibble}.
#'
#' @return \code{tibble} o \code{gt} (si \code{gt = TRUE}).
#' @export
general_tabla <- function(ediciones,
                          regiones = "ALL",
                          usar_codigos = TRUE,
                          incluir_peru = FALSE,
                          gt = FALSE) {

  # --- Validaciones básicas ---
  if (missing(ediciones) || length(ediciones) < 1L || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico (p. ej., 2025 o 2019:2025).")
  }
  ediciones <- sort(unique(as.integer(ediciones)))

  # Rango INCORE defensivo (opcional)
  .RANGO_INCORE_MIN <- 2016L
  .RANGO_INCORE_MAX <- 2025L
  if (any(ediciones < .RANGO_INCORE_MIN | ediciones > .RANGO_INCORE_MAX)) {
    stop(sprintf("'ediciones' fuera de rango INCORE [%d–%d].",
                 .RANGO_INCORE_MIN, .RANGO_INCORE_MAX))
  }

  # --- Leer data base ---
  datos <- leer_incore(
    edicion      = ediciones,
    usar_codigos = usar_codigos,
    verbose      = FALSE
  )

  # Sólo índice general “de portada”
  datos <- datos |>
    dplyr::filter(stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"))

  # Incluir/Excluir Perú
  if (!isTRUE(incluir_peru)) {
    datos <- dplyr::filter(datos, .data$region != "Perú")
  }

  # --- Filtrar regiones con helpers (grupos + exclusiones + códigos) ---
  if (!identical(regiones, "ALL")) {
    regs_ok <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regs_ok)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    datos <- dplyr::filter(datos, .data$region %in% regs_ok)
  }

  # --- Preparar y pivotear ---
  datos <- datos |>
    dplyr::transmute(
      region  = stringr::str_squish(.data$region),
      edicion = as.integer(.data$edicion),
      valor   = round(.data$valor, 2)
    ) |>
    dplyr::arrange(.data$region, .data$edicion)

  if (nrow(datos) == 0L) stop("No hay datos para las combinaciones solicitadas.")

  tabla_wide <- tidyr::pivot_wider(
    datos,
    id_cols      = .data$region,
    names_from   = .data$edicion,
    values_from  = .data$valor,
    names_prefix = "puntaje_",
    values_fn    = list(valor = mean)  # por si aparecen duplicados esporádicos
  ) |>
    dplyr::arrange(.data$region)

  # Reordenar columnas según 'ediciones'
  punt_cols <- paste0("puntaje_", ediciones)
  punt_cols <- punt_cols[punt_cols %in% names(tabla_wide)]
  tabla_wide <- dplyr::select(tabla_wide, .data$region, dplyr::all_of(punt_cols))

  # --- Salidas ---
  if (!isTRUE(gt)) {
    return(tibble::as_tibble(tabla_wide))
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("No se encontró el paquete 'gt'. Devuelvo tibble en su lugar.")
    return(tibble::as_tibble(tabla_wide))
  }

  etiquetas <- stats::setNames(
    object = paste("Puntaje", ediciones),
    nm     = paste0("puntaje_", ediciones)
  )

  gt::gt(tabla_wide) |>
    gt::tab_header(
      title = "Índice de Competitividad Regional — Puntaje General",
      subtitle = if (length(ediciones) == 1) {
        paste0("Edición ", ediciones)
      } else {
        paste0("Ediciones ", min(ediciones), "–", max(ediciones))
      }
    ) |>
    gt::cols_label(.list = etiquetas) |>
    gt::fmt_number(columns = dplyr::all_of(punt_cols), decimals = 2) |>
    gt::tab_options(table.font.size = gt::px(14))
}
