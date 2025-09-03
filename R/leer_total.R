#' Leer la base INCORE completa (web o local) con filtros opcionales
#'
#' Descarga/lee el Excel del INCORE (2025) y devuelve **todas** las filas,
#' incluidos registros cuya \code{unidad} no es "Puntaje del 0 al 10".
#' Admite filtros por \code{edicion}, \code{anio}, \code{pilar}, \code{indicador},
#' \code{region} y \code{unidad}. Útil para explorar "la base total".
#'
#' A diferencia de las funciones \strong{general_*} e \strong{indc_*}, aquí
#' \emph{no} se filtra por unidad. Si lo deseas, puedes fijar
#' \code{unidad = "Puntaje del 0 al 10"} para quedarte sólo con puntajes.
#'
#' @param ruta Ruta local o URL pública del Excel. Por defecto usa la URL 2025.
#' @param hoja Índice o nombre de la hoja del Excel (por defecto 1).
#' @param edicion (Opcional) Año(s) de edición (p.ej., 2016:2025). Numérico o carácter.
#' @param anio (Opcional) Año(s) de referencia del dato (p.ej., 2020:2024).
#' @param pilar (Opcional) Pilar(es), código(s) o nombre(s).
#' @param indicador (Opcional) Indicador(es), código(s) o nombre(s).
#' @param region (Opcional) Región(es), código(s) o nombre(s). Use \code{"ALL"} para todas.
#' @param unidad (Opcional) Cadena exacta a filtrar en la columna \code{unidad}.
#'   Use \code{"ALL"} (default) para no filtrar. Ej.: \code{"Puntaje del 0 al 10"}.
#' @param usar_codigos Si \code{TRUE} (default), traduce códigos a nombres oficiales
#'   (\code{catalogo_*()}) antes de filtrar.
#' @param verbose Si \code{TRUE}, imprime mensajes informativos.
#'
#' @return Un \code{tibble} con columnas estandarizadas:
#' \code{edicion}, \code{pilar}, \code{indicador}, \code{unidad}, \code{fuente},
#' \code{nota}, \code{region}, \code{etiqueta}, \code{valor}, \code{posicion}, \code{anio}.
#'
#' @examples
#' \donttest{
#' # Todo sin filtrar
#' # leer_total(verbose = FALSE)
#'
#' # Sólo puntajes (0–10) del pilar Laboral en 2025
#' # leer_total(edicion = 2025, pilar = "LAB", unidad = "Puntaje del 0 al 10")
#'
#' # Varias regiones por nombre/código y años de dato 2021–2024
#' # leer_total(anio = 2021:2024, region = c("MOQ","Arequipa","Lima*"))
#' }
#' @export
leer_total <- function(
    ruta = "https://incore-spaces.nyc3.digitaloceanspaces.com/documents/1752079464_INCORE_2025_OpenData_1752079464.xlsx",
    hoja = 1,
    edicion = NULL,
    anio = NULL,
    pilar = NULL,
    indicador = NULL,
    region = NULL,
    unidad = "ALL",
    usar_codigos = TRUE,
    verbose = TRUE
) {
  # --- Validaciones rápidas y rango temporal de referencia ---
  rng_val <- 2016:2025
  if (!is.null(edicion)) {
    ed_ok <- suppressWarnings(as.integer(edicion))
    if (any(is.na(ed_ok)) || any(!ed_ok %in% rng_val)) {
      stop("Argumento 'edicion' fuera de rango permitido (2016–2025) o no numérico.")
    }
  }
  if (!is.null(anio)) {
    an_ok <- suppressWarnings(as.integer(anio))
    if (any(is.na(an_ok)) || any(!an_ok %in% rng_val)) {
      stop("Argumento 'anio' fuera de rango permitido (2016–2025) o no numérico.")
    }
  }

  # --- 1) Descargar si es URL (con manejo de error claro) ---
  is_url <- grepl("^https?://", ruta, ignore.case = TRUE)
  src <- ruta
  if (is_url) {
    tf <- tempfile(fileext = ".xlsx")
    if (verbose) message("Descargando datos desde la web…")
    ok <- tryCatch({
      utils::download.file(ruta, tf, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("✖ No se pudo descargar el archivo desde la URL proporcionada.")
      message("  Detalle: ", conditionMessage(e))
      FALSE
    })
    if (!ok) stop("Descarga fallida. Verifica la conexión/URL.")
    src <- tf
  }

  # --- 2) Importar y normalizar nombres/tipos ---
  if (verbose) message("Importando y limpiando datos…")
  df <- readxl::read_excel(src, sheet = hoja) |>
    janitor::clean_names()

  # Normalizar 'año'/'ano' -> 'anio'
  if ("a\u00f1o" %in% names(df)) {
    df <- dplyr::rename(df, anio = !!rlang::sym("a\u00f1o"))
  } else if (!"anio" %in% names(df) && "ano" %in% names(df)) {
    df <- dplyr::rename(df, anio = ano)
  }

  # Coerción de tipos conocidos (si existen)
  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("edicion", "anio", "posicion")), ~ suppressWarnings(as.integer(.x))),
      dplyr::across(dplyr::any_of(c("valor")), ~ suppressWarnings(as.numeric(.x)))
    )

  # --- 3) Traducción de códigos a nombres oficiales (si se pide) ---
  # Se traduce sólo si el usuario pasó códigos y la columna existe en df.
  if (usar_codigos) {
    if (!is.null(pilar) && "pilar" %in% names(df) && !all(pilar %in% unique(df$pilar))) {
      pilar <- traducir_codigo(pilar, catalogo_pilar())
    }
    if (!is.null(indicador) && "indicador" %in% names(df) && !all(indicador %in% unique(df$indicador))) {
      indicador <- traducir_codigo(indicador, catalogo_indicador())
    }
    if (!is.null(region) && "region" %in% names(df) && !identical(region, "ALL") &&
        !all(region %in% unique(df$region))) {
      region <- traducir_codigo(region, catalogo_region())
    }
  }

  # --- 4) Filtros opcionales (NO filtramos por 'unidad' salvo que el usuario lo pida) ---
  if (verbose) message("Aplicando filtros seleccionados…")

  if (!is.null(edicion) && "edicion" %in% names(df)) {
    df <- dplyr::filter(df, .data$edicion %in% !!as.integer(edicion))
  }
  if (!is.null(anio) && "anio" %in% names(df)) {
    df <- dplyr::filter(df, .data$anio %in% !!as.integer(anio))
  }
  if (!is.null(pilar) && "pilar" %in% names(df)) {
    df <- dplyr::filter(df, .data$pilar %in% !!pilar)
  }
  if (!is.null(indicador) && "indicador" %in% names(df)) {
    df <- dplyr::filter(df, .data$indicador %in% !!indicador)
  }
  if (!identical(region, "ALL") && !is.null(region) && "region" %in% names(df)) {
    df <- dplyr::filter(df, .data$region %in% !!region)
  }
  if (!identical(unidad, "ALL") && "unidad" %in% names(df)) {
    df <- dplyr::filter(df, .data$unidad %in% !!unidad)
  }

  # --- 5) Orden y salida ---
  # Ordenamos por edicion/anio/region/indicador para que sea amable al ojo
  ord_cols <- intersect(c("edicion", "anio", "pilar", "indicador", "region"), names(df))
  if (length(ord_cols)) {
    df <- dplyr::arrange(df, !!!rlang::syms(ord_cols))
  }

  if (verbose) message("Datos listos: ", nrow(df), " filas, ", ncol(df), " columnas.")
  tibble::as_tibble(df)
}
