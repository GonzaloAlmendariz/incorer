#' Tabla de indicadores por pilar (filas = regiones; columnas = indicadores×edición)
#'
#' Construye una tabla con los **indicadores** de un **pilar** del INCORE para
#' una o varias **ediciones**. Las filas son **regiones** y las columnas son
#' combinaciones *indicador × edición* (por ejemplo, `LAB2_2025`).
#' Puede devolver un `tibble` (por defecto), un `gt` con encabezados por edición
#' (spanners), o datos en **formato largo** para graficar.
#'
#' La función parte de `leer_incore()` y:
#' - filtra por `pilar` (código o nombre),
#' - permite seleccionar `indicadores` (códigos o nombres; `"ALL"` para todos los del pilar),
#' - promedia si hubiera duplicados esporádicos,
#' - ordena regiones alfabéticamente,
#' - arma nombres de columnas `CODIGO_EDICION` (si hay código) o `indicador_limpio_EDICION`.
#'
#' @param ediciones Vector numérico con una o más ediciones (p. ej. `2025` o `2018:2025`). **Obligatorio**.
#' @param pilar Cadena con el pilar (código o nombre), p. ej. `"LAB"` o `"Laboral"`. **Obligatorio**.
#' @param indicadores `"ALL"` (default) para todos los indicadores del pilar, o vector de códigos/nombres.
#'   Se ignorará el indicador `"General"` salvo que se pida explícitamente.
#' @param regiones `"ALL"` (default) o vector de regiones (códigos, nombres o grupos `gr_*`; admite exclusiones con `-`).
#' @param usar_codigos Si `TRUE` (default), traduce códigos a nombres oficiales antes de filtrar.
#' @param incluir_peru `FALSE` (default). Si `TRUE`, incluye la fila `"Perú"`.
#' @param largo Si `TRUE`, devuelve un tibble **largo** con columnas `region, edicion, indicador, valor`.
#'              La columna `indicador` usa el **código** cuando está disponible.
#' @param gt Si `TRUE`, devuelve una tabla `gt` con spanners por edición; si `FALSE` (default), devuelve `tibble`.
#' @param verbose Mostrar mensajes informativos (`FALSE` por defecto).
#'
#' @return
#' - Si `largo = TRUE`: `tibble` con columnas `region, edicion, indicador, valor`.
#' - Si `largo = FALSE` y `gt = FALSE`: `tibble` ancho (filas regiones; columnas indicadores×edición).
#' - Si `largo = FALSE` y `gt = TRUE`: tabla `gt` formateada con spanners por edición.
#' @export
indc_tabla <- function(ediciones,
                       pilar,
                       indicadores = "ALL",
                       regiones = "ALL",
                       usar_codigos = TRUE,
                       incluir_peru = FALSE,
                       largo = FALSE,
                       gt = FALSE,
                       verbose = FALSE) {

  # --- Validaciones y deps ---
  if (missing(ediciones) || length(ediciones) < 1L || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico (p. ej., 2025 o 2018:2025).")
  }
  if (missing(pilar) || length(pilar) != 1L) {
    stop("Debes especificar un 'pilar' (código o nombre).")
  }
  ediciones <- sort(unique(as.integer(ediciones)))
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE)) {
    stop("Necesitas 'dplyr', 'tidyr' y 'stringr'.")
  }

  # Validación dura del rango INCORE
  indc_validar_rango(ediciones = ediciones)

  # --- Catálogos auxiliares ---
  dic_pilar <- try(catalogo_pilar(), silent = TRUE)
  dic_ind   <- try(catalogo_indicador(), silent = TRUE)

  # Resolver pilar (devolver NOMBRE oficial si vino código)
  pilar_nom <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)

  # --- Leer base con códigos y normalizar ---
  datos <- indc_leer_incore(
    edicion         = ediciones,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = verbose
  ) |>
    indc_normalizar_columnas() |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$indicador != "General" | identical(indicadores, "ALL"),
                  .data$region != "Perú" | isTRUE(incluir_peru))

  # Filtrar por pilar (acepta código o nombre)
  if (!all(is.na(datos$pilar_cod)) && pilar %in% datos$pilar_cod) {
    datos <- dplyr::filter(datos, .data$pilar_cod == !!pilar)
  } else {
    datos <- dplyr::filter(datos, .data$pilar == !!pilar_nom)
  }

  # Filtrar regiones (grupos + exclusiones + códigos)
  datos <- indc_filtrar_regiones(datos, regiones = regiones, usar_codigos = usar_codigos)

  # Filtrar indicadores (códigos o nombres) si no es "ALL"
  if (!identical(indicadores, "ALL")) {
    # Traducir mezcla de códigos/nombres -> (preferimos códigos)
    if (!inherits(dic_ind, "try-error")) {
      # columnas seguras
      dic_ind <- dplyr::rename(
        dic_ind,
        cod = dplyr::any_of(c("codigo","code")),
        nom = dplyr::any_of(c("nombre","name"))
      )
      ind_in <- unique(indicadores)
      ind_codes <- c(
        ind_in[ind_in %in% dic_ind$cod],
        dic_ind$cod[match(ind_in[ind_in %in% dic_ind$nom], dic_ind$nom)]
      )
      ind_codes <- unique(stats::na.omit(ind_codes))
      if (length(ind_codes)) {
        datos <- dplyr::filter(datos, .data$ind_cod %in% ind_codes | .data$indicador %in% ind_in)
      } else {
        datos <- dplyr::filter(datos, .data$indicador %in% ind_in)
      }
    } else {
      datos <- dplyr::filter(datos, .data$indicador %in% indicadores | .data$ind_cod %in% indicadores)
    }
  } else {
    datos <- dplyr::filter(datos, .data$indicador != "General")
  }

  # Consolidar (defensivo) a un valor por (region, edicion, indicador)
  datos <- datos |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      .by = c(.data$region, .data$edicion, .data$indicador, .data$ind_cod)
    ) |>
    dplyr::mutate(
      region  = stringr::str_squish(.data$region),
      edicion = as.integer(.data$edicion),
      valor   = round(.data$valor, 2)
    ) |>
    dplyr::arrange(.data$region, .data$edicion, dplyr::coalesce(.data$ind_cod, .data$indicador))

  if (nrow(datos) == 0L) {
    stop("No hay datos para las combinaciones solicitadas (pilar/indicadores/ediciones/regiones).")
  }

  # --- Salida LARGA (opcional) ---
  if (isTRUE(largo)) {
    ind_col <- dplyr::if_else(!is.na(datos$ind_cod) & nzchar(datos$ind_cod),
                              datos$ind_cod, datos$indicador)
    out <- dplyr::tibble(
      region   = datos$region,
      edicion  = datos$edicion,
      indicador= ind_col,
      valor    = datos$valor
    )
    return(out)
  }

  # --- Construcción de nombres de columnas (ancho) ---
  # Preferir CÓDIGO; si no hay, usar nombre limpio
  make_clean <- function(x) {
    if (requireNamespace("janitor", quietly = TRUE)) {
      janitor::make_clean_names(x)
    } else {
      x |>
        gsub("[^A-Za-z0-9]+","_", x = _) |>
        gsub("_+$","", x = _) |>
        tolower()
    }
  }

  ind_key <- dplyr::if_else(!is.na(datos$ind_cod) & nzchar(datos$ind_cod),
                            datos$ind_cod, make_clean(datos$indicador))
  datos <- dplyr::mutate(datos, colname = paste0(ind_key, "_", .data$edicion))

  tab_wide <- tidyr::pivot_wider(
    datos |> dplyr::select(.data$region, .data$colname, .data$valor),
    names_from  = .data$colname,
    values_from = .data$valor
  ) |>
    dplyr::arrange(.data$region)

  # --- Tibble o GT ---
  if (!isTRUE(gt)) return(tibble::as_tibble(tab_wide))

  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("No se encontró 'gt'. Devuelvo tibble en su lugar.")
    return(tibble::as_tibble(tab_wide))
  }

  # Spanners por edición
  all_cols <- setdiff(names(tab_wide), "region")
  ed_map <- dplyr::tibble(
    col = all_cols,
    ed  = suppressWarnings(as.integer(sub(".*_(\\d{4})$", "\\1", all_cols)))
  )

  g <- gt::gt(tab_wide) |>
    gt::tab_header(
      title    = paste0("INCORE | Pilar: ", pilar_nom),
      subtitle = if (length(ediciones) == 1) {
        paste0("Edición ", ediciones)
      } else {
        paste0("Ediciones ", min(ediciones), "–", max(ediciones))
      }
    ) |>
    gt::fmt_number(columns = all_cols, decimals = 2) |>
    gt::tab_options(table.font.size = gt::px(14))

  for (ed in sort(unique(ed_map$ed))) {
    cols_ed <- ed_map$col[ed_map$ed == ed]
    if (length(cols_ed) > 0) {
      g <- gt::tab_spanner(g, label = paste0("Edición ", ed), columns = cols_ed)
    }
  }

  g
}
