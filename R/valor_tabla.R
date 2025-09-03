#' Tabla de valores por indicador (filas = regiones; columnas = indicador×edición)
#'
#' Construye una tabla con los **valores** (no puntajes) de los **indicadores**
#' de un **pilar** del INCORE para una o varias **ediciones**.
#' Las filas son **regiones** y las columnas son combinaciones *indicador × edición*
#' (p.ej., `ECO3_2020`). Puede devolver:
#' - un `tibble` largo (si `largo=TRUE`),
#' - un `tibble` ancho (default),
#' - o un `gt` con spanners por edición (si `gt=TRUE`).
#'
#' Siempre **excluye** `unidad == "Puntaje del 0 al 10"`.
#'
#' @param ediciones integer(). Una o más ediciones (p. ej. `2025` o `2018:2025`). **Obligatorio.**
#' @param pilar character(1). Código o nombre del pilar, p. ej. `"ECO"` o `"Entorno económico"`. **Obligatorio.**
#' @param indicadores "ALL" para todos los del pilar, o vector de códigos/nombres.
#'   También admite entradas que empiecen con número (p.ej., `"3"` o `"3.1 ..."`) y las mapea a `ECO3` dentro de ese pilar.
#' @param regiones "ALL" (default) o vector de regiones (códigos o nombres).
#' @param usar_codigos logical(1). Si `TRUE`, traduce códigos antes de filtrar (pilar/regiones).
#' @param incluir_peru logical(1). Si `TRUE`, incluye la fila `"Perú"`. Default `FALSE`.
#' @param largo logical(1). Si `TRUE`, devuelve tibble **largo** (`region, edicion, indicador, valor, unidad, fuente`).
#' @param gt logical(1). Si `TRUE`, devuelve una tabla `gt` con spanners por edición; si `FALSE`, devuelve tibble.
#' @param verbose logical(1). Mensajes informativos.
#'
#' @return
#' - Si `largo = TRUE`: `tibble` con columnas `region, edicion, indicador, valor, unidad, fuente`.
#' - Si `largo = FALSE` y `gt = FALSE`: `tibble` **ancho** (filas regiones; columnas indicador×edición).
#' - Si `largo = FALSE` y `gt = TRUE`: `gt` con spanners por edición.
#'
#' @export
valor_tabla <- function(ediciones,
                        pilar,
                        indicadores = "ALL",
                        regiones = "ALL",
                        usar_codigos = TRUE,
                        incluir_peru = FALSE,
                        largo = FALSE,
                        gt = FALSE,
                        verbose = FALSE) {

  # -------- validaciones básicas ----------
  if (missing(ediciones) || length(ediciones) < 1L || !is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico (p. ej., 2025 o 2018:2025).")
  }
  if (missing(pilar) || !is.character(pilar) || length(pilar) != 1L) {
    stop("'pilar' es obligatorio (código o nombre).")
  }
  valor_assert_rango_fechas(ediciones = ediciones)

  # deps mínimas
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE)) {
    stop("Necesitas 'dplyr', 'tidyr' y 'stringr'.")
  }

  # -------- traer base completa (NO puntajes) ----------
  base <- valor_traer_base_valores(
    ediciones   = ediciones,
    pilar       = pilar,
    regiones    = regiones,
    unidad      = "ALL",
    usar_codigos = usar_codigos,
    verbose     = verbose
  )

  if (nrow(base) == 0) {
    stop("No hay registros para ese pilar/ediciones/regiones.")
  }

  # excluir puntaje
  base <- dplyr::filter(base, .data$unidad != "Puntaje del 0 al 10")
  if (nrow(base) == 0) {
    stop("No hay valores (no-puntaje) para esos filtros.")
  }

  # excluir Perú si se pide
  if (!isTRUE(incluir_peru)) {
    base <- dplyr::filter(base, .data$region != "Perú")
  }
  if (nrow(base) == 0) stop("Sin datos tras excluir 'Perú'.")

  # -------- catálogo de indicadores (normalizado a code/name) ----------
  cat_ind <- try(catalogo_indicador(), silent = TRUE)
  if (inherits(cat_ind, "try-error") || !is.data.frame(cat_ind)) {
    stop("No se pudo leer 'catalogo_indicador()'.")
  }
  # soporto 'cod'/'codigo'/'code' y 'nombre'/'name'
  cn <- names(cat_ind)
  code_col <- intersect(c("code","codigo","cod"), cn)[1]
  name_col <- intersect(c("name","nombre"), cn)[1]
  if (is.na(code_col) || is.na(name_col)) {
    stop("El catálogo de indicadores debe tener columna de código (code/codigo/cod) y nombre (name/nombre).")
  }
  cat_ind <- dplyr::rename(cat_ind, code = !!rlang::sym(code_col), name = !!rlang::sym(name_col))
  cat_ind$code <- stringr::str_squish(toupper(cat_ind$code))
  cat_ind$name <- stringr::str_squish(cat_ind$name)

  # --- resolver código de pilar de forma robusta ---
  # intentamos usar el catálogo; si falla, aceptamos directamente un código válido
  pilar_code <- {
    # 1) si ya viene como código estándar, úsalo
    if (toupper(pilar) %in% c("ECO","LAB","INF","SAL","EDU","INS")) {
      toupper(pilar)
    } else {
      # 2) mapear nombre -> código desde el catálogo
      dic_p <- try(catalogo_pilar(), silent = TRUE)
      if (!inherits(dic_p, "try-error") && is.data.frame(dic_p)) {
        cn <- names(dic_p)
        code_col <- intersect(c("code","codigo","cod"), cn)[1]
        name_col <- intersect(c("name","nombre"), cn)[1]
        if (!is.na(code_col) && !is.na(name_col)) {
          dic_p <- dplyr::rename(dic_p, code = !!rlang::sym(code_col), name = !!rlang::sym(name_col))
          dic_p$code <- toupper(stringr::str_squish(dic_p$code))
          dic_p$name <- stringr::str_squish(dic_p$name)
          hit <- dic_p$code[dic_p$code == toupper(pilar) | dic_p$name == stringr::str_squish(pilar)]
          if (length(hit)) toupper(hit[1]) else stop("No pude resolver el pilar '", pilar, "'.")
        } else {
          stop("Catálogo de pilares sin columnas code/nombre.")
        }
      } else {
        stop("No se pudo leer 'catalogo_pilar()' para resolver el pilar.")
      }
    }
  }

  cat_p <- dplyr::filter(cat_ind, stringr::str_starts(.data$code, paste0("^", pilar_code)))

  # -------- resolver 'indicadores' (ALL / códigos / nombres / "3", "3.1...") ----------
  map_ind <- function(z) {
    Z <- toupper(gsub("\\s+", "", z))
    # 1) ya es código válido del pilar
    if (Z %in% cat_p$code) return(Z)
    # 2) empieza con dígitos -> ECO3
    m <- stringr::str_match(z, "^\\s*(\\d{1,2})")
    if (!is.na(m[1,2])) {
      cand <- paste0(pilar_code, as.integer(m[1,2]))
      if (cand %in% cat_p$code) return(cand)
    }
    # 3) nombre completo
    nm <- stringr::str_squish(z)
    if (nm %in% cat_p$name) return(cat_p$code[match(nm, cat_p$name)])
    NA_character_
  }

  wanted_codes <- if (identical(indicadores, "ALL")) {
    cat_p$code
  } else {
    raw <- unique(stringr::str_squish(as.character(indicadores)))
    out <- vapply(raw, map_ind, character(1))
    misses <- raw[is.na(out)]
    out <- unique(stats::na.omit(out))
    if (length(misses)) {
      warning("Indicadores no reconocidos y serán omitidos: ", paste(misses, collapse = ", "))
    }
    if (!length(out)) stop("Ningún indicador reconocido para ese pilar.")
    out
  }

  # en la base los indicadores están por NOMBRE → quedarnos con los que existan
  name_to_code <- stats::setNames(cat_p$code, cat_p$name)
  code_to_name <- stats::setNames(cat_p$name, cat_p$code)

  have_names <- intersect(unique(base$indicador), cat_p$name)
  have_codes <- unique(name_to_code[have_names])
  keep_codes <- intersect(wanted_codes, have_codes)
  if (!length(keep_codes)) stop("No hay valores para los indicadores solicitados en esas ediciones/regiones.")

  datos <- dplyr::filter(base, .data$indicador %in% code_to_name[keep_codes])

  # -------- resumen defensivo: un valor por (region, edicion, indicador) ----------
  datos <- datos |>
    dplyr::summarise(
      valor  = mean(.data$valor, na.rm = TRUE),
      unidad = dplyr::first(.data$unidad),
      fuente = dplyr::first(.data$fuente),
      .by = c(.data$region, .data$edicion, .data$indicador)
    ) |>
    dplyr::mutate(
      region  = stringr::str_squish(.data$region),
      edicion = as.integer(.data$edicion),
      indicador = stringr::str_squish(.data$indicador)
    ) |>
    dplyr::arrange(.data$region, .data$edicion, .data$indicador)

  if (nrow(datos) == 0L) stop("No hay datos tras resumir.")

  # -------- salida larga (opcional) ----------
  if (isTRUE(largo)) {
    return(dplyr::select(datos, .data$region, .data$edicion, .data$indicador, .data$valor, .data$unidad, .data$fuente))
  }

  # -------- construir nombres de columnas (preferir CÓDIGO si existe) ----------
  datos <- datos |>
    dplyr::left_join(
      dplyr::select(cat_p, code, name),
      by = c("indicador" = "name")
    )

  ind_key <- ifelse(!is.na(datos$code) & nzchar(datos$code),
                    datos$code,
                    janitor::make_clean_names(datos$indicador))

  datos <- dplyr::mutate(datos, colname = paste0(ind_key, "_", .data$edicion))

  # wide: filas = región; columnas = indicador×edición
  tab_wide <- tidyr::pivot_wider(
    datos |> dplyr::select(.data$region, .data$colname, .data$valor),
    names_from = .data$colname,
    values_from = .data$valor
  ) |>
    dplyr::arrange(.data$region)

  # -------- devolver tibble ancho o gt ----------
  if (!isTRUE(gt)) {
    return(tibble::as_tibble(tab_wide))
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("No se encontró 'gt'. Devuelvo tibble en su lugar.")
    return(tibble::as_tibble(tab_wide))
  }

  # spanners por edición (todas las columnas salvo region)
  all_cols <- setdiff(names(tab_wide), "region")
  ed_map <- tibble::tibble(
    col = all_cols,
    ed  = suppressWarnings(as.integer(sub(".*_(\\d{4})$", "\\1", all_cols)))
  )

  cap <- valor_fuente_caption(datos)

  g <- gt::gt(tab_wide) |>
    gt::tab_header(
      title = paste0("INCORE | Pilar: ", valor_resolver_pilar(pilar, usar_codigos = TRUE)),
      subtitle = if (length(ediciones) == 1) {
        paste0("Edición ", ediciones)
      } else {
        paste0("Ediciones ", min(ediciones), "–", max(ediciones))
      }
    ) |>
    gt::fmt_number(columns = all_cols, decimals = 2) |>
    gt::tab_options(table.font.size = gt::px(14)) |>
    gt::tab_source_note(cap)

  for (ed in sort(unique(ed_map$ed))) {
    cols_ed <- ed_map$col[ed_map$ed == ed]
    if (length(cols_ed)) {
      g <- gt::tab_spanner(g, label = paste0("Edición ", ed), columns = cols_ed)
    }
  }

  g
}
