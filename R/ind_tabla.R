#' Tabla de INCORE por ediciones (general) o por pilares (lista para radar)
#'
#' Construye tablas a partir del INCORE reutilizando `leer_incore()`:
#'
#' - **modo = "general"**: arma una tabla por *ediciones* (filas = regiones,
#'   columnas = `puntaje_YYYY`). Equivale a una versión flexible de `general_tabla()`.
#'
#' - **modo = "pilares"** (una sola edición): arma los 6 pilares por región. Si
#'   `largo = TRUE`, devuelve tres columnas (`region`, `pilar`, `valor`)
#'   (ideal para `general_radar()`). **Nota:** la columna `edicion` se omite
#'   en la salida porque es redundante.
#'
#' Siempre ordena alfabéticamente por `region`. En *modo general* los nombres
#' de columnas usan el prefijo `puntaje_YYYY`.
#'
#' @param ediciones Vector numérico con una o más ediciones (p. ej. `2025` o `2019:2025`).
#'   En `modo = "pilares"` debe tener longitud 1.
#' @param regiones Vector de regiones (códigos, nombres o grupos `gr_*`). Use `"ALL"` para todas.
#' @param modo `"general"` o `"pilares"`.
#' @param largo Si `TRUE` y `modo="pilares"`, devuelve formato largo (`region, pilar, valor`).
#' @param usar_codigos Si `TRUE`, traduce códigos a nombres oficiales antes de filtrar.
#' @param gt Si `TRUE`, devuelve una tabla `gt`; en caso contrario, un `tibble`.
#' @param verbose Si `TRUE`, imprime mensajes informativos.
#'
#' @return Un `tibble` (por defecto) o un objeto `gt` si `gt = TRUE`.
#' @export
ind_tabla <- function(ediciones,
                       regiones = "ALL",
                       modo = c("general", "pilares"),
                       largo = FALSE,
                       usar_codigos = TRUE,
                       gt = FALSE,
                       verbose = FALSE) {

  modo <- match.arg(modo)

  # --- validaciones básicas ---
  if (missing(ediciones) || length(ediciones) < 1L) {
    stop("Debes especificar al menos una edición en 'ediciones'.")
  }
  if (!is.numeric(ediciones)) {
    stop("'ediciones' debe ser numérico (p. ej., 2025 o 2019:2025).")
  }
  ediciones <- sort(unique(as.integer(ediciones)))
  if (modo == "pilares" && length(ediciones) != 1L) {
    stop("Para 'modo = \"pilares\"' debes pasar UNA sola edición.")
  }

  # --- lectura base (reutiliza leer_incore) ---
  if (verbose) message("Leyendo INCORE…")
  datos <- leer_incore(
    edicion      = ediciones,
    usar_codigos = usar_codigos,
    verbose      = FALSE
  )

  # Normalizar columnas clave y trims
  datos <- indc_normalizar_columnas(datos) |>
    dplyr::mutate(
      region    = stringr::str_squish(.data$region),
      pilar     = stringr::str_squish(.data$pilar),
      indicador = stringr::str_squish(.data$indicador)
    )

  # Filtro de regiones (grupos + exclusiones + códigos)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- indc_resolver_regiones(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    datos <- dplyr::filter(datos, .data$region %in% regiones_filtrar)
  }

  # -------------------------------
  # MODO: GENERAL (por ediciones)
  # -------------------------------
  if (modo == "general") {
    datos_g <- datos |>
      dplyr::filter(stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"),
                    .data$region != "Perú") |>
      dplyr::summarise(
        valor = mean(.data$valor, na.rm = TRUE),
        .by = c(.data$region, .data$edicion)
      ) |>
      dplyr::mutate(
        valor    = round(.data$valor, 2),
        col_name = paste0("puntaje_", .data$edicion)
      )

    # wide: filas región, columnas puntaje_YYYY
    tab_wide <- tidyr::pivot_wider(
      datos_g |> dplyr::select(.data$region, .data$col_name, .data$valor),
      names_from = .data$col_name,
      values_from = .data$valor
    ) |>
      dplyr::arrange(.data$region)

    if (!isTRUE(gt)) return(tibble::as_tibble(tab_wide))

    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("No se encontró el paquete 'gt'. Devuelvo tibble.")
      return(tibble::as_tibble(tab_wide))
    }

    punt_cols <- paste0("puntaje_", ediciones)
    punt_cols <- punt_cols[punt_cols %in% names(tab_wide)]

    out_gt <- gt::gt(tab_wide) |>
      gt::tab_header(
        title = "Índice de Competitividad Regional — Puntaje General",
        subtitle = if (length(ediciones) == 1) {
          paste0("Edición ", ediciones)
        } else {
          paste0("Ediciones ", min(ediciones), "–", max(ediciones))
        }
      ) |>
      { \(x) {
        for (yy in ediciones) {
          nm <- paste0("puntaje_", yy)
          if (nm %in% names(tab_wide)) {
            x <- gt::cols_label(x, !!nm := gt::md(paste0("**Puntaje ", yy, "**")))
          }
        }
        x
      } }() |>
      gt::fmt_number(columns = dplyr::all_of(punt_cols), decimals = 2) |>
      gt::tab_source_note("Fuente: Instituto Peruano de Economía (IPE)") |>
      gt::tab_options(table.font.size = gt::px(14))

    return(out_gt)
  }

  # ------------------------------------
  # MODO: PILARES (una edición, 6 pilares)
  # ------------------------------------
  pilares_orden <- c("Entorno económico","Laboral","Infraestructura","Salud","Educación","Instituciones")

  datos_p <- datos |>
    dplyr::filter(.data$indicador == "General",
                  .data$pilar %in% pilares_orden) |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      .by = c(.data$edicion, .data$region, .data$pilar)
    ) |>
    dplyr::mutate(pilar = factor(.data$pilar, levels = pilares_orden))

  # salida LARGA (region, pilar, valor) — sin edicion
  if (isTRUE(largo)) {
    out_long <- datos_p |>
      dplyr::select(.data$region, .data$pilar, .data$valor) |>
      dplyr::mutate(valor = round(.data$valor, 2)) |>
      dplyr::arrange(.data$region, .data$pilar)

    if (!isTRUE(gt)) return(out_long)

    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("No se encontró el paquete 'gt'. Devuelvo tibble.")
      return(out_long)
    }

    return(
      out_long |>
        tidyr::pivot_wider(names_from = .data$pilar, values_from = .data$valor) |>
        dplyr::arrange(.data$region) |>
        gt::gt(rowname_col = "region") |>
        gt::fmt_number(columns = where(is.numeric), decimals = 2) |>
        gt::tab_header(
          title = "INCORE — Pilares (formato para radar)",
          subtitle = paste0("Puntaje General (", ediciones, ")")
        ) |>
        gt::tab_source_note("Fuente: Instituto Peruano de Economía (IPE)") |>
        gt::tab_options(table.font.size = gt::px(14))
    )
  }

  # salida ANCHA (una columna por pilar) — sin edicion
  tab_pilares <- datos_p |>
    dplyr::select(.data$region, .data$pilar, .data$valor) |>
    tidyr::pivot_wider(names_from = .data$pilar, values_from = .data$valor) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2))) |>
    dplyr::arrange(.data$region)

  if (!isTRUE(gt)) return(tab_pilares)

  if (!requireNamespace("gt", quietly = TRUE)) {
    warning("No se encontró el paquete 'gt'. Devuelvo tibble.")
    return(tab_pilares)
  }

  gt::gt(tab_pilares, rowname_col = "region") |>
    gt::fmt_number(columns = where(is.numeric), decimals = 2) |>
    gt::tab_header(
      title = "INCORE — Tabla de pilares",
      subtitle = paste0("Puntaje General (", ediciones, ")")
    ) |>
    gt::tab_source_note("Fuente: Instituto Peruano de Economía (IPE)") |>
    gt::tab_options(table.font.size = gt::px(14))
}
