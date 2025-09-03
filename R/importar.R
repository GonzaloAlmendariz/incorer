#' Leer la base INCORE y devolver SOLO puntajes (0–10)
#'
#' - Importa el Excel (web o local) y estandariza nombres/tipos.
#' - Filtra SIEMPRE a filas con `Unidad == "Puntaje del 0 al 10"`.
#' - Opcionalmente agrega `pilar_cod` (ECO, LAB, …) e `ind_cod` (ECO1, LAB3, …).
#' - Permite filtrar por edición/año/pilar/indicador/región usando códigos o nombres.
#'
#' @param ruta URL o ruta local al Excel INCORE (por defecto el 2025).
#' @param hoja Índice o nombre de hoja (default 1).
#' @param edicion (opc) año(s) de edición INCORE (num o char).
#' @param anio (opc) año(s) de referencia del dato (num o char).
#' @param pilar (opc) pilar(es), códigos (ECO, LAB) o nombres.
#' @param indicador (opc) indicador(es), códigos (LAB3) o nombres largos.
#' @param region (opc) región(es), códigos (AMZ, AQP, …) o nombres.
#' @param usar_codigos Si TRUE, traduce códigos de entrada a nombres antes de filtrar.
#' @param agregar_codigos Si TRUE, añade columnas `pilar_cod` e `ind_cod`.
#' @param verbose Si TRUE, muestra mensajes.
#'
#' @return tibble con solo puntajes 0–10 (posiblemente filtrado). Si
#'         `agregar_codigos=TRUE`, incluye `pilar_cod` e `ind_cod`.
#' @export
leer_incore <- function(
    ruta = "https://incore-spaces.nyc3.digitaloceanspaces.com/documents/1752079464_INCORE_2025_OpenData_1752079464.xlsx",
    hoja = 1,
    edicion = NULL,
    anio = NULL,
    pilar = NULL,
    indicador = NULL,
    region = NULL,
    usar_codigos = TRUE,
    agregar_codigos = FALSE,
    verbose = TRUE
) {
  # --- 1) Descarga si es URL ---
  is_url <- grepl("^https?://", ruta, ignore.case = TRUE)
  src <- ruta
  if (is_url) {
    tf <- tempfile(fileext = ".xlsx")
    if (verbose) message("Descargando datos desde la web…")
    utils::download.file(ruta, tf, mode = "wb", quiet = TRUE)
    src <- tf
  }

  # --- 2) Leer y limpiar nombres ---
  if (verbose) message("Importando y limpiando datos…")
  df <- readxl::read_excel(src, sheet = hoja)
  df <- janitor::clean_names(df)

  # Normalizar `anio`
  if ("a\u00f1o" %in% names(df)) {
    df <- dplyr::rename(df, anio = !!rlang::sym("a\u00f1o"))
  } else if (!"anio" %in% names(df) && "ano" %in% names(df)) {
    df <- dplyr::rename(df, anio = ano)
  }

  # Tipos (best-effort)
  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("anio","edicion","posicion")), ~ suppressWarnings(as.integer(.x))),
      dplyr::across(dplyr::any_of(c("valor")), ~ suppressWarnings(as.numeric(.x)))
    )

  # --- 3) SOLO puntajes 0–10 ---
  if (!"unidad" %in% names(df)) {
    stop("La base no contiene columna `unidad`; no puedo filtrar a 'Puntaje del 0 al 10'.")
  }
  unidad_norm <- stringr::str_squish(stringr::str_to_lower(df$unidad))
  es_puntaje <- unidad_norm %in% c("puntaje del 0 al 10","puntaje 0 al 10","puntaje (0 al 10)")
  n_antes <- nrow(df)
  df <- df[es_puntaje, , drop = FALSE]
  if (verbose) message("Filtrando a 'Puntaje del 0 al 10': ", nrow(df), " filas (de ", n_antes, ").")
  if (nrow(df) == 0L) stop("No hay filas con 'Unidad = Puntaje del 0 al 10'.")

  # --- 4) Estandarizar strings básicos (sin usar el pronombre .) ---
  if ("pilar" %in% names(df))   df$pilar   <- stringr::str_squish(df$pilar)
  if ("indicador" %in% names(df)) df$indicador <- stringr::str_squish(df$indicador)
  if ("region" %in% names(df))  df$region  <- stringr::str_squish(df$region)

  # --- 5) Agregar códigos (si se pide) ---
  if (isTRUE(agregar_codigos)) {
    # Intentar con catálogos si existen; si fallan, usar fallback.
    cat_p <- try(catalogo_pilar(), silent = TRUE)
    cat_i <- try(catalogo_indicador(), silent = TRUE)

    # pilar_cod (join por nombre de pilar)
    if (!inherits(cat_p, "try-error") && all(c("codigo","nombre") %in% names(cat_p)) && "pilar" %in% names(df)) {
      cat_p2 <- dplyr::rename(cat_p, pilar_cod = codigo, pilar_nombre = nombre)
      df <- dplyr::left_join(df, cat_p2, by = c("pilar" = "pilar_nombre"))
    } else {
      df$pilar_cod <- NA_character_
    }

    # ind_cod (join por nombre de indicador)
    if (!inherits(cat_i, "try-error") && all(c("codigo","nombre") %in% names(cat_i)) && "indicador" %in% names(df)) {
      cat_i2 <- dplyr::rename(cat_i, ind_cod = codigo, ind_nombre = nombre)
      df <- dplyr::left_join(df, cat_i2, by = c("indicador" = "ind_nombre"))
    } else {
      df$ind_cod <- NA_character_
    }

    # --- Fallback si quedó NA: construir códigos desde el texto largo ---
    # Regla: "X.Y ..." → mapear X a pilar (1=ECO,2=LAB,3=INF,4=SAL,5=EDU,6=INS) y Y a índice.
    map_num2pil <- c("1"="ECO","2"="LAB","3"="INF","4"="SAL","5"="EDU","6"="INS")

    # Si pilar_cod sigue NA, intentar mapear por nombre de pilar
    if ("pilar" %in% names(df)) {
      if (all(is.na(df$pilar_cod))) {
        map_pilar <- c(
          "Entorno económico"="ECO","Laboral"="LAB","Infraestructura"="INF",
          "Salud"="SAL","Educación"="EDU","Instituciones"="INS",
          "Índice de Competitividad Regional"="GEN","Indice de Competitividad Regional"="GEN"
        )
        df$pilar_cod <- dplyr::coalesce(df$pilar_cod, unname(map_pilar[df$pilar]))
      }
    }

    # Construir ind_cod desde el nombre del indicador si hace falta
    if ("indicador" %in% names(df)) {
      falta_ind <- is.na(df$ind_cod) | df$ind_cod == ""
      if (any(falta_ind)) {
        # extraer el "X.Y" inicial
        base_xy <- stringr::str_match(df$indicador[falta_ind], "^\\s*([1-6])\\.(\\d+)")[, 2:3, drop = FALSE]
        pref <- map_num2pil[base_xy[, 1]]
        suf  <- base_xy[, 2]
        ind_fallback <- ifelse(!is.na(pref) & !is.na(suf), paste0(pref, suf), NA_character_)
        df$ind_cod[falta_ind] <- ind_fallback
      }
    }

    # Asegurar que si aún queda NA, al menos caiga en el propio texto (no ideal, pero estable)
    if ("pilar_cod" %in% names(df)) df$pilar_cod <- dplyr::coalesce(df$pilar_cod, df$pilar)
    if ("ind_cod"   %in% names(df)) df$ind_cod   <- dplyr::coalesce(df$ind_cod, df$indicador)
  }

  # --- 6) Traducción de códigos de entrada (si corresponde) ---
  if (isTRUE(usar_codigos)) {
    # años / edición → as.integer si son char con números
    if (!is.null(edicion)) edicion <- suppressWarnings(as.integer(traducir_codigo(as.character(edicion), catalogo_anio())))
    if (!is.null(anio))    anio    <- suppressWarnings(as.integer(traducir_codigo(as.character(anio), catalogo_anio())))

    if (!is.null(pilar) && "pilar" %in% names(df)) {
      # traducir a nombre si se pasó código
      p_tr <- try(traducir_codigo(pilar, catalogo_pilar()), silent = TRUE)
      if (!inherits(p_tr, "try-error")) pilar <- dplyr::coalesce(p_tr, pilar)
    }
    if (!is.null(indicador) && "indicador" %in% names(df)) {
      i_tr <- try(traducir_codigo(indicador, catalogo_indicador()), silent = TRUE)
      if (!inherits(i_tr, "try-error")) indicador <- dplyr::coalesce(i_tr, indicador)
    }
    if (!is.null(region) && "region" %in% names(df)) {
      r_tr <- try(traducir_codigo(region, catalogo_region()), silent = TRUE)
      if (!inherits(r_tr, "try-error")) region <- unique(c(stats::na.omit(r_tr), region))
    }
  }

  # --- 7) Aplicar filtros (edicion / anio / pilar / indicador / region) ---
  if (verbose) message("Aplicando filtros seleccionados…")

  if (!is.null(edicion) && "edicion" %in% names(df)) {
    keep <- df$edicion %in% edicion
    if (any(keep, na.rm = TRUE)) df <- df[keep, , drop = FALSE]
    else if (verbose) message("⚠️ No hay filas con edicion %in% {", paste(edicion, collapse = ", "), "}.")
  }

  if (!is.null(anio) && "anio" %in% names(df)) {
    keep <- df$anio %in% anio
    if (any(keep, na.rm = TRUE)) df <- df[keep, , drop = FALSE]
    else if (verbose) message("⚠️ No hay filas con anio %in% {", paste(anio, collapse = ", "), "}.")
  }

  # Filtros por pilar/indicador aceptan códigos o nombres si están disponibles
  if (!is.null(pilar) && "pilar" %in% names(df)) {
    if (isTRUE(agregar_codigos) && "pilar_cod" %in% names(df)) {
      df <- df[df$pilar %in% pilar | df$pilar_cod %in% pilar, , drop = FALSE]
    } else {
      df <- df[df$pilar %in% pilar, , drop = FALSE]
    }
  }

  if (!is.null(indicador) && "indicador" %in% names(df)) {
    if (isTRUE(agregar_codigos) && "ind_cod" %in% names(df)) {
      df <- df[df$indicador %in% indicador | df$ind_cod %in% indicador, , drop = FALSE]
    } else {
      df <- df[df$indicador %in% indicador, , drop = FALSE]
    }
  }

  if (!is.null(region) && "region" %in% names(df)) {
    df <- df[df$region %in% region, , drop = FALSE]
  }

  if (verbose) message("Datos listos: ", nrow(df), " filas, ", ncol(df), " columnas (solo puntaje 0–10).")
  tibble::as_tibble(df)
}
