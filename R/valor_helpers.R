#' Helpers para la familia `valor_*`
#'
#' Funciones internas (validadores y utilitarios) usadas por `valor_*`.
#' Cargan catálogos vía `catalogo_*()` y usan `leer_total()` para traer la base
#' completa (no forzan unidad).
#'
#' @keywords internal
#' @noRd

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- util: normalizar catálogos a (code, name) ----
.vh_norm_catalogo <- function(tbl,
                              code_cols = c("codigo","code"),
                              name_cols = c("nombre","name")) {
  if (is.null(tbl) || !is.data.frame(tbl)) return(NULL)
  cn <- names(tbl)
  ccol <- intersect(code_cols, cn)[1]
  ncol <- intersect(name_cols, cn)[1]
  if (is.na(ccol) || is.na(ncol)) return(NULL)
  out <- tbl
  names(out)[match(ccol, names(out))] <- "code"
  names(out)[match(ncol, names(out))] <- "name"
  out
}

# --- NUEVO: normalizador robusto de texto ---
.vh_norm_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  # quitar tildes
  x <- if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trans_general(x, "Latin-ASCII")
  } else x
  # normalizar "pre/per capita"
  x <- gsub("\\b(pr|pe)r\\s*c(a|á)pit(a|á)\\b", "percapita", x)
  # colapsar espacios
  x <- gsub("\\s+", " ", x)
  x
}



# =========================
# Validaciones
# =========================

#' Validar rango de fechas permitido (2016–2025)
#'
#' Verifica que cualquiera de los argumentos temporales provistos
#' (`edicion`, `ediciones`, `anio`) estén dentro del rango 2016–2025.
#' Lanza `stop()` si encuentra valores fuera de rango o no numéricos.
#'
#' @param edicion integer(1) o `NULL`.
#' @param ediciones integer() o `NULL`.
#' @param anio integer() o `NULL`.
#' @param rango integer(). Secuencia válida. Default: `2016:2025`.
#' @return `invisible(TRUE)` si todo OK.
valor_assert_rango_fechas <- function(edicion = NULL,
                                      ediciones = NULL,
                                      anio = NULL,
                                      rango = 2016:2025) {
  vals <- integer(0)
  tags <- character(0)

  if (!is.null(edicion)) {
    e <- suppressWarnings(as.integer(edicion))
    vals <- c(vals, e); tags <- c(tags, rep("edicion", length(e)))
  }
  if (!is.null(ediciones)) {
    e <- suppressWarnings(as.integer(ediciones))
    vals <- c(vals, e); tags <- c(tags, rep("ediciones", length(e)))
  }
  if (!is.null(anio)) {
    a <- suppressWarnings(as.integer(anio))
    vals <- c(vals, a); tags <- c(tags, rep("anio", length(a)))
  }

  if (length(vals)) {
    bad_idx <- which(is.na(vals) | !(vals %in% rango))
    if (length(bad_idx)) {
      bad_vals <- vals[bad_idx]
      bad_src  <- tags[bad_idx]
      stop(
        paste0(
          "Fechas fuera de rango permitido (2016–2025) o no numéricas. ",
          "Valores problemáticos: ",
          paste0(sprintf("%s=%s", bad_src, bad_vals), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

# =========================
# Resolutores (región / pilar / indicador)
# =========================

#' Resolver regiones (códigos o nombres o grupos) a nombres oficiales
#' Soporta:
#'   - "ALL"
#'   - tokens de grupo: "gr_costa", "gr_sierra", etc. (definidos por el usuario)
#'   - exclusiones con prefijo "-": p.ej. c("gr_costa","-Lima*")
valor_resolver_regiones <- function(regiones = "ALL",
                                    usar_codigos = TRUE) {
  if (identical(regiones, "ALL")) return("ALL")

  # 1) separar inclusiones y exclusiones
  regiones <- unique(as.character(regiones))
  inc_tokens <- regiones[!startsWith(regiones, "-")]
  exc_tokens <- sub("^-", "", regiones[startsWith(regiones, "-")])

  # 2) expandir grupos (gr_*)
  expand_groups <- function(tokens) {
    if (!length(tokens)) return(character(0))
    groups <- incorer_get_region_groups()
    out <- unlist(lapply(tokens, function(tok) {
      if (startsWith(tok, "gr_") && tok %in% names(groups)) {
        groups[[tok]]
      } else {
        tok
      }
    }), use.names = FALSE)
    unique(out)
  }

  inc_expanded <- expand_groups(inc_tokens)
  exc_expanded <- expand_groups(exc_tokens)

  # 3) traducir códigos a nombres (si procede)
  translate_if_needed <- function(vec) {
    v <- stringr::str_squish(vec)
    if (isTRUE(usar_codigos)) {
      tr <- try(suppressWarnings(traducir_codigo(v, catalogo_region())), silent = TRUE)
      if (!inherits(tr, "try-error")) {
        # conservar originales si no machean
        v <- ifelse(is.na(tr), v, tr)
      }
    }
    unique(v)
  }

  inc_final <- translate_if_needed(inc_expanded)
  exc_final <- translate_if_needed(exc_expanded)

  # 4) aplicar exclusión
  if (length(exc_final)) {
    inc_final <- setdiff(inc_final, exc_final)
  }

  # si después de todo queda vacío, devolvemos vector vacío (caller decide)
  if (!length(inc_final)) character(0) else inc_final
}

#' Resolver pilar (código o nombre) -> nombre oficial
#' @param pilar character(1) o NULL
#' @param usar_codigos logical(1)
#' @return nombre del pilar (o el original si no se puede traducir)
valor_resolver_pilar <- function(pilar = NULL,
                                 usar_codigos = TRUE) {
  if (is.null(pilar)) return(NULL)
  p <- stringr::str_squish(as.character(pilar)[1])
  if (isTRUE(usar_codigos)) {
    tr <- try(suppressWarnings(traducir_codigo(p, catalogo_pilar())), silent = TRUE)
    if (!inherits(tr, "try-error") && length(tr) == 1L && !is.na(tr)) return(tr)
  }
  p
}

#' Resolver indicador (código o nombre) -> list(code, name)
#' @param indicador character(1) o NULL
#' @param usar_codigos logical(1)
#' @return list(code, name) (NULL/NULL si no se reconoce)
valor_resolver_indicador <- function(indicador = NULL,
                                     usar_codigos = TRUE) {
  if (is.null(indicador) || length(indicador) < 1) {
    return(list(code = NULL, name = NULL))
  }
  x_raw <- as.character(indicador[1])
  x_n   <- .vh_norm_text(x_raw)

  cat_ind <- try(catalogo_indicador(), silent = TRUE)
  cat_ok  <- is.data.frame(cat_ind)
  if (cat_ok) {
    # aceptar code/codigo y name/nombre
    cat_ind <- .vh_norm_catalogo(cat_ind, code_cols = c("code","codigo"),
                                 name_cols = c("name","nombre"))
    cat_ok <- is.data.frame(cat_ind) && all(c("code","name") %in% names(cat_ind))
  }

  if (cat_ok) {
    cat_ind$._norm_code <- .vh_norm_text(cat_ind$code)
    cat_ind$._norm_name <- .vh_norm_text(cat_ind$name)

    # 1) ¿llega como código exacto?
    if (x_raw %in% cat_ind$code) {
      return(list(code = x_raw, name = cat_ind$name[match(x_raw, cat_ind$code)]))
    }
    # 2) ¿llega como nombre exacto?
    if (x_raw %in% cat_ind$name) {
      return(list(code = cat_ind$code[match(x_raw, cat_ind$name)], name = x_raw))
    }
    # 3) ¿match por texto normalizado?
    i_code <- match(x_n, cat_ind$._norm_code)
    if (!is.na(i_code)) {
      return(list(code = cat_ind$code[i_code], name = cat_ind$name[i_code]))
    }
    i_name <- match(x_n, cat_ind$._norm_name)
    if (!is.na(i_name)) {
      return(list(code = cat_ind$code[i_name], name = cat_ind$name[i_name]))
    }
  }

  # fallback: si viene como "ECO3" o similar y el catálogo no ayudó
  if (grepl("^[A-Z]{3}\\d+$", x_raw)) {
    return(list(code = x_raw, name = x_raw))
  }
  list(code = NULL, name = NULL)
}

# =========================
# Traída de base (valores)
# =========================

#' Traer base para valores con filtros (NO filtra por unidad salvo que se pida)
#'
#' Envuelve a `leer_total()` aplicando normalizaciones mínimas y filtros.
#' No agrega; devuelve la base tal cual (tras filtros).
#'
#' @param edicion,ediciones,anio Filtros temporales.
#' @param pilar,indicador,regiones Filtros semánticos (códigos o nombres).
#' @param unidad character(1). `"ALL"` para no filtrar.
#' @param usar_codigos logical(1). Traduce códigos antes de filtrar.
#' @param verbose logical(1).
#' @return tibble con columnas estandarizadas de `leer_total()`.
valor_traer_base_valores <- function(edicion = NULL,
                                     ediciones = NULL,
                                     anio = NULL,
                                     pilar = NULL,
                                     indicador = NULL,
                                     regiones = "ALL",
                                     unidad = "ALL",
                                     usar_codigos = TRUE,
                                     verbose = FALSE) {

  valor_assert_rango_fechas(edicion = edicion, ediciones = ediciones, anio = anio)

  pilar_nm     <- valor_resolver_pilar(pilar, usar_codigos = usar_codigos)
  ind_res      <- valor_resolver_indicador(indicador, usar_codigos = usar_codigos)
  regiones_ok  <- valor_resolver_regiones(regiones, usar_codigos = usar_codigos)

  eds <- unique(stats::na.omit(as.integer(c(edicion %||% integer(0), ediciones %||% integer(0)))))
  an  <- if (!is.null(anio)) unique(stats::na.omit(as.integer(anio))) else NULL

  df <- leer_total(
    edicion      = if (length(eds)) eds else NULL,
    anio         = an,
    pilar        = pilar_nm,
    indicador    = if (!is.null(ind_res$code) || !is.null(ind_res$name))
      c(ind_res$code, ind_res$name) else indicador,
    region       = regiones_ok,
    unidad       = unidad,
    usar_codigos = usar_codigos,
    verbose      = verbose
  )

  # normalizaciones suaves
  nm <- names(df)
  if ("region"    %in% nm) df$region    <- stringr::str_squish(df$region)
  if ("pilar"     %in% nm) df$pilar     <- stringr::str_squish(df$pilar)
  if ("indicador" %in% nm) df$indicador <- stringr::str_squish(df$indicador)
  if ("unidad"    %in% nm) df$unidad    <- stringr::str_squish(df$unidad)
  if ("fuente"    %in% nm) df$fuente    <- stringr::str_squish(df$fuente)
  if ("valor"     %in% nm) df$valor     <- suppressWarnings(as.numeric(df$valor))
  if ("edicion"   %in% nm) df$edicion   <- suppressWarnings(as.integer(df$edicion))
  if ("anio"      %in% nm) df$anio      <- suppressWarnings(as.integer(df$anio))

  tibble::as_tibble(df)
}

# =========================
# Unidades: abreviación y formateo
# =========================

# mapa/heurística de unidades a etiqueta corta y tipo (para formateo)
.vh_unidad_map <- function(u) {
  u <- as.character(u %||% "")
  out <- lapply(u, function(s0) {
    s <- stringr::str_squish(s0)

    # Tipos: "pct", "pp", "moneda", "centavos_kwh", "numero", "horas", "indice", "otro"
    if (identical(s, "Puntaje del 0 al 10")) {
      list(short = "Puntaje (0–10)", type = "indice")
    } else if (grepl("puntos porcentuales", s, ignore.case = TRUE)) {
      list(short = "pp", type = "pp")
    } else if (grepl("^Porcentaje", s, ignore.case = TRUE) ||
               grepl("porcentaje del pbi", s, ignore.case = TRUE) ||
               grepl("porcentaje de", s, ignore.case = TRUE)) {
      list(short = "%", type = "pct")
    } else if (grepl("Centavos de S/ por kWh", s, ignore.case = TRUE)) {
      list(short = "ctv/kWh", type = "centavos_kwh")
    } else if (grepl("Millones de soles", s, ignore.case = TRUE)) {
      list(short = "Millones S/ (log)", type = "numero")
    } else if (grepl("Soles de Lima Metropolitana", s, ignore.case = TRUE) ||
               grepl("Soles constantes", s, ignore.case = TRUE) ||
               grepl("Soles de Lima Metropolitana del", s, ignore.case = TRUE)) {
      list(short = "S/ (const.)", type = "moneda")
    } else if (grepl("^Horas al d[ií]a$", s, ignore.case = TRUE)) {
      list(short = "horas/día", type = "horas")
    } else if (grepl("por cada 1,?000 habitantes", s, ignore.case = TRUE) ||
               grepl("^N[uú]mero de ", s, ignore.case = TRUE) ||
               grepl("^Expedientes resueltos", s, ignore.case = TRUE) ||
               grepl("^Conflictos sociales", s, ignore.case = TRUE)) {
      list(short = "Nº", type = "numero")
    } else {
      list(short = s, type = "otro")
    }
  })
  tibble::tibble(
    unidad       = u,
    unidad_short = vapply(out, `[[`, character(1L), "short"),
    unidad_tipo  = vapply(out, `[[`, character(1L), "type")
  )
}

#' Etiqueta corta de unidad (para ejes/títulos compactos)
#' @param unidad character(1) o vector
#' @return character() con abreviación
valor_unidad_short_label <- function(unidad) {
  .vh_unidad_map(unidad)$unidad_short
}

#' Formateo numérico condicional por unidad
#' @param x numeric()
#' @param unidad character(1) o `NULL`
#' @param accuracy numeric(1) para `scales::number()`
#' @return character()
valor_formato_valor <- function(x,
                                unidad = NULL,
                                accuracy = 0.01) {
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Falta 'scales' para formateo.")
  }
  x_num <- suppressWarnings(as.numeric(x))
  base  <- scales::number(x_num, accuracy = accuracy)

  if (is.null(unidad)) return(base)

  tipo <- .vh_unidad_map(unidad)$unidad_tipo[1]

  if (tipo == "pct")            return(paste0(scales::number(x_num, accuracy = accuracy), "%"))
  if (tipo == "pp")             return(paste0(scales::number(x_num, accuracy = accuracy), " pp"))
  if (tipo == "moneda")         return(paste0("S/ ", scales::number(x_num, accuracy = accuracy)))
  if (tipo == "centavos_kwh")   return(paste0(scales::number(x_num, accuracy = accuracy), " ctv/kWh"))
  if (tipo == "horas")          return(paste0(scales::number(x_num, accuracy = accuracy), " h"))
  # "numero" e "indice" y otros caen a number genérico
  base
}

# =========================
# Fuente
# =========================

#' Construir etiqueta de fuente global
#' @param df data.frame con columna `fuente`
#' @param preferir_ipe logical(1)
#' @param max_list integer(1)
#' @return character(1)
valor_fuente_caption <- function(df,
                                 preferir_ipe = TRUE,
                                 max_list = 3L) {
  if (!"fuente" %in% names(df)) {
    return(if (preferir_ipe) "Fuente: IPE (cálculos)" else "Fuente: No especificada")
  }
  f <- unique(df$fuente[!is.na(df$fuente) & df$fuente != ""])
  if (!length(f)) {
    return(if (preferir_ipe) "Fuente: IPE (cálculos)" else "Fuente: No especificada")
  }
  if (length(f) == 1L) return(paste0("Fuente: ", f))
  if (length(f) <= max_list) return(paste0("Fuentes: ", paste(f, collapse = "; ")))
  paste0("Fuentes: ", paste(f[seq_len(max_list)], collapse = "; "), ", …")
}

#' Construir texto de fuente por indicador (para facet captions)
#' Devuelve un named vector: `names = indicador`, `values = fuente condensada`.
#' @param df data.frame con columnas `indicador` y `fuente`
#' @param preferir_ipe logical(1)
#' @param max_list integer(1)
#' @return named character()
valor_fuente_por_indicador <- function(df,
                                       preferir_ipe = TRUE,
                                       max_list = 2L) {
  if (!all(c("indicador","fuente") %in% names(df))) return(character(0))
  agg <- df |>
    dplyr::group_by(.data$indicador) |>
    dplyr::summarise(fu = unique(.data$fuente[!is.na(.data$fuente) & .data$fuente != ""]), .groups = "drop") |>
    dplyr::mutate(
      txt = dplyr::case_when(
        lengths(fu) == 0L ~ if (preferir_ipe) "Fuente: IPE (cálculos)" else "Fuente: No espec.",
        lengths(fu) == 1L ~ paste0("Fuente: ", vapply(fu, `[`, character(1), 1)),
        lengths(fu) <= max_list ~ paste0("Fuentes: ", vapply(fu, function(x) paste(x, collapse = "; "), character(1))),
        TRUE ~ paste0("Fuentes: ", paste(head(unlist(fu), max_list), collapse = "; "), ", …")
      )
    )
  stats::setNames(agg$txt, agg$indicador)
}

# =========================
# Escalas para mapas/heatmaps
# =========================

#' Paleta continua estándar para valores (0–10 por defecto)
#'
#' Devuelve `scale_fill_gradientn()` configurada.
#'
#' @param paleta "viridis","cividis","magma","blues"
#' @param limits numeric(2) rango de la escala (default c(0,10))
#' @param na_value color para NA
#' @return objeto `ggplot2` Scale
valor_scale_fill_continua <- function(paleta = c("viridis","cividis","magma","blues"),
                                      limits = c(0, 10),
                                      na_value = "grey90") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Falta 'ggplot2' para construir la escala.")
  }
  paleta <- match.arg(paleta)

  pal_blues <- c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C")
  pal_virid <- c("#440154","#30678D","#35B778","#FDE725")
  pal_civid <- c("#00204D","#2C728E","#95D840","#FDE725")
  pal_magma <- c("#000004","#3B0F70","#8C2981","#DE4968","#FCA636","#FCFDBF")

  cols <- switch(
    paleta,
    "viridis" = grDevices::colorRampPalette(pal_virid)(256),
    "cividis" = grDevices::colorRampPalette(pal_civid)(256),
    "magma"   = grDevices::colorRampPalette(pal_magma)(256),
    "blues"   = pal_blues
  )

  ggplot2::scale_fill_gradientn(colors = cols, limits = limits, na.value = na_value)
}
