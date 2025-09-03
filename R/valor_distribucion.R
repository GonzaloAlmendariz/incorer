#' Distribución de valores por indicador (una edición y un pilar)
#'
#' Para una **edición** y un **pilar**, muestra la dispersión de **valores originales**
#' entre regiones por **indicador** del pilar, como **boxplot** o **violin**.
#' El eje X usa el **código** del indicador (p.ej. LAB1, EDU3) si el catálogo está disponible.
#'
#' - Eje X: códigos de indicador (LAB1, LAB2, …).
#' - Eje Y: valor en su unidad original (no puntaje 0–10).
#'
#' @param edicion integer(1) Año de edición (p. ej., 2025).
#' @param pilar character(1) Código o nombre del pilar (p.ej. "LAB" o "Laboral").
#' @param indicadores "ALL" (default) o vector de **códigos**/nombres a incluir.
#' @param regiones "ALL" (default) o vector de regiones a incluir.
#' @param usar_codigos logical. Si TRUE, permite pasar códigos en pilar/regiones/indicadores.
#' @param tipo "boxplot" (default) o "violin".
#' @param jitter logical. TRUE para superponer puntos individuales (alpha suave).
#' @param paleta "blues" (default), "viridis", "cividis". Se aplica **por indicador** (discreta).
#' @param linea_promedio logical. Dibuja una línea horizontal punteada con el promedio global.
#' @param mostrar_leyenda logical. Mostrar/ocultar leyenda (default FALSE).
#' @param incluir_peru logical. Incluir la fila "Perú" (default FALSE).
#'
#' @return Un objeto `ggplot2`.
#' @export
valor_distribucion <- function(edicion,
                               pilar,
                               indicadores = "ALL",
                               regiones = "ALL",
                               usar_codigos = TRUE,
                               tipo = c("boxplot", "violin"),
                               jitter = FALSE,
                               paleta = c("blues", "viridis", "cividis"),
                               linea_promedio = TRUE,
                               mostrar_leyenda = FALSE,
                               incluir_peru = FALSE) {

  # --- Validaciones y args ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica y de longitud 1 (p. ej., 2025).")
  }
  if (missing(pilar) || length(pilar) != 1L) stop("'pilar' es obligatorio (código o nombre).")

  tipo   <- match.arg(tipo)
  paleta <- match.arg(paleta)
  edicion <- as.integer(edicion)

  req <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Falta '", p, "'.")
  req("ggplot2"); req("dplyr"); req("stringr"); req("forcats")

  # Rango duro recomendado (usa tus helpers de valor_)
  if (exists("valor_assert_rango_fechas")) valor_assert_rango_fechas(edicion = edicion)

  # --- Leer base de VALORES (no fija unidad) ---
  df <- valor_traer_base_valores(
    edicion      = edicion,
    pilar        = pilar,
    regiones     = regiones,
    usar_codigos = usar_codigos,
    unidad       = "ALL",
    verbose      = FALSE
  )

  # Normalizaciones mínimas y filtros base
  nm <- names(df)
  must <- c("region","pilar","indicador","valor")
  if (!all(must %in% nm)) stop("La base no tiene columnas mínimas: ", paste(must, collapse = ", "))

  df$region    <- stringr::str_squish(df$region)
  df$pilar     <- stringr::str_squish(df$pilar)
  df$indicador <- stringr::str_squish(df$indicador)
  df$valor     <- suppressWarnings(as.numeric(df$valor))

  if (!isTRUE(incluir_peru)) {
    df <- dplyr::filter(df, .data$region != "Perú")
  }
  df <- dplyr::filter(df, .data$indicador != "General")

  # Resolver pilar (nombre oficial si llegó código)
  pil_nom <- if (exists("valor_resolver_pilar")) {
    valor_resolver_pilar(pilar, usar_codigos = usar_codigos)
  } else pilar
  df <- dplyr::filter(df, .data$pilar == !!pil_nom)
  if (nrow(df) == 0L) stop("No hay datos para el pilar '", pilar, "' en la edición ", edicion, ".")

  # --- Mapear a CÓDIGO de indicador (si hay catálogo) ---
  ind_code <- "ind_code"
  if (!ind_code %in% names(df) || all(is.na(df[[ind_code]]))) {
    dic_ind <- try(catalogo_indicador(), silent = TRUE)
    if (!inherits(dic_ind, "try-error") && is.data.frame(dic_ind)) {
      # aceptar code/codigo y name/nombre
      ccol <- intersect(c("codigo","code"), names(dic_ind))[1]
      ncol <- intersect(c("nombre","name"), names(dic_ind))[1]
      if (!is.na(ccol) && !is.na(ncol)) {
        m <- dic_ind[, c(ccol, ncol)]
        names(m) <- c("code","name")
        df <- dplyr::left_join(
          df,
          dplyr::select(m, ind_code = .data$code, indicador = .data$name),
          by = "indicador"
        )
      }
    }
    # fallback: si no hay catálogo, usa el propio nombre como “código”
    if (!"ind_code" %in% names(df)) df$ind_code <- df$indicador
  }

  # --- Filtro de indicadores (opcional; acepta código o nombre) ---
  if (!identical(indicadores, "ALL")) {
    ind_in <- unique(stringr::str_squish(as.character(indicadores)))
    df <- dplyr::filter(df, .data$ind_code %in% ind_in | .data$indicador %in% ind_in)
  }
  if (nrow(df) == 0L) stop("No hay datos tras los filtros de indicadores/regiones.")

  # --- Consolidar a un valor por (region, ind_code) ---
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     .by = c(.data$region, .data$ind_code))

  # Ordenar indicadores por mediana (desc)
  ord <- df |>
    dplyr::summarise(mediana = stats::median(.data$valor, na.rm = TRUE), .by = .data$ind_code) |>
    dplyr::arrange(dplyr::desc(.data$mediana), .data$ind_code) |>
    dplyr::pull(.data$ind_code)
  df$ind_code <- forcats::fct_relevel(df$ind_code, ord)

  # Paleta discreta por indicador
  make_disc <- function(n, key = "blues") {
    if (key == "blues")   return(grDevices::colorRampPalette(c("#F7FBFF","#DEEBF7","#9ECAE1","#3182BD","#08519C"))(n))
    if (key == "viridis") return(grDevices::colorRampPalette(c("#440154","#30678D","#35B778","#FDE725"))(n))
    if (key == "cividis") return(grDevices::colorRampPalette(c("#00204D","#2C728E","#95D840","#FDE725"))(n))
    grDevices::gray.colors(n)
  }
  ind_levels <- levels(df$ind_code)
  pal_vals   <- make_disc(length(ind_levels), paleta); names(pal_vals) <- ind_levels

  # Promedio global (línea punteada)
  prom <- mean(df$valor, na.rm = TRUE)

  # Unidad (si es única, úsala en subtítulo)
  unidad_txt <- tryCatch({
    if ("unidad" %in% names(df)) {
      u <- unique(na.omit(df$unidad))
      if (length(u) == 1) u else NULL
    } else NULL
  }, error = function(e) NULL)

  # Geometría base
  g_base <- if (tipo == "boxplot") {
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data$ind_code),
                          color = "grey25", width = 0.6, outlier.alpha = 0.35)
  } else {
    ggplot2::geom_violin(ggplot2::aes(fill = .data$ind_code),
                         color = "grey25", alpha = 0.9, width = 0.9, trim = TRUE)
  }

  # --- Plot ---
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$ind_code, y = .data$valor)) +
    g_base +
    { if (isTRUE(jitter)) ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.5) else NULL } +
    { if (isTRUE(linea_promedio)) ggplot2::geom_hline(yintercept = prom, linetype = 2, color = "grey40") else NULL } +
    ggplot2::scale_fill_manual(values = pal_vals,
                               breaks = ind_levels,
                               guide  = if (isTRUE(mostrar_leyenda)) "legend" else "none") +
    ggplot2::labs(
      title    = paste0("INCORE — Distribución de valores (Pilar ", pil_nom, ")"),
      subtitle = paste0("Edición ", edicion, if (!is.null(unidad_txt)) paste0(" · Unidad: ", unidad_txt) else ""),
      x = NULL, y = NULL, fill = "Indicador",
      caption  = valor_fuente_caption(df)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = if (isTRUE(mostrar_leyenda)) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin      = ggplot2::margin(8, 12, 8, 12)
    )

  # Avisos de legibilidad
  n_reg <- dplyr::n_distinct(df$region)
  if (n_reg < 5)  warning("Hay pocas regiones (", n_reg, "). La distribución puede no ser muy informativa.")
  if (n_reg > 26) warning("Hay muchas regiones (", n_reg, "). Puede afectar la legibilidad.")

  p
}
