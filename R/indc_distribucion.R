#' Distribución de puntajes por indicador (una edición y un pilar)
#'
#' Para una **edición** y un **pilar** del INCORE, muestra la dispersión de
#' puntajes (0–10) entre regiones por **indicador** de ese pilar, como
#' **boxplot** o **violin**. Usa siempre `...agregar_codigos = TRUE` para que
#' el eje X sea el **código** del indicador (p.ej. LAB1, EDU3).
#'
#' - Eje X: códigos de indicador (LAB1, LAB2, …).
#' - Eje Y: puntaje (0–10).
#'
#' @param edicion integer(1). Año de edición (p. ej., 2025). Obligatorio.
#' @param pilar character(1). Código o nombre del pilar (p.ej. "LAB" o "Laboral"). Obligatorio.
#' @param indicadores character(). Vector de **códigos** o nombres a incluir, o "ALL" (default).
#' @param regiones character(). Vector de regiones a incluir; "ALL" = todas (default).
#' @param usar_codigos logical. Si TRUE, permite pasar códigos en pilar/regiones/indicadores. Default TRUE.
#' @param tipo "boxplot" (default) o "violin".
#' @param jitter logical. TRUE para superponer puntos individuales (alpha suave).
#' @param paleta "blues" (default), "viridis", "cividis". Se aplica **por indicador** (discreta).
#' @param linea_promedio logical. Dibuja una línea horizontal punteada con el promedio global. Default TRUE.
#' @param mostrar_leyenda logical. Mostrar/ocultar leyenda (default FALSE).
#'
#' @return Un objeto `ggplot2`.
#' @export
indc_distribucion <- function(edicion,
                              pilar,
                              indicadores = "ALL",
                              regiones = "ALL",
                              usar_codigos = TRUE,
                              tipo = c("boxplot", "violin"),
                              jitter = FALSE,
                              paleta = c("blues", "viridis", "cividis"),
                              linea_promedio = TRUE,
                              mostrar_leyenda = FALSE) {

  # --- Validaciones y args ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica y de longitud 1 (p. ej., 2025).")
  }
  if (missing(pilar) || length(pilar) != 1L) stop("'pilar' es obligatorio (código o nombre).")

  tipo   <- match.arg(tipo)
  paleta <- match.arg(paleta)
  edicion <- as.integer(edicion)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("forcats", quietly = TRUE)) {
    stop("Faltan paquetes: 'dplyr', 'stringr', 'forcats'.")
  }

  # Rango duro recomendado
  if (exists("indc_validar_rango")) indc_validar_rango(edicion = edicion)

  # --- Leer base con códigos agregados ---
  dat <- indc_leer_incore(
    edicion       = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  )
  dat <- indc_normalizar_columnas(dat)

  # Puntajes 0–10, excluir "Perú" y "General"
  dat <- dplyr::filter(
    dat,
    .data$unidad == "Puntaje del 0 al 10",
    .data$region != "Perú",
    .data$indicador != "General"
  )

  # --- Resolver pilar (acepta código o nombre) ---
  pil_res <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)
  # Filtramos por código si existe pilar_cod; si no, por nombre
  if ("pilar_cod" %in% names(dat) && any(dat$pilar_cod %in% pil_res)) {
    dat <- dplyr::filter(dat, .data$pilar_cod %in% !!pil_res)
    pilar_titulo <- unique(dat$pilar_cod)[1]
  } else {
    dat <- dplyr::filter(dat, .data$pilar %in% !!pil_res)
    pilar_titulo <- if ("pilar_cod" %in% names(dat) && length(unique(dat$pilar_cod)) == 1) {
      unique(dat$pilar_cod)[1]
    } else {
      pil_res[1]
    }
  }
  if (nrow(dat) == 0L) stop("No hay datos para el pilar '", pilar, "' en la edición ", edicion, ".")

  # --- Filtro de indicadores (opcional; acepta código o nombre) ---
  if (!identical(indicadores, "ALL")) {
    ind_in <- unique(stringr::str_squish(as.character(indicadores)))
    dat <- dplyr::filter(dat, .data$ind_cod %in% ind_in | .data$indicador %in% ind_in)
  }

  # --- Filtrar regiones (grupos/códigos) ---
  dat <- indc_filtrar_regiones(dat, regiones = regiones, usar_codigos = usar_codigos)
  if (nrow(dat) == 0L) stop("No hay datos tras los filtros de indicadores/regiones.")

  # --- Consolidar a un valor por (region, ind_cod) ---
  dat <- dat |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = c(.data$region, .data$ind_cod))

  # Asegurar que existan códigos de indicador
  if (all(is.na(dat$ind_cod)) || !nrow(dat)) stop("No se encontraron códigos de indicador (ind_cod).")

  # Ordenar indicadores por mediana (desc)
  ord <- dat |>
    dplyr::summarise(mediana = stats::median(.data$valor, na.rm = TRUE), .by = .data$ind_cod) |>
    dplyr::arrange(dplyr::desc(.data$mediana), .data$ind_cod) |>
    dplyr::pull(.data$ind_cod)
  dat$ind_cod <- forcats::fct_relevel(dat$ind_cod, ord)

  # Paleta discreta por indicador (derivada de rampas)
  make_disc <- function(n, key = "blues") {
    if (key == "blues")   return(grDevices::colorRampPalette(c("#F7FBFF","#DEEBF7","#9ECAE1","#3182BD","#08519C"))(n))
    if (key == "viridis") return(grDevices::colorRampPalette(c("#440154","#30678D","#35B778","#FDE725"))(n))
    if (key == "cividis") return(grDevices::colorRampPalette(c("#00204D","#2C728E","#95D840","#FDE725"))(n))
    grDevices::gray.colors(n)
  }
  ind_levels <- levels(dat$ind_cod)
  pal_vals   <- make_disc(length(ind_levels), paleta); names(pal_vals) <- ind_levels

  # Promedio global (línea punteada)
  prom <- mean(dat$valor, na.rm = TRUE)

  # Geometría base
  g_base <- if (tipo == "boxplot") {
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data$ind_cod),
                          color = "grey25", width = 0.6, outlier.alpha = 0.35)
  } else {
    ggplot2::geom_violin(ggplot2::aes(fill = .data$ind_cod),
                         color = "grey25", alpha = 0.9, width = 0.9, trim = TRUE)
  }

  # --- Plot ---
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$ind_cod, y = .data$valor)) +
    g_base +
    { if (isTRUE(jitter)) ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.5) else NULL } +
    { if (isTRUE(linea_promedio)) ggplot2::geom_hline(yintercept = prom, linetype = 2, color = "grey40") else NULL } +
    ggplot2::scale_fill_manual(values = pal_vals,
                               breaks = ind_levels,
                               guide  = if (isTRUE(mostrar_leyenda)) "legend" else "none") +
    ggplot2::scale_y_continuous(limits = c(0,10)) +
    ggplot2::labs(
      title    = paste0("INCORE | Pilar ", pilar_titulo, ")"),
      subtitle = paste0("Edición ", edicion),
      x = NULL, y = "Puntaje (0–10)", fill = "Indicador",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = if (isTRUE(mostrar_leyenda)) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin      = ggplot2::margin(8, 12, 8, 12)
    )

  # Sugerencias de legibilidad (no detiene el gráfico)
  n_reg <- dplyr::n_distinct(dat$region)
  if (n_reg < 5)  warning("Hay pocas regiones (", n_reg, "). La distribución puede no ser muy informativa.")
  if (n_reg > 26) warning("Hay muchas regiones (", n_reg, "). Puede afectar la legibilidad.")

  p
}
