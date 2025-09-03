#' Dispersión del puntaje General (total o de un pilar) vs promedio
#'
#' Genera un gráfico de dispersión del puntaje **General** (0–10) por región en
#' **una edición**, comparándolo con un **promedio**. Por defecto, el promedio se
#' calcula sobre las **regiones seleccionadas** (tras aplicar filtros). Opcionalmente,
#' puede mostrarse el **promedio nacional** (todas las regiones).
#'
#' @param edicion integer(1). Edición a graficar (obligatorio).
#' @param pilar NULL (default) para índice general total, o nombre/código del pilar
#'   (p. ej., "SAL", "Salud") para el **General** de ese pilar.
#' @param regiones character(). Vector de regiones o grupos (p. ej. "gr_costa").
#'   Use "ALL" para todas.
#' @param usar_codigos logical. Si TRUE, traduce códigos de regiones/pilar antes de filtrar.
#' @param promedio_nacional logical. Si TRUE, la línea y el cálculo del promedio usan
#'   **todas** las regiones (nacional); si FALSE (default), usan **solo** las regiones
#'   filtradas (selección actual).
#' @param esquema "binario","divergente","semaforo","region","monocromo".
#' @param color_arriba,color_abajo Colores para "binario" (>= prom / < prom).
#' @param paleta_region "ipe","okabe_ito","viridis" (cuando esquema = "region").
#' @param color_mono color para "monocromo".
#' @param mostrar_leyenda logical. Mostrar leyenda (por defecto FALSE).
#'
#' @return ggplot
#' @export
general_media <- function(edicion,
                          pilar = NULL,
                          regiones = "ALL",
                          usar_codigos = TRUE,
                          promedio_nacional = FALSE,
                          esquema = c("binario","divergente","semaforo","region","monocromo"),
                          color_arriba = "#2C7FB8",
                          color_abajo  = "#D7301F",
                          paleta_region = c("ipe","okabe_ito","viridis"),
                          color_mono = "#3B5B92",
                          mostrar_leyenda = FALSE) {

  # --- Validaciones básicas y args ---
  stopifnot(length(edicion) == 1, is.numeric(edicion))
  edicion <- as.integer(edicion)
  esquema <- match.arg(esquema)
  paleta_region <- match.arg(paleta_region)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Necesitas 'ggplot2'.")

  # Rango duro INCORE (defensivo)
  .RANGO_INCORE_MIN <- 2016L
  .RANGO_INCORE_MAX <- 2025L
  if (edicion < .RANGO_INCORE_MIN || edicion > .RANGO_INCORE_MAX) {
    stop(sprintf("'edicion' fuera de rango INCORE [%d–%d].", .RANGO_INCORE_MIN, .RANGO_INCORE_MAX))
  }

  # --- Leer base de esa edición (una sola vez) ---
  base_all <- leer_incore(edicion = edicion, usar_codigos = usar_codigos, verbose = FALSE) |>
    dplyr::mutate(
      region    = stringr::str_squish(.data$region),
      pilar     = stringr::str_squish(.data$pilar),
      indicador = stringr::str_squish(.data$indicador)
    ) |>
    dplyr::filter(.data$region != "Perú",
                  .data$unidad == "Puntaje del 0 al 10")

# --- Filtrado por pilar (NULL = índice general total) ---
if (is.null(pilar)) {
  # Índice General total: pilar que empieza con "Índice de Competitividad Regional"
  df_all <- dplyr::filter(base_all, stringr::str_starts(.data$pilar, "Índice de Competitividad Regional"))
  titulo <- paste0("Índice de Competitividad Regional — ", edicion)
  subt_base <- "Puntaje general"
} else {
  pilar_obj <- pilar
  if (isTRUE(usar_codigos)) {
    tr <- try(suppressWarnings(traducir_codigo(pilar_obj, catalogo_pilar())), silent = TRUE)
    if (!inherits(tr, "try-error") && length(tr) == 1L && !is.na(tr)) pilar_obj <- tr
  }
  df_all <- dplyr::filter(base_all, .data$pilar == !!pilar_obj, .data$indicador == "General")
  if (nrow(df_all) == 0L) stop("No hay filas para el pilar especificado. Revisa 'pilar' (nombre o código).")
  titulo <- paste0("Índice de Competitividad Regional | ", edicion)
  subt_base <- paste0("Puntaje general del pilar: ", pilar_obj)
}

# --- Filtro de regiones con grupos/exclusiones/códigos (para la SELECCIÓN a graficar) ---
if (!identical(regiones, "ALL")) {
  regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
  if (!length(regiones_filtrar)) {
    stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
  }
  df_sel <- dplyr::filter(df_all, .data$region %in% regiones_filtrar)
} else {
  df_sel <- df_all
}

# Consolidar a un valor por región (selección)
df_sel <- df_sel |>
  dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region)

if (nrow(df_sel) == 0L) stop("No hay datos para graficar tras los filtros.")

# --- Promedio (selección vs nacional) ---
prom <- if (isTRUE(promedio_nacional)) {
  # promedio sobre TODAS las regiones disponibles en df_all (nacional)
  mean(dplyr::summarise(df_all, valor = mean(.data$valor, na.rm = TRUE), .by = .data$region)$valor, na.rm = TRUE)
} else {
  # promedio sobre la selección filtrada (default)
  mean(df_sel$valor, na.rm = TRUE)
}
prom_label <- if (isTRUE(promedio_nacional)) "Promedio nacional" else "Promedio (selección)"

# Preparaciones
df <- df_sel |>
  dplyr::arrange(dplyr::desc(.data$valor), .data$region) |>
  dplyr::mutate(
    region_fac = forcats::fct_reorder(.data$region, .data$valor),
    dif        = .data$valor - prom,
    etiqueta   = scales::number(.data$valor, accuracy = 0.01)
  )

xr    <- range(c(df$valor, prom), na.rm = TRUE)
nudge <- diff(xr) * 0.03 + 0.03
df <- df |>
  dplyr::mutate(
    x_label = dplyr::if_else(.data$valor < prom, .data$valor - nudge, .data$valor + nudge),
    hjust_l = dplyr::if_else(.data$valor < prom, 1, 0)
  )

# --- Paleta para esquema = "region" ---
build_palette <- function(n, which = "ipe") {
  which <- match.arg(which, c("ipe","okabe_ito","viridis"))
  if (which == "ipe") {
    base_cols <- c(
      "#355C7D","#6C5B7B","#C06C84","#F67280","#F8B195",
      "#4575B4","#74ADD1","#ABD9E9","#E0F3F8","#FEE090",
      "#FDAE61","#F46D43","#D73027","#66BD63","#1A9850",
      "#006837","#8C510A","#BF812D","#DFC27D","#80CDC1",
      "#018571","#35978F","#A6CEE3","#1F78B4","#B2DF8A","#33A02C"
    )
    if (n <= length(base_cols)) base_cols[seq_len(n)] else grDevices::colorRampPalette(base_cols)(n)
  } else if (which == "okabe_ito") {
    oi <- c("#000000","#E69F00","#56B4E9","#009E73",
            "#F0E442","#0072B2","#D55E00","#CC79A7")
    if (n <= length(oi)) oi[seq_len(n)] else grDevices::colorRampPalette(oi)(n)
  } else {
    anchors <- c("#440154","#3B528B","#21918C","#5DC863","#FDE725")
    grDevices::colorRampPalette(anchors)(n)
  }
}

legend_setting <- if (isTRUE(mostrar_leyenda)) "right" else "none"
mapping_color <- NULL
scale_color   <- NULL
seg_color     <- NULL
point_color   <- NULL
text_color    <- "grey15"

if (esquema == "binario") {
  df <- dplyr::mutate(df,
                      grupo = dplyr::if_else(.data$valor < prom,
                                             "Por debajo del promedio",
                                             "Sobre/igual al promedio"))
  mapping_color <- ggplot2::aes(color = .data$grupo)
  scale_color <- ggplot2::scale_color_manual(
    values = c("Por debajo del promedio" = color_abajo,
               "Sobre/igual al promedio" = color_arriba),
    guide  = if (mostrar_leyenda) "legend" else "none"
  )

} else if (esquema == "divergente") {
  mapping_color <- ggplot2::aes(color = .data$dif)
  lim <- max(abs(range(df$dif, na.rm = TRUE)))
  scale_color <- ggplot2::scale_color_gradient2(
    low = color_abajo, mid = "grey70", high = color_arriba,
    midpoint = 0, limits = c(-lim, lim),
    guide = if (mostrar_leyenda) ggplot2::guide_colorbar(title = "Δ vs prom.") else "none"
  )

} else if (esquema == "semaforo") {
  tol <- 0.25
  df <- dplyr::mutate(
    df,
    banda = dplyr::case_when(
      .data$dif < -tol ~ "Bajo",
      .data$dif >  tol ~ "Alto",
      TRUE            ~ "En torno al prom."
    )
  )
  mapping_color <- ggplot2::aes(color = .data$banda)
  scale_color <- ggplot2::scale_color_manual(
    values = c("Bajo" = color_abajo, "En torno al prom." = "grey55", "Alto" = color_arriba),
    guide  = if (mostrar_leyenda) "legend" else "none"
  )

} else if (esquema == "region") {
  levs <- levels(df$region_fac)
  pal  <- build_palette(length(levs), which = paleta_region); names(pal) <- levs
  mapping_color <- ggplot2::aes(color = .data$region_fac)
  scale_color <- ggplot2::scale_color_manual(values = pal, guide = if (mostrar_leyenda) "legend" else "none")

} else if (esquema == "monocromo") {
  if (isTRUE(mostrar_leyenda)) {
    warning("La leyenda no aplica al esquema 'monocromo'; se ocultará.")
    legend_setting <- "none"
  }
  seg_color   <- color_mono
  point_color <- color_mono
}

# --- Plot ---
p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$valor, y = .data$region_fac)) +
  ggplot2::geom_vline(xintercept = prom, linetype = 2, linewidth = 0.7, color = "grey40") +
  {
    if (!is.null(mapping_color)) {
      ggplot2::geom_segment(
        ggplot2::aes(x = prom, xend = .data$valor, y = .data$region_fac, yend = .data$region_fac,
                     color = !!rlang::get_expr(mapping_color$colour)),
        linewidth = 0.7, alpha = 0.75
      )
    } else {
      ggplot2::geom_segment(
        ggplot2::aes(x = prom, xend = .data$valor, y = .data$region_fac, yend = .data$region_fac),
        color = seg_color, linewidth = 0.7, alpha = 0.75
      )
    }
  } +
  {
    if (!is.null(mapping_color)) {
      ggplot2::geom_point(mapping_color, size = 3)
    } else {
      ggplot2::geom_point(size = 3, color = point_color)
    }
  } +
  ggplot2::geom_text(
    ggplot2::aes(x = .data$x_label, y = .data$region_fac,
                 label = .data$etiqueta, hjust = .data$hjust_l),
    size = 3, color = text_color
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 10),
                              expand = ggplot2::expansion(mult = c(0.02, 0.08))) +
  { if (!is.null(scale_color)) scale_color else NULL } +
  ggplot2::labs(
    title    = titulo,
    subtitle = paste0(subt_base, " | ", prom_label, ": ",
                      scales::number(prom, accuracy = 0.01)),
    x = "Puntaje (0–10)", y = NULL, color = NULL,
    caption = "Fuente: Instituto Peruano de Economía (IPE)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = legend_setting,
    panel.grid.minor = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.margin = ggplot2::margin(10, 18, 10, 10)
  )

p
}
