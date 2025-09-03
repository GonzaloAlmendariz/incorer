#' Dispersión por indicador vs promedio nacional (edición única)
#'
#' Genera un gráfico de dispersión del puntaje (0–10) de un **indicador**
#' mostrando cada región frente al **promedio nacional** de ese indicador.
#' Acepta código (p.ej. "EDU2") o nombre de indicador.
#'
#' Siempre excluye `region == "Perú"` e `indicador == "General"`, y usa
#' `leer_incore(agregar_codigos = TRUE)` para trabajar con códigos.
#'
#' @param edicion integer(1). Edición a graficar (obligatorio).
#' @param indicador character(1). Código (p.ej. "EDU2") o nombre del indicador (obligatorio).
#' @param regiones character(). Vector de regiones a incluir. Use `"ALL"` para todas.
#' @param usar_codigos logical. Si TRUE, traduce códigos de regiones a nombres.
#' @param esquema "binario","divergente","semaforo","region","monocromo".
#' @param color_arriba,color_abajo Colores para `esquema="binario"`.
#' @param paleta_region "ipe","okabe_ito","viridis" (cuando `esquema="region"`).
#' @param color_mono color base para `esquema="monocromo"`.
#' @param mostrar_leyenda logical. Mostrar leyenda (default FALSE).
#' @param subtitulo_nombre logical. Si TRUE (default) el subtítulo usa el **nombre completo**
#'   del indicador (vía `catalogo_indicador()`); si FALSE, usa el código.
#'
#' @return Un objeto `ggplot2`.
#' @export
indc_media <- function(edicion,
                       indicador,
                       regiones = "ALL",
                       usar_codigos = TRUE,
                       esquema = c("binario","divergente","semaforo","region","monocromo"),
                       color_arriba = "#2C7FB8",
                       color_abajo  = "#D7301F",
                       paleta_region = c("ipe","okabe_ito","viridis"),
                       color_mono = "#3B5B92",
                       mostrar_leyenda = FALSE,
                       subtitulo_nombre = TRUE) {

  # --- Validaciones básicas ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica (p.ej., 2024).")
  }
  if (missing(indicador) || !is.character(indicador) || length(indicador) != 1L) {
    stop("'indicador' es obligatorio (código p.ej. 'EDU2' o nombre).")
  }
  indc_validar_rango(edicion = edicion)

  edicion <- as.integer(edicion)
  esquema <- match.arg(esquema)
  paleta_region <- match.arg(paleta_region)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Necesitas 'ggplot2'.")

  # --- Lectura y normalización (con códigos) ---
  df0 <- indc_leer_incore(
    edicion         = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  ) |>
    indc_normalizar_columnas() |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$region != "Perú",
                  .data$indicador != "General")

  # --- Resolver indicador (acepta código o nombre) ---
  ind_res <- indc_resolver_indicador(indicador)
  ind_cod <- ind_res$codigo
  ind_nom <- ind_res$nombre

  # Preferimos filtrar por código si `ind_cod` existe en la base
  if ("ind_cod" %in% names(df0) && any(df0$ind_cod == ind_cod, na.rm = TRUE)) {
    df <- dplyr::filter(df0, .data$ind_cod == !!ind_cod)
  } else {
    # caer por nombre completo
    df <- dplyr::filter(df0, .data$indicador %in% c(ind_nom, ind_cod))
  }

  if (nrow(df) == 0L) stop("No hay datos para el indicador '", indicador, "' en ", edicion, ".")

  # --- Filtrar regiones (grupos + exclusiones + códigos) ---
  df <- indc_filtrar_regiones(df, regiones = regiones, usar_codigos = usar_codigos)
  if (nrow(df) == 0L) stop("Sin datos tras filtrar regiones.")

  # --- Consolidar a UN valor por región ---
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region)

  prom <- mean(df$valor, na.rm = TRUE)

  # --- Orden y auxiliares para etiquetas ---
  df <- df |>
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

  # --- Coloreo según esquema ---
  legend_setting <- if (isTRUE(mostrar_leyenda)) "right" else "none"
  mapping_color <- NULL
  scale_color   <- NULL
  seg_color     <- NULL
  point_color   <- NULL
  text_color    <- "grey15"

  if (esquema == "binario") {
    df <- df |>
      dplyr::mutate(grupo = dplyr::if_else(.data$valor < prom,
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
      guide = if (mostrar_leyenda) ggplot2::guide_colorbar(title = "Δ vs promedio") else "none"
    )

  } else if (esquema == "semaforo") {
    tol <- 0.25
    df <- df |>
      dplyr::mutate(
        banda = dplyr::case_when(
          .data$dif < -tol ~ "Bajo",
          .data$dif >  tol ~ "Alto",
          TRUE            ~ "En torno al promedio"
        )
      )
    mapping_color <- ggplot2::aes(color = .data$banda)
    scale_color <- ggplot2::scale_color_manual(
      values = c("Bajo" = color_abajo, "En torno al promedio" = "grey55", "Alto" = color_arriba),
      guide  = if (mostrar_leyenda) "legend" else "none"
    )

  } else if (esquema == "region") {
    levs <- levels(df$region_fac)
    pal  <- indc_paleta_discreta(length(levs), which = paleta_region)
    names(pal) <- levs
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

  # --- Subtítulo: nombre completo o código ---
  ind_label <- if (isTRUE(subtitulo_nombre) && !is.na(ind_nom) && nzchar(ind_nom)) ind_nom else ind_cod
  subtitle_text <- paste0("Indicador: ", ind_label, " (", edicion, ") — Promedio nacional: ",
                          scales::number(prom, accuracy = 0.01))

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
      title    = "Índice de Competitividad Regional",
      subtitle = subtitle_text,
      x = "Puntaje (0–10)", y = NULL, color = NULL,
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
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
