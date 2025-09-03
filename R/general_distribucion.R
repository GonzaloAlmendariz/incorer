#' Distribución de puntajes (General o por pilares) en una edición
#'
#' Para una **edición** del INCORE, muestra la dispersión de puntajes entre
#' regiones como **boxplot** o **violin**. Puede graficar el **índice general**
#' (un solo eje, código **GEN**) o **los seis pilares** (códigos **ECO/LAB/INF/SAL/EDU/INS**).
#'
#' Siempre usa `leer_incore(..., agregar_codigos = TRUE)` y solo filas con
#' `Unidad == "Puntaje del 0 al 10"`.
#'
#' @param edicion Entero de longitud 1 (p. ej., 2025). **Obligatorio**.
#' @param modo `"general"` (un solo eje con el índice general) o `"pilares"`
#'   (un eje por cada pilar). Por defecto `"pilares"`.
#' @param regiones Vector de regiones a incluir (códigos, nombres o grupos como `"gr_costa"`).
#'   Use `"ALL"` para todas.
#' @param usar_codigos Si `TRUE`, traduce códigos de regiones a nombres oficiales.
#' @param tipo `"boxplot"` (default) o `"violin"`.
#' @param jitter `TRUE/FALSE` para superponer puntos individuales (alpha suave).
#' @param paleta Paleta de relleno: `"blues"` (default), `"viridis"`, `"cividis"`.
#'   En `modo="pilares"` se aplica por **pilar**; en `modo="general"` el relleno es fijo.
#' @param linea_promedio `TRUE/FALSE`. Dibuja línea horizontal punteada del promedio nacional.
#' @param mostrar_leyenda `TRUE/FALSE`. Por defecto `FALSE`.
#'
#' @return Un objeto `ggplot2`.
#' @export
general_distribucion <- function(edicion,
                                 modo = c("pilares", "general"),
                                 regiones = "ALL",
                                 usar_codigos = TRUE,
                                 tipo = c("boxplot", "violin"),
                                 jitter = FALSE,
                                 paleta = c("blues", "viridis", "cividis"),
                                 linea_promedio = TRUE,
                                 mostrar_leyenda = FALSE) {

  # --- Validaciones ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser un número de longitud 1 (p. ej., 2025).")
  }
  edicion <- as.integer(edicion)
  modo    <- match.arg(modo)
  tipo    <- match.arg(tipo)
  paleta  <- match.arg(paleta)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("forcats", quietly = TRUE)) {
    stop("Faltan paquetes: 'dplyr', 'tidyr', 'stringr', 'forcats'.")
  }

  # --- Leer datos (SIEMPRE con códigos agregados) ---
  dat <- leer_incore(
    edicion         = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  ) |>
    dplyr::mutate(
      region    = stringr::str_squish(.data$region),
      pilar     = stringr::str_squish(.data$pilar),
      indicador = stringr::str_squish(.data$indicador)
    )

  # Filtrar regiones (grupos + exclusiones + códigos)
  if (!identical(regiones, "ALL")) {
    regiones_filtrar <- resolver_regiones_grupos(regiones, usar_codigos = usar_codigos)
    if (!length(regiones_filtrar)) {
      stop("Después de expandir grupos/traducir códigos, no quedaron regiones para filtrar.")
    }
    dat <- dplyr::filter(dat, .data$region %in% regiones_filtrar)
  }

  # Solo puntajes
  dat <- dplyr::filter(dat, .data$unidad == "Puntaje del 0 al 10")

  # Advertencias de legibilidad por tamaño de muestra
  n_reg <- dplyr::n_distinct(dat$region)
  if (n_reg < 5)  warning("Hay pocas regiones (", n_reg, "). El box/violin puede no ser muy informativo.")
  if (n_reg > 26) warning("Hay muchas regiones (", n_reg, "). Puede afectar la legibilidad.")

  # --- helpers de paletas discretas ---
  make_disc <- function(n, key = "blues") {
    if (key == "blues")   return(grDevices::colorRampPalette(c("#F7FBFF","#DEEBF7","#9ECAE1","#3182BD","#08519C"))(n))
    if (key == "viridis") return(grDevices::colorRampPalette(c("#440154","#30678D","#35B778","#FDE725"))(n))
    if (key == "cividis") return(grDevices::colorRampPalette(c("#00204D","#2C728E","#95D840","#FDE725"))(n))
    grDevices::gray.colors(n)
  }

  if (modo == "general") {
    # Índice general: pilar == "Índice de Competitividad Regional ..."
    dat_g <- dat |>
      dplyr::filter(stringr::str_starts(.data$pilar, "Índice de Competitividad Regional")) |>
      dplyr::filter(.data$region != "Perú") |>
      dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region)

    if (nrow(dat_g) == 0L) stop("No hay datos de índice general para graficar.")

    prom <- mean(dat_g$valor, na.rm = TRUE)

    g_base <- if (tipo == "boxplot") {
      ggplot2::geom_boxplot(fill = "#4C78A8", color = "grey25", width = 0.25, outlier.alpha = 0.35)
    } else {
      ggplot2::geom_violin(fill = "#4C78A8", color = "grey25", alpha = 0.85, width = 0.7, trim = TRUE)
    }

    p <- ggplot2::ggplot(dat_g, ggplot2::aes(x = factor("GEN", levels = "GEN"), y = .data$valor)) +
      g_base +
      { if (isTRUE(jitter)) ggplot2::geom_jitter(width = 0.08, height = 0, alpha = 0.35, size = 1.6) else NULL } +
      { if (isTRUE(linea_promedio)) ggplot2::geom_hline(yintercept = prom, linetype = 2, color = "grey40") else NULL } +
      ggplot2::scale_y_continuous(limits = c(0,10)) +
      ggplot2::labs(
        title    = "Distribución de puntajes — Índice General",
        subtitle = paste0("Edición ", edicion),
        x = NULL, y = "Puntaje (0–10)",
        caption = "Fuente: Instituto Peruano de Economía (IPE)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.minor = ggplot2::element_blank(),
        plot.title.position = "plot"
      )
    return(p)
  }

  # --- modo == "pilares": usar SIEMPRE códigos del pilar (pilar_cod) ---
  pilares_ok <- c("ECO","LAB","INF","SAL","EDU","INS")  # códigos
  dat_p <- dat |>
    dplyr::filter(.data$indicador == "General",
                  .data$region != "Perú") |>
    dplyr::mutate(pilar_cod = dplyr::coalesce(.data$pilar_cod, .data$pilar)) |>
    dplyr::filter(.data$pilar_cod %in% pilares_ok)

  if (nrow(dat_p) == 0L) stop("No hay datos de pilares (indicador == 'General') para graficar.")

  # Ordenar pilares por mediana (desc)
  ord <- dat_p |>
    dplyr::summarise(med = stats::median(.data$valor, na.rm = TRUE), .by = .data$pilar_cod) |>
    dplyr::arrange(dplyr::desc(.data$med)) |>
    dplyr::pull(.data$pilar_cod)
  dat_p <- dplyr::mutate(dat_p, pilar_cod = forcats::fct_relevel(.data$pilar_cod, ord))

  prom <- mean(dat_p$valor, na.rm = TRUE)
  pal_vals <- make_disc(length(levels(dat_p$pilar_cod)), paleta)
  names(pal_vals) <- levels(dat_p$pilar_cod)

  g_base <- if (tipo == "boxplot") {
    ggplot2::geom_boxplot(ggplot2::aes(fill = .data$pilar_cod),
                          color = "grey25", width = 0.6, outlier.alpha = 0.3)
  } else {
    ggplot2::geom_violin(ggplot2::aes(fill = .data$pilar_cod),
                         color = "grey25", alpha = 0.9, width = 0.9, trim = TRUE)
  }

  p <- ggplot2::ggplot(dat_p, ggplot2::aes(x = .data$pilar_cod, y = .data$valor)) +
    g_base +
    { if (isTRUE(jitter)) ggplot2::geom_jitter(width = 0.12, height = 0, alpha = 0.35, size = 1.5) else NULL } +
    { if (isTRUE(linea_promedio)) ggplot2::geom_hline(yintercept = prom, linetype = 2, color = "grey40") else NULL } +
    ggplot2::scale_fill_manual(values = pal_vals, guide = if (mostrar_leyenda) "legend" else "none") +
    ggplot2::scale_y_continuous(limits = c(0,10)) +
    ggplot2::labs(
      title    = "Índice de Competitividad Regional",
      subtitle = paste0("Edición ", edicion),
      x = "Pilar", y = "Puntaje (0–10)", fill = "Pilar",
      caption = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = if (mostrar_leyenda) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot"
    )

  p
}
