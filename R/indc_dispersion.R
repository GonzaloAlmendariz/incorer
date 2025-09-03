#' Dispersión de un conjunto de indicadores dentro de un pilar (una edición)
#'
#' Para una edición y un pilar, muestra un diagrama de dispersión donde el eje X
#' son los CÓDIGOS de indicadores (p.ej. LAB2, LAB5, …) y cada punto es el
#' puntaje (0–10) de una región en ese indicador. Siempre filtra a
#' Unidad == "Puntaje del 0 al 10" y excluye indicador == "General".
#'
#' @param edicion integer(1)
#' @param pilar character(1) Código o nombre del pilar (p.ej. "LAB" o "Laboral")
#' @param regiones "ALL" o vector de regiones
#' @param usar_codigos logical
#' @param paleta "ipe","okabe_ito","viridis"
#' @param mostrar_promedio logical (punto rombo por indicador)
#' @param promedio_shape, promedio_size, promedio_color, promedio_fill forma del promedio
#' @param jitter_width, jitter_height dispersión
#' @param mostrar_leyenda logical (leyenda de colores por región)
#' @return ggplot
#' @export
indc_dispersion <- function(
    edicion,
    pilar,
    regiones = "ALL",
    usar_codigos = TRUE,
    paleta = c("ipe","okabe_ito","viridis"),
    mostrar_promedio = TRUE,
    promedio_shape = 23,
    promedio_size  = 3.5,
    promedio_color = "black",
    promedio_fill  = "white",
    jitter_width = 0.08,
    jitter_height = 0.0,
    mostrar_leyenda = TRUE
) {
  stopifnot(length(edicion) == 1, is.numeric(edicion))
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Necesitas 'ggplot2'.")

  # Validación dura del rango
  indc_validar_rango(edicion = edicion)

  # ------------------ leer base (con códigos) ------------------
  # Usa tu alias interno si existe; si no, cae a leer_incore
  if (exists("indc_leer_incore", mode = "function")) {
    dat <- indc_leer_incore(edicion, usar_codigos = usar_codigos, agregar_codigos = TRUE, verbose = FALSE)
  } else {
    dat <- leer_incore(edicion = edicion, usar_codigos = usar_codigos, agregar_codigos = TRUE, verbose = FALSE)
  }

  # Normalizar columnas clave
  dat <- indc_normalizar_columnas(dat)

  # Solo puntajes y exclusiones base
  dat <- dat |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$region != "Perú",
                  .data$indicador != "General")

  # ------------------ resolver pilar (nombre/código) ------------------
  # indc_resolver_pilar devuelve el NOMBRE oficial si pasas un código.
  pil_nom <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)
  # intentamos también obtener el código correspondiente al nombre (si hay catálogo)
  pil_cod_from_nom <- NA_character_
  if (exists("catalogo_pilar", mode = "function")) {
    dic_pil <- try(catalogo_pilar(), silent = TRUE)
    if (!inherits(dic_pil, "try-error") && nrow(dic_pil)) {
      # columnas flexibles
      col_cod <- intersect(c("codigo","cod","code"), names(dic_pil))
      col_nom <- if ("nombre" %in% names(dic_pil)) "nombre" else intersect(c("pilar","pilar_nombre"), names(dic_pil))
      if (length(col_cod) && length(col_nom)) {
        pil_cod_from_nom <- dic_pil[[col_cod[1]]][match(pil_nom, dic_pil[[col_nom[1]]])]
      }
    }
  }
  # filtrar por nombre o por código (robusto a distintas estructuras)
  dat <- dplyr::filter(
    dat,
    .data$pilar == !!pil_nom |
      (!is.na(.data$pilar_cod) & .data$pilar_cod %in% c(!!pilar, !!pil_cod_from_nom))
  )
  if (nrow(dat) == 0L) stop("No hay datos para el pilar '", pilar, "' en la edición ", edicion, ".")

  # ------------------ filtro de regiones (opcional) ------------------
  dat <- indc_filtrar_regiones(dat, regiones = regiones, usar_codigos = usar_codigos)
  if (nrow(dat) == 0L) stop("No hay datos tras filtrar regiones.")

  # ------------------ consolidar y ordenar eje X (ind_cod) -----------
  # Asegurar un valor por (region, ind_cod)
  dat <- dat |>
    dplyr::mutate(ind_cod = dplyr::coalesce(.data$ind_cod, .data$indicador)) |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     .by = c(.data$region, .data$ind_cod))

  if (all(is.na(dat$ind_cod))) stop("No se encontraron códigos de indicador (ind_cod).")

  dat <- dat |>
    dplyr::arrange(.data$ind_cod) |>
    dplyr::mutate(ind_cod = factor(.data$ind_cod, levels = unique(.data$ind_cod)))

  # Paleta por región
  reg_levels <- sort(unique(dat$region))
  pal_vals <- indc_paleta_discreta(length(reg_levels), which = paleta)
  names(pal_vals) <- reg_levels

  # Promedio por indicador (sobre regiones)
  prom <- dat |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$ind_cod)

  # ------------------ plot ------------------
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$ind_cod, y = .data$valor, color = .data$region)) +
    ggplot2::geom_jitter(width = jitter_width, height = jitter_height, size = 2.6, alpha = 0.9,
                         show.legend = mostrar_leyenda) +
    {
      if (isTRUE(mostrar_promedio)) {
        ggplot2::geom_point(
          data = prom,
          mapping = ggplot2::aes(x = .data$ind_cod, y = .data$valor),
          inherit.aes = FALSE,
          shape = promedio_shape, size = promedio_size,
          color = promedio_color, fill = promedio_fill, stroke = 0.6
        )
      } else NULL
    } +
    ggplot2::scale_color_manual(name = "Región", values = pal_vals) +
    ggplot2::scale_y_continuous(limits = c(0, 10), breaks = 0:10) +
    ggplot2::labs(
      title    = paste0("INCORE | ", pil_nom, " (", edicion, ")"),
      subtitle = "Puntaje por indicador (0–10)",
      x = NULL, y = NULL,
      caption = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = if (isTRUE(mostrar_leyenda)) "right" else "none",
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  p
}
