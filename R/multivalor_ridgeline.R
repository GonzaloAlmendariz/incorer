#' Ridgeline de densidades por año (valores, no puntajes)
#'
#' Muestra la distribución de los **valores originales** de un indicador del INCORE
#' a lo largo de varias ediciones/años, usando ridgelines (una cresta por año).
#' Agrega sobre **regiones** por cada año y excluye puntajes 0–10.
#'
#' @param indicador character(1). Código (p.ej. "EDU2") o nombre del indicador.
#' @param ediciones integer(n>=2). Años/ediciones a incluir (p.ej. `2017:2025`).
#' @param regiones "ALL" o vector de regiones (códigos o nombres). Se usa solo para limitar
#'   qué regiones aportan a la densidad de cada año.
#' @param usar_codigos logical(1). Si `TRUE`, traduce códigos en filtros.
#' @param incluir_peru logical(1). Si `TRUE`, incluye la fila "Perú". Default `FALSE`.
#' @param paleta "ipe","okabe_ito","viridis". Paleta discreta para las crestas (años).
#' @param scale numeric(1). Factor vertical de superposición de crestas. Default 1.1.
#' @param rel_min_height numeric(1). Umbral mínimo relativo de densidad. Default 0.001.
#' @param mostrar_leyenda logical(1). Mostrar/ocultar leyenda. Default `FALSE`.
#'
#' @return Un objeto `ggplot2` con las densidades por año.
#' @examples
#' # valor_ridgeline("EDU2", 2017:2025)
#' # valor_ridgeline("EDU2", 2017:2025, regiones = c("Arequipa","Cusco","Tacna"))
#' @export
valor_ridgeline <- function(indicador,
                                 ediciones,
                                 regiones = "ALL",
                                 usar_codigos = TRUE,
                                 incluir_peru = FALSE,
                                 paleta = c("ipe","okabe_ito","viridis"),
                                 scale = 1.1,
                                 rel_min_height = 0.001,
                                 mostrar_leyenda = FALSE) {
  paleta <- match.arg(paleta)

  # deps
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Falta 'dplyr'.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("ggridges", quietly = TRUE)) stop("Falta 'ggridges'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Falta 'stringr'.")

  # traer base (valores, no puntajes)
  df0 <- valor_traer_base_valores(
    ediciones   = ediciones,
    indicador   = indicador,
    regiones    = regiones,
    usar_codigos= usar_codigos,
    unidad      = "ALL",
    verbose     = FALSE
  )

  if (!nrow(df0)) stop("No se encontró información para el indicador/ediciones/regiones.")

  # limpiar / normalizar
  df0 <- dplyr::mutate(
    df0,
    unidad = stringr::str_squish(.data$unidad),
    valor  = suppressWarnings(as.numeric(.data$valor))
  )

  # excluir puntajes 0–10 y, por defecto, Perú
  df0 <- dplyr::filter(df0, .data$unidad != "Puntaje del 0 al 10")
  if (!isTRUE(incluir_peru) && "region" %in% names(df0)) {
    df0 <- dplyr::filter(df0, .data$region != "Perú")
  }

  # elegir columna de año (prefiere 'anio'; si no, usa 'edicion')
  if ("anio" %in% names(df0)) {
    df0$anio <- suppressWarnings(as.integer(df0$anio))
  } else {
    df0$anio <- suppressWarnings(as.integer(df0$edicion))
  }

  df <- dplyr::filter(
    df0,
    !is.na(.data$anio),
    .data$anio %in% as.integer(ediciones),
    is.finite(.data$valor)
  )

  if (!nrow(df)) stop("No hay “valores” (no puntajes) para este indicador/ediciones.")

  # evitar NaN de bandwidth: conservar años con >= 3 observaciones válidas
  cnt <- dplyr::count(df, .data$anio, wt = !is.na(.data$valor))
  anos_ok <- cnt$anio[cnt$n >= 3]
  df <- dplyr::filter(df, .data$anio %in% anos_ok)
  if (!nrow(df)) {
    stop("No hay suficientes observaciones por año (se requieren ≥ 3) para estimar densidades.")
  }

  # ordenar años
  anos_orden <- sort(unique(df$anio))
  df$anio_f  <- factor(df$anio, levels = anos_orden)  # de arriba (reciente) a abajo (antiguo)

  # paletas discretas (años)
  build_palette <- function(n, which = c("ipe","okabe_ito","viridis")) {
    which <- match.arg(which)
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
  pal_vals <- build_palette(length(anos_orden), paleta)

  # fuente (caption)
  caption_txt <- tryCatch(valor_fuente_caption(df), error = function(e) NULL)

  # plot
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$valor, y = .data$anio_f, fill = .data$anio_f)
  ) +
    ggridges::geom_density_ridges(
      scale = scale,
      rel_min_height = rel_min_height,
      color = "grey20",
      linewidth = 0.35
    ) +
    ggplot2::scale_fill_manual(values = setNames(pal_vals, rev(anos_orden))) +
    ggplot2::labs(
      title    = paste0("INCORE | ", unique(df$indicador)),
      subtitle = paste0("Unidad: ", unique(df$unidad)),
      x        = NULL,
      y        = "Año",
      fill     = "Año",
      caption  = caption_txt %||% "Fuente: IPE / varias"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = if (isTRUE(mostrar_leyenda)) "right" else "none",
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  return(p)
}
