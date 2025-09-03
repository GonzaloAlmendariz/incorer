#' Dispersión bivariada de \strong{valores} (una edición)
#'
#' Grafica un diagrama de dispersión que cruza \strong{dos valores} (no puntajes 0–10)
#' de indicadores del INCORE por \strong{región} en una \strong{edición}.
#' Cada punto es una región: el eje X corresponde a \code{ind_x} y el eje Y a \code{ind_y}.
#'
#' Acepta indicadores por \strong{código} (p. ej., \code{"EDU1"}) o por \strong{nombre completo},
#' y filtra automáticamente \code{region == "Perú"} salvo que se indique lo contrario.
#' Ofrece un modo de color \strong{monocromo} o paletas cualitativas, además de un
#' modo para \strong{resaltar} un subconjunto de regiones.
#'
#' @details
#' \itemize{
#'   \item Trabaja con \strong{valores originales} (unidades propias), \emph{no} con puntajes 0–10.
#'   \item Si hay duplicados por (región, indicador), se consolida usando el \strong{promedio}.
#'   \item Con \code{usar_codigos = TRUE} se aceptan \strong{códigos} de región e indicador;
#'         internamente se traducen a nombres oficiales.
#'   \item En \code{modo = "resaltar"}, las regiones de \code{highlight_regiones} se dibujan con
#'         \code{highlight_color} y el resto en gris tenue.
#' }
#'
#' @param edicion integer(1). Año de edición (p. ej., 2025).
#' @param ind_x,ind_y character(1). Código o nombre del indicador a ubicar en los ejes
#'   X e Y, respectivamente (\strong{valores, no puntajes}).
#' @param regiones \code{"ALL"} (default) o vector de regiones a incluir (códigos o nombres).
#' @param usar_codigos logical. Si \code{TRUE}, traduce códigos de región (y acepta códigos de indicador) antes de filtrar.
#' @param incluir_peru logical. Si \code{TRUE}, incluye la fila \code{"Perú"}. Default \code{FALSE}.
#' @param paleta character(1). Paleta para regiones: \code{"ipe"}, \code{"okabe_ito"}, \code{"viridis"} o \code{"mono"}.
#'   En \code{"mono"} todos los puntos comparten \code{color_mono} y no se muestra leyenda.
#' @param color_mono character(1). Color para \code{paleta = "mono"}. Default \code{"#3B5B92"}.
#' @param modo character(1). \code{"normal"} (colores por región) o \code{"resaltar"}.
#' @param highlight_regiones character(). Conjunto de regiones a resaltar cuando \code{modo = "resaltar"} (recomendado \eqn{\le} 8).
#' @param highlight_color character(1). Color para regiones resaltadas. Default \code{"#D7301F"}.
#' @param mostrar_leyenda logical. Mostrar leyenda (ignorado en \code{"mono"}; en \code{"resaltar"} se muestran solo las resaltadas).
#'
#' @return Un objeto \strong{ggplot2} con la dispersión bivariada.
#'
#' @section Sugerencias:
#' \itemize{
#'   \item Si las unidades de \code{ind_x} y \code{ind_y} difieren mucho, considere estandarizar fuera de la función
#'         o usar ejes en log (\code{scale_x_log10()}, \code{scale_y_log10()}).
#'   \item Para destacar patrones regionales, use \code{modo = "resaltar"} con 3–6 regiones clave y paleta monocroma para el resto.
#' }
#'
#' @examples
#' # 1) Dispersión simple (todas las regiones, color por región)
#' bivalor_dispersion(2025, ind_x = "EDU1", ind_y = "LAB3")
#'
#' # 2) Subconjunto de regiones y paleta Okabe-Ito
#' bivalor_dispersion(2025, "EDU1", "LAB3",
#'                    regiones = c("Arequipa", "Lima*", "MOQ"),
#'                    paleta = "okabe_ito")
#'
#' # 3) Modo monocromo, sin leyenda
#' bivalor_dispersion(2025, "EDU1", "LAB3", paleta = "mono", color_mono = "#555577")
#'
#' # 4) Resaltar conjunto sobre fondo gris
#' bivalor_dispersion(2025, "EDU1", "LAB3",
#'                    modo = "resaltar",
#'                    highlight_regiones = c("Arequipa","Cusco","Tacna"),
#'                    highlight_color = "#C43C39",
#'                    mostrar_leyenda = TRUE)
#'
#'
#' @family bivalor_
#' @seealso \code{\link{bivalor_mapa}}, \code{\link{bivalor_tabla}}
#' @export
bivalor_dispersion <- function(edicion,
                               ind_x, ind_y,
                               regiones = "ALL",
                               usar_codigos = TRUE,
                               incluir_peru = FALSE,
                               paleta = c("ipe","okabe_ito","viridis","mono"),
                               color_mono = "#3B5B92",
                               modo = c("normal","resaltar"),
                               highlight_regiones = character(0),
                               highlight_color = "#D7301F",
                               mostrar_leyenda = TRUE) {
  # deps
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr", quietly = TRUE))   stop("Falta 'dplyr'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Falta 'stringr'.")
  if (!requireNamespace("tidyr", quietly = TRUE))   stop("Falta 'tidyr'.")
  # gghighlight solo si se usa "resaltar"
  modo  <- match.arg(modo)
  paleta <- match.arg(paleta)
  if (modo == "resaltar" && !requireNamespace("gghighlight", quietly = TRUE)) {
    stop("Falta 'gghighlight' para el modo 'resaltar'. Instálalo con install.packages('gghighlight').")
  }

  # validación edición
  valor_assert_rango_fechas(edicion = edicion)
  edicion <- as.integer(edicion)

  # resolver indicadores (acepta cod/nombre; tu catálogo usa 'cod' / 'nombre')
  cat_ind <- catalogo_indicador()
  # normaliza nombres de columnas a (cod, nombre)
  if ("codigo" %in% names(cat_ind)) names(cat_ind)[names(cat_ind)=="codigo"] <- "cod"
  if ("code"   %in% names(cat_ind)) names(cat_ind)[names(cat_ind)=="code"]   <- "cod"
  if ("name"   %in% names(cat_ind)) names(cat_ind)[names(cat_ind)=="name"]   <- "nombre"
  cat_ind$cod    <- stringr::str_squish(toupper(cat_ind$cod))
  cat_ind$nombre <- stringr::str_squish(cat_ind$nombre)

  map_ind <- function(x) {
    x1 <- stringr::str_squish(x)
    x_up <- toupper(gsub("\\s+","", x1))
    # ¿coincide como código?
    if (x_up %in% cat_ind$cod) {
      list(cod = x_up, nombre = cat_ind$nombre[match(x_up, cat_ind$cod)])
    } else if (x1 %in% cat_ind$nombre) {
      list(cod = cat_ind$cod[match(x1, cat_ind$nombre)], nombre = x1)
    } else {
      list(cod = x1, nombre = x1) # fallback
    }
  }
  rx <- map_ind(ind_x)
  ry <- map_ind(ind_y)

  # traer base total de esa edición y quedarnos con NO–PUNTAJE
  base <- valor_traer_base_valores(
    edicion = edicion,
    regiones = regiones,
    unidad   = "ALL",
    usar_codigos = usar_codigos,
    verbose = FALSE
  ) |>
    dplyr::filter(.data$region != "Perú",
                  .data$unidad != "Puntaje del 0 al 10")

  if (!isTRUE(incluir_peru)) {
    base <- dplyr::filter(base, .data$region != "Perú")
  }

  if (nrow(base) == 0) stop("No hay valores (no-puntaje) para esos filtros.")

  # Para cruzar X vs Y: en base el indicador está por NOMBRE
  df_x <- base |>
    dplyr::filter(.data$indicador == !!rx$nombre) |>
    dplyr::select(region, valor_x = valor, unidad_x = unidad)
  df_y <- base |>
    dplyr::filter(.data$indicador == !!ry$nombre) |>
    dplyr::select(region, valor_y = valor, unidad_y = unidad)

  datos <- dplyr::inner_join(df_x, df_y, by = "region")
  if (nrow(datos) == 0) stop("No hay cruce común de regiones para esos indicadores.")

  # etiquetas cortas de unidad
  ux <- .vh_unidad_map(datos$unidad_x)$unidad_short[1]
  uy <- .vh_unidad_map(datos$unidad_y)$unidad_short[1]

  # títulos
  title_lab <- paste0("INCORE | ", rx$cod, " × ", ry$cod)
  subt_lab  <- paste0(rx$nombre, " [", ux, "]  vs  ", ry$nombre, " [", uy, "]")

  # paletas cualitativas (para modo "normal")
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

  # base plot
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = .data$valor_x, y = .data$valor_y))

  if (modo == "normal") {
    if (paleta == "mono") {
      p <- p +
        ggplot2::geom_point(color = color_mono, size = 2.8, alpha = 0.9, show.legend = FALSE)
      legend_pos <- "none"
      scale_col  <- NULL
    } else {
      regs <- sort(unique(datos$region))
      pal  <- build_palette(length(regs), which = paleta)
      names(pal) <- regs
      p <- p +
        ggplot2::geom_point(ggplot2::aes(color = .data$region), size = 2.8, alpha = 0.9)
      legend_pos <- if (isTRUE(mostrar_leyenda)) "right" else "none"
      scale_col  <- ggplot2::scale_color_manual(values = pal)
    }

  } else { # modo == "resaltar"
    # chequear límite de resaltadas
    if (length(highlight_regiones) > 8) {
      stop("Máximo 8 regiones en 'highlight_regiones' para no saturar las etiquetas.")
    }
    # traducir códigos de regiones si corresponde
    hregs <- highlight_regiones
    if (usar_codigos && length(hregs)) {
      tr <- try(suppressWarnings(traducir_codigo(hregs, catalogo_region())), silent = TRUE)
      if (!inherits(tr, "try-error")) {
        hregs <- unique(c(stats::na.omit(tr), hregs))
      }
    }
    # factor para que la leyenda muestre solo las resaltadas
    datos$grupo <- ifelse(datos$region %in% hregs, datos$region, "(otras)")

    # paleta: sólo para resaltadas
    regs_h <- intersect(unique(datos$region), hregs)
    pal_h  <- stats::setNames(rep(highlight_color, length(regs_h)), regs_h)

    p <- p +
      ggplot2::geom_point(
        data = dplyr::mutate(datos, color_key = ifelse(.data$region %in% regs_h, .data$region, NA_character_)),
        ggplot2::aes(color = .data$color_key),
        size = 3, alpha = 0.95, na.rm = TRUE
      ) +
      # <<< AQUÍ EL HIGHLIGHT CON CAJA >>>
      gghighlight::gghighlight(
        region %in% !!regs_h,
        label_key = region,
        use_group_by = FALSE,
        unhighlighted_params = list(color = "grey70", alpha = 0.35),
        label_params = list(
          # cajita blanca alrededor del texto
          fill = "white",
          label.size = 0.2
        ),
        keep_scales = TRUE
      )

    # Leyenda: sólo regiones resaltadas (no mostrar "(otras)")
    legend_pos <- if (isTRUE(mostrar_leyenda) && length(regs_h)) "right" else "none"
    scale_col  <- ggplot2::scale_color_manual(
      values = pal_h,
      breaks = regs_h,  # solo las resaltadas
      labels = regs_h,
      na.translate = FALSE,
      guide = if (length(regs_h) && mostrar_leyenda) "legend" else "none"
    )
  }

  p <- p +
    { if (!is.null(scale_col)) scale_col else NULL } +
    ggplot2::labs(
      title = paste0(title_lab, " (", edicion, ")"),
      subtitle = subt_lab,
      x = paste0(rx$nombre, " [", ux, "]"),
      y = paste0(ry$nombre, " [", uy, "]"),
      color = "Región"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = legend_pos,
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12)
    )

  p
}
