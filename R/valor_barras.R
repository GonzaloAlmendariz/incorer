#' Barras de un VALOR por región (una edición, indicador único)
#'
#' Grafica barras horizontales (coord_flip) del **valor** de un **indicador**
#' específico para **una** edición (NO puntaje). Ordena de mayor a menor y
#' colorea por región con paletas cualitativas.
#'
#' - Filtra automáticamente a \code{unidad != "Puntaje del 0 al 10"}.
#' - \code{indicador} puede ser **código** (p.ej., "ECO7") o **nombre**.
#' - El eje muestra la \emph{unidad} propia del indicador (etiqueta corta).
#' - El \emph{caption} usa la columna \code{fuente} de los datos.
#'
#' @param edicion integer(1). Edición a graficar (p. ej., 2024). Obligatorio.
#' @param pilar character(1). Código o nombre del pilar (opcional si el
#'   \code{indicador} ya identifica unívocamente).
#' @param indicador character(1). Código o nombre completo del indicador.
#' @param regiones "ALL" o vector de regiones (códigos o nombres).
#' @param usar_codigos logical(1). Si TRUE, traduce códigos en filtros.
#' @param incluir_peru logical(1). Si TRUE, incluye "Perú". Default FALSE.
#' @param paleta "ipe","okabe_ito","viridis".
#' @param mostrar_leyenda logical(1). Mostrar/ocultar leyenda. Default FALSE.
#' @param label_indicador "cod" (default) o "nombre": cómo rotular el indicador en el título.
#'
#' @return Un objeto \code{ggplot2}.
#' @examples
#' # ECO7 por regiones (2024), título con código
#' # valor_barras(2024, pilar="ECO", indicador="ECO7")
#'
#' # Mismo con nombre en el título
#' # valor_barras(2024, pilar="ECO", indicador="ECO7", label_indicador="nombre")
#'
#' # Filtrando regiones e incluyendo Perú
#' # valor_barras(2024, indicador="1.7 Billeteras digitales",
#' #              regiones=c("MOQ","Arequipa","Lima*"),
#' #              incluir_peru=TRUE, paleta="okabe_ito", mostrar_leyenda=TRUE)
#' @export
valor_barras <- function(edicion,
                         pilar = NULL,
                         indicador = NULL,
                         regiones = "ALL",
                         usar_codigos = TRUE,
                         incluir_peru = FALSE,
                         paleta = c("ipe","okabe_ito","viridis"),
                         mostrar_leyenda = FALSE,
                         label_indicador = c("nombre","cod")) {

  # --- dependencias ---
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("dplyr",   quietly = TRUE)) stop("Falta 'dplyr'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Falta 'stringr'.")
  if (!requireNamespace("forcats", quietly = TRUE)) stop("Falta 'forcats'.")

  # --- argumentos ---
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion)) {
    stop("'edicion' debe ser numérica y de longitud 1 (p. ej., 2024).")
  }
  edicion <- as.integer(edicion)
  paleta  <- match.arg(paleta)
  label_indicador <- match.arg(label_indicador)

  # rango INCORE 2016–2025
  valor_assert_rango_fechas(edicion = edicion)

  # --- traer base (NO forzar unidad aquí) ---
  df <- valor_traer_base_valores(
    edicion      = edicion,
    pilar        = pilar,
    indicador    = indicador,   # puede venir como cod o nombre
    regiones     = regiones,
    unidad       = "ALL",
    usar_codigos = usar_codigos,
    verbose      = FALSE
  )

  # quedarnos solo con valores NO puntaje
  df <- dplyr::filter(df, .data$unidad != "Puntaje del 0 al 10")

  # opcionalmente excluir "Perú"
  if (!isTRUE(incluir_peru)) {
    df <- dplyr::filter(df, .data$region != "Perú")
  }

  if (nrow(df) == 0L) {
    stop("No hay valores (no-puntaje) para esos filtros.")
  }

  # --- resolver indicador único (código / nombre + unidad y fuente) ---
  # normalizar catálogo para mapear código<->nombre
  cat_ind <- try(catalogo_indicador(), silent = TRUE)
  if (!inherits(cat_ind, "try-error") && is.data.frame(cat_ind)) {
    # admitir columnas "cod"/"codigo"/"code" y "nombre"/"name"
    cn <- names(cat_ind)
    code_col <- intersect(c("cod","codigo","code"), cn)[1]
    name_col <- intersect(c("nombre","name"), cn)[1]
    if (!is.na(code_col) && !is.na(name_col)) {
      names(cat_ind)[match(code_col, names(cat_ind))] <- "code"
      names(cat_ind)[match(name_col, names(cat_ind))] <- "name"
    } else {
      cat_ind <- NULL
    }
  } else {
    cat_ind <- NULL
  }

  # si el usuario NO pasó indicador, intentar deducir único
  if (is.null(indicador)) {
    unicos <- sort(unique(df$indicador))
    if (length(unicos) != 1L) {
      stop("Debes especificar 'indicador' (código o nombre). Opciones detectadas: ",
           paste(unicos, collapse = "; "))
    }
    indicador <- unicos[1]
  }

  # determinar code y name del indicador pedido
  ind_code <- ind_name <- NULL
  if (!is.null(cat_ind)) {
    if (indicador %in% cat_ind$code) {
      ind_code <- indicador
      ind_name <- cat_ind$name[match(indicador, cat_ind$code)]
    } else if (indicador %in% cat_ind$name) {
      ind_name <- indicador
      ind_code <- cat_ind$code[match(indicador, cat_ind$name)]
    }
  }
  # fallback si no lo encontramos en catálogo
  if (is.null(ind_code) && is.null(ind_name)) {
    # buscar coincidencia exacta en df$indicador (nombre)
    if (indicador %in% df$indicador) {
      ind_name <- indicador
    } else {
      # asumir que nos pasaron un "código" no catalogado; filtramos por grepl de prefijo?
      # preferible ser estrictos:
      stop("No pude resolver el indicador '", indicador, "' contra el catálogo ni la base.")
    }
  }

  # filtrar df al indicador resuelto (por nombre, que es lo seguro en la base)
  if (!is.null(ind_name)) {
    df <- dplyr::filter(df, .data$indicador == !!ind_name)
  } else {
    # improbable: sin nombre; intentar por code -> mapear nombre en catálogo
    df <- df[0, ]  # asegura error más abajo si no hay filas
  }

  if (nrow(df) == 0L) {
    stop("No hay datos para el indicador seleccionado en ", edicion, ".")
  }

  # unidad (corta) para el eje
  uinfo <- .vh_unidad_map(unique(df$unidad)[1])
  y_lab <- if (!is.null(uinfo$unidad_short[1]) && !is.na(uinfo$unidad_short[1])) uinfo$unidad_short[1] else "Valor"

  # fuente para caption
  cap_fuente <- valor_fuente_caption(df)

  # --- preparar datos para plot ---
  df_sum <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
    dplyr::arrange(dplyr::desc(.data$valor), .data$region) |>
    dplyr::mutate(region = forcats::fct_reorder(.data$region, .data$valor))

  # paleta por región
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
  reg_levels <- levels(df_sum$region)
  pal_vals   <- build_palette(length(reg_levels), which = paleta); names(pal_vals) <- reg_levels

  # etiquetas numéricas formateadas por unidad
  df_sum$etiqueta <- valor_formato_valor(df_sum$valor, unidad = unique(df$unidad)[1])

  # título según preferencia
  ind_label <- if (label_indicador == "nombre") (ind_name %||% ind_code) else (ind_code %||% ind_name)
  titulo <- paste0("INCORE — ", ind_label, " (", edicion, ")")

  # límites de eje X dinámicos (valor), con pequeño headroom para texto
  xmax <- max(df_sum$valor, na.rm = TRUE)
  xpad <- (xmax %||% 0) * 0.06 + 0.02

  p <- ggplot2::ggplot(
    df_sum,
    ggplot2::aes(x = .data$region, y = .data$valor, fill = .data$region)
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$etiqueta),
      hjust = -0.15, size = 3
    ) +
    ggplot2::scale_fill_manual(values = pal_vals, guide = if (mostrar_leyenda) "legend" else "none") +
    ggplot2::expand_limits(y = xmax + xpad) +
    ggplot2::labs(
      title    = titulo,
      subtitle = NULL,
      x = NULL, y = y_lab,
      fill     = "Región",
      caption  = cap_fuente
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = if (mostrar_leyenda) "right" else "none",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin      = ggplot2::margin(10, 18, 10, 10)
    )

  return(p)
}


