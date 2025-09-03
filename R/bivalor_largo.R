#' Valores bivariados por edición (dos indicadores en un solo panel)
#'
#' Traza dos series de **valores** (no puntajes) para `ind_x` y `ind_y`
#' a lo largo de `ediciones`, agregando (promedio) sobre las `regiones` elegidas.
#' Cada indicador va con su color y su propio eje Y (izquierdo/derecho).
#'
#' @param ediciones integer(n>=2)
#' @param ind_x,ind_y character(1) Código o nombre de indicador (valor, NO puntaje)
#' @param regiones "ALL" o vector de regiones (códigos o nombres). Se promedia.
#' @param usar_codigos logical
#' @param incluir_peru logical
#' @param col_x,col_y colores de las dos series
#' @return ggplot
#' @export
bivalor_largo <- function(ediciones,
                          ind_x,
                          ind_y,
                          regiones = "ALL",
                          usar_codigos = TRUE,
                          incluir_peru = FALSE,
                          col_x = "#355C7D",
                          col_y = "#35B778") {

  # --- validaciones ---
  if (missing(ediciones) || length(ediciones) < 2 || !is.numeric(ediciones))
    stop("'ediciones' debe ser numérico (>= 2 valores).")
  if (missing(ind_x) || missing(ind_y))
    stop("Debes pasar 'ind_x' e 'ind_y' (código o nombre).")

  valor_assert_rango_fechas(ediciones = ediciones)

  req <- function(p) if (!requireNamespace(p, quietly = TRUE))
    stop("Falta '", p, "'.")
  req("dplyr"); req("stringr"); req("ggplot2")

  # --- catálogo de indicadores (detecta columna cod/code/codigo y nombre) ---
  cat_ind <- try(catalogo_indicador(), silent = TRUE)
  if (inherits(cat_ind, "try-error") || !is.data.frame(cat_ind)) {
    stop("No pude leer 'catalogo_indicador()'.")
  }
  # normalizar columnas posibles
  nm <- names(cat_ind)
  cod_col <- c("ind_cod","cod","codigo","code")[c("ind_cod","cod","codigo","code") %in% nm][1]
  nom_col <- c("nombre","name")[c("nombre","name") %in% nm][1]
  if (is.na(cod_col) || is.na(nom_col)) {
    stop("El catálogo debe tener columna de código (ind_cod/cod/codigo/code) y nombre (nombre/name).")
  }
  cat_ind <- dplyr::transmute(
    cat_ind,
    code = stringr::str_squish(toupper(.data[[cod_col]])),
    name = stringr::str_squish(.data[[nom_col]])
  )

  # --- resolutor genérico código/nombre ---
  .resuelve_ind <- function(x) {
    x_s <- stringr::str_squish(x)
    x_up <- toupper(gsub("\\s+", "", x_s))
    # directo por código
    if (x_up %in% cat_ind$code)
      return(list(code = x_up, name = cat_ind$name[match(x_up, cat_ind$code)]))
    # por nombre exacto
    if (x_s %in% cat_ind$name)
      return(list(code = cat_ind$code[match(x_s, cat_ind$name)], name = x_s))
    stop("No pude resolver el indicador '", x, "'.")
  }

  indX <- .resuelve_ind(ind_x)
  indY <- .resuelve_ind(ind_y)

  # --- traer base (todas las regiones/ediciones) y filtrar valores no puntaje ---
  base <- valor_traer_base_valores(
    ediciones   = ediciones,
    regiones    = regiones,
    unidad      = "ALL",
    usar_codigos = usar_codigos,
    verbose     = FALSE
  )

  if (!isTRUE(incluir_peru)) {
    base <- dplyr::filter(base, .data$region != "Perú")
  }

  # Solo valores (no puntaje)
  base <- dplyr::filter(base, .data$unidad != "Puntaje del 0 al 10")

  # Nos quedamos con las filas cuyos nombres de indicador coincidan con los dos resueltos
  base$indicador <- stringr::str_squish(base$indicador)

  # mapa nombre<->código desde catálogo
  name_to_code <- stats::setNames(cat_ind$code, cat_ind$name)

  # filtrar solo los dos indicadores (por nombre)
  keep_names <- c(indX$name, indY$name)
  base <- dplyr::filter(base, .data$indicador %in% keep_names)

  if (nrow(base) == 0) {
    stop("No hay datos de valores para esos indicadores/regiones/ediciones.")
  }

  # agregar (promedio) por edicion e indicador sobre regiones seleccionadas
  df <- base |>
    dplyr::summarise(
      valor = mean(.data$valor, na.rm = TRUE),
      unidad = dplyr::first(.data$unidad),
      .by = c(.data$edicion, .data$indicador)
    ) |>
    dplyr::mutate(
      edicion = as.integer(.data$edicion),
      ind_cod = name_to_code[.data$indicador],
      ind_name = .data$indicador
    )

  # unidades por indicador
  uni_x <- df$unidad[df$ind_cod == indX$code][1]
  uni_y <- df$unidad[df$ind_cod == indY$code][1]

  # info corta de unidades (para subtítulo)
  uinfo_x <- .vh_unidad_map(uni_x)
  uinfo_y <- .vh_unidad_map(uni_y)

  # separar series
  sx <- dplyr::filter(df, .data$ind_cod == indX$code)
  sy <- dplyr::filter(df, .data$ind_cod == indY$code)

  # asegurar que ambas tienen mismas ediciones (intersección)
  eds_common <- intersect(sx$edicion, sy$edicion)
  sx <- dplyr::filter(sx, .data$edicion %in% eds_common)
  sy <- dplyr::filter(sy, .data$edicion %in% eds_common)

  if (!length(eds_common)) stop("No hay ediciones comunes entre las dos series.")

  # --- reescalado para eje secundario ---
  # y' = (y - a)/b para pintar sy en el mismo canvas que sx
  rng_x <- range(sx$valor, na.rm = TRUE); dx <- diff(rng_x); if (!is.finite(dx) || dx == 0) dx <- 1
  rng_y <- range(sy$valor, na.rm = TRUE); dy <- diff(rng_y); if (!is.finite(dy) || dy == 0) dy <- 1
  b <- dy / dx
  a <- rng_y[1] - b * rng_x[1]
  # función de ida (para graficar Y en escala X) y vuelta (para rotular eje secundario)
  fwd  <- function(y) (y - a) / b
  inv  <- function(y_scaled) a + b * y_scaled

  # --- plot ---
  title_lab <- paste0(
    "INCORE — ", indX$name, "  vs  ", indY$name
  )
  subt_lab <- paste0(
    "Unidad: ", uni_x, "  |  ", uni_y
  )

  # calcular límites con un margen de 10%
  y_all <- c(sx$valor, fwd(sy$valor))
  y_min <- min(y_all, na.rm = TRUE)
  y_max <- max(y_all, na.rm = TRUE)
  y_pad <- 0.4 * (y_max - y_min)  # 40% extra
  y_limits <- c(y_min - y_pad, y_max + y_pad)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = sx,
      ggplot2::aes(x = .data$edicion, y = .data$valor),
      linewidth = 1, color = col_x, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = sx,
      ggplot2::aes(x = .data$edicion, y = .data$valor),
      size = 2, color = col_x, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = sy,
      ggplot2::aes(x = .data$edicion, y = fwd(.data$valor)),
      linewidth = 1, color = col_y, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = sy,
      ggplot2::aes(x = .data$edicion, y = fwd(.data$valor)),
      size = 2, color = col_y, na.rm = TRUE
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(eds_common))) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      name = paste0(indX$name, " — ", uinfo_x$unidad_short[1]),
      sec.axis = ggplot2::sec_axis(
        ~ inv(.),
        name = paste0(indY$name, " — ", uinfo_y$unidad_short[1])
      )
    ) +
    ggplot2::labs(
      title = paste0(title_lab, " (", max(eds_common), ")"),
      subtitle = subt_lab,
      x = NULL, y = NULL,
      caption = valor_fuente_caption(base)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(8, 12, 8, 12),
      axis.title.y      = if (requireNamespace("ggtext", quietly = TRUE))
        ggtext::element_markdown(color = col_x)
      else ggplot2::element_text(color = col_x),
      axis.title.y.right = if (requireNamespace("ggtext", quietly = TRUE))
        ggtext::element_markdown(color = col_y)
      else ggplot2::element_text(color = col_y)
    )

  p
}
