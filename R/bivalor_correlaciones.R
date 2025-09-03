#' Matriz de correlaciones bivariadas de **valores** (una edición)
#'
#' Cruza varios indicadores (valores originales, NO puntajes 0–10) y muestra:
#' - Arriba: dispersión por región (X vs Y).
#' - Diagonal: densidades univariadas.
#' - Abajo: heatmap del coeficiente de correlación entre indicadores.
#'
#' Siempre excluye `region == "Perú"` salvo que se pida lo contrario.
#'
#' @param edicion integer(1). Edición (p. ej., 2025).
#' @param indicadores character(n>=2). Códigos (p. ej., "EDU1") o nombres de indicadores (valores).
#' @param regiones "ALL" o vector de regiones (códigos/nombres/grupos).
#' @param usar_codigos logical. Si TRUE, traduce códigos de región/indicador.
#' @param incluir_peru logical. Incluir "Perú". Default FALSE.
#' @param metodo "spearman" (default) o "pearson".
#' @param paleta_cor "rb" (rojo-azul) o "viridis" para el heatmap de r.
#' @param alpha_puntos numeric. Opacidad de puntos (default 0.8).
#'
#' @return Un objeto `GGally::ggpairs`.
#' @export
bivalor_correlaciones <- function(edicion,
                                  indicadores,
                                  regiones = "ALL",
                                  usar_codigos = TRUE,
                                  incluir_peru = FALSE,
                                  metodo = c("spearman","pearson"),
                                  paleta_cor = c("rb","viridis"),
                                  alpha_puntos = 0.8) {

  # -------- validaciones & deps --------
  if (missing(edicion) || length(edicion) != 1L || !is.numeric(edicion))
    stop("'edicion' debe ser numérica y de longitud 1.")
  if (missing(indicadores) || length(indicadores) < 2L)
    stop("'indicadores' debe tener al menos 2 elementos (códigos o nombres).")

  metodo     <- match.arg(metodo)
  paleta_cor <- match.arg(paleta_cor)
  edicion    <- as.integer(edicion)

  req <- function(p) if (!requireNamespace(p, quietly = TRUE)) stop("Falta '", p, "'.")
  req("dplyr"); req("tidyr"); req("stringr"); req("ggplot2"); req("GGally")

  # -------- lectura base (valores; no filtramos a puntaje 0–10) --------
  df0 <- leer_incore(
    edicion         = edicion,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  )

  # columnas mínimas
  nm <- names(df0)
  must <- c("region","indicador","valor")
  if (!all(must %in% nm)) {
    stop("La base no tiene columnas mínimas: ", paste(must, collapse = ", "))
  }

  # normalizar strings y tipos
  df0 <- dplyr::mutate(
    df0,
    region    = stringr::str_squish(.data$region),
    indicador = stringr::str_squish(.data$indicador),
    valor     = suppressWarnings(as.numeric(.data$valor))
  )

  # excluir Perú por defecto
  if (!isTRUE(incluir_peru)) {
    df0 <- dplyr::filter(df0, .data$region != "Perú")
  }

  # -------- resolver indicadores (códigos/nombres) --------
  # preferimos usar 'ind_cod' si existe; si no, mapeamos via catálogo
  if (!"ind_cod" %in% names(df0) || all(is.na(df0$ind_cod))) {
    dic_ind <- try(catalogo_indicador(), silent = TRUE)
    if (!inherits(dic_ind, "try-error") && all(c("codigo","nombre") %in% names(dic_ind))) {
      df0 <- dplyr::left_join(
        df0,
        dplyr::select(dic_ind, ind_cod = .data$codigo, ind_name = .data$nombre),
        by = c("indicador" = "ind_name")
      )
    } else {
      df0$ind_cod <- df0$indicador
    }
  }

  # traducir 'indicadores' de entrada (mezcla de códigos y nombres) -> usar códigos si se puede
  dic_ind2 <- try(catalogo_indicador(), silent = TRUE)
  if (!inherits(dic_ind2, "try-error") && all(c("codigo","nombre") %in% names(dic_ind2))) {
    ind_in <- unique(c(
      dic_ind2$codigo[dic_ind2$codigo %in% indicadores],
      dic_ind2$codigo[match(indicadores, dic_ind2$nombre)]
    ))
    ind_in <- unique(stats::na.omit(ind_in))
    if (!length(ind_in)) {
      # si no mapeó, usar tal cual lo recibido
      ind_in <- unique(indicadores)
    }
  } else {
    ind_in <- unique(indicadores)
  }

  df <- dplyr::filter(df0, .data$ind_cod %in% ind_in | .data$indicador %in% ind_in)
  if (nrow(df) == 0L) stop("No hay datos para los indicadores/regiones seleccionados en la edición indicada.")

  # -------- filtro regiones (opcional) --------
  if (!identical(regiones, "ALL")) {
    regs <- regiones
    if (usar_codigos && exists("traducir_codigo", mode = "function") && exists("catalogo_region", mode = "function")) {
      tr <- try(suppressWarnings(traducir_codigo(regiones, catalogo_region())), silent = TRUE)
      if (!inherits(tr, "try-error")) regs <- unique(c(stats::na.omit(tr), regiones))
    }
    df <- dplyr::filter(df, .data$region %in% regs)
  }
  if (nrow(df) == 0L) stop("No hay datos tras filtrar regiones.")

  # -------- consolidar & pivotear: región x indicadores --------
  # 1 valor por (region, ind_cod)
  df_sum <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .by = c(.data$region, .data$ind_cod))

  # columnas en orden de 'ind_in' si es posible
  if (all(ind_in %in% df_sum$ind_cod)) {
    levels_cols <- ind_in
  } else {
    levels_cols <- sort(unique(df_sum$ind_cod))
  }

  mat <- tidyr::pivot_wider(
    df_sum,
    id_cols     = .data$region,
    names_from  = .data$ind_cod,
    values_from = .data$valor
  )

  # quitar filas totalmente NA y columnas con varianza ~0 o NA
  # (correlaciones necesitarán pares completos)
  num <- dplyr::select(mat, - .data$region)
  # mantener solo indicadores con al menos 3 valores no-NA
  keep_cols <- names(num)[colSums(!is.na(num)) >= 3]
  if (length(keep_cols) < 2) stop("Quedan < 2 indicadores con datos suficientes para correlacionar.")
  num <- num[, keep_cols, drop = FALSE]

  # reordenar columnas
  ord <- intersect(levels_cols, names(num))
  num <- num[, ord, drop = FALSE]

  # etiquetas “bonitas” (código → nombre) para títulos, si existe catálogo
  lab_map <- setNames(ord, ord)
  if (!inherits(dic_ind2, "try-error") && all(c("codigo","nombre") %in% names(dic_ind2))) {
    nn <- dic_ind2$nombre[match(ord, dic_ind2$codigo)]
    lab_map[!is.na(nn)] <- nn[!is.na(nn)]
  }

  # -------- funciones de celda para ggpairs --------
  # lower: heatmap de r
  lower_fun <- function(data, mapping, ...){
    x <- rlang::eval_tidy(mapping$x, data)
    y <- rlang::eval_tidy(mapping$y, data)
    ok <- stats::complete.cases(x, y)
    r <- suppressWarnings(stats::cor(x[ok], y[ok], method = metodo))
    r <- ifelse(is.finite(r), r, NA_real_)

    # color según r
    dfc <- data.frame(r = r)
    p <- ggplot2::ggplot(dfc, ggplot2::aes(x = 0, y = 0, fill = r)) +
      ggplot2::geom_tile(width = 1, height = 1) +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(r), "NA", sprintf("%.2f", r))),
                         color = "black", size = 4.2) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = ggplot2::margin(2,2,2,2))

    if (paleta_cor == "rb") {
      p + ggplot2::scale_fill_gradient2(low = "#D7301F", mid = "white", high = "#2C7FB8",
                                        limits = c(-1,1), oob = scales::squish)
    } else {
      p + ggplot2::scale_fill_viridis_c(limits = c(-1,1), option = "C", oob = scales::squish)
    }
  }

  # upper: dispersión con puntos
  upper_fun <- GGally::wrap(
    "points",
    alpha = alpha_puntos,
    size  = 1.8
  )

  # diagonal: densidad
  diag_fun <- GGally::wrap("densityDiag")

  # nombres seguros
  labels <- colnames(num)           # etiquetas originales para mostrar
  colnames(num) <- make.names(labels)  # nombres seguros para R

  # construir ggpairs
  g <- GGally::ggpairs(
    data   = num,
    columns = seq_len(ncol(num)),
    upper   = list(continuous = upper_fun),
    lower   = list(continuous = lower_fun),
    diag    = list(continuous = diag_fun),
    columnLabels = labels
  ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot"
    ) +
    ggplot2::labs(
      title    = "INCORE | Relación bivariadas entre indicadores",
      subtitle = paste0("Edición ", edicion, " | Método: ", metodo),
      caption  = "Nota: correlaciones sobre pares completos por celda."
    )

  return(g)
}
