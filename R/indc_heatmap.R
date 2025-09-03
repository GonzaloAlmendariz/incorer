#' Heatmap de puntajes por indicadores o por tiempo (INCORE)
#'
#' Dibuja un mapa de calor con filas = regiones. Las columnas pueden ser:
#' - **Indicadores** de un pilar en **una** edición (`modo = "indicadores"`).
#' - **Ediciones** para **un** indicador (`modo = "tiempo"`).
#'
#' Siempre usa `indc_leer_incore(..., agregar_codigos = TRUE)` para contar con
#' columnas de códigos (`pilar_cod`, `ind_cod`) y trabajar solo con puntajes 0–10.
#' En ambos modos se excluye el indicador "General".
#'
#' @param modo "indicadores" (columnas = indicadores del pilar, una edición)
#'             o "tiempo" (columnas = ediciones, un indicador).
#' @param pilar character(1). Código o nombre del pilar (requerido si `modo="indicadores"`).
#' @param indicador character(1). Código o nombre del indicador (requerido si `modo="tiempo"`).
#' @param edicion integer(1). Edición cuando `modo="indicadores"`.
#' @param ediciones integer(n>=2). Rango de ediciones cuando `modo="tiempo"`.
#' @param regiones "ALL" o vector de regiones (códigos, nombres o grupos `gr_*`;
#'                 admite exclusiones con prefijo "-").
#' @param usar_codigos logical. Si TRUE, admite filtros por códigos de región/pilar/indicador.
#' @param ordenar "ninguno","por_ultimo","por_promedio" (solo en `modo="tiempo"`).
#' @param paleta "blues","viridis","cividis","magma".
#' @param anotar logical. Escribir el valor dentro de cada celda.
#' @param mostrar_leyenda logical.
#' @return Un objeto `ggplot2`.
#' @export
indc_heatmap <- function(modo = c("indicadores","tiempo"),
                         pilar = NULL,
                         indicador = NULL,
                         edicion = NULL,
                         ediciones = NULL,
                         regiones = "ALL",
                         usar_codigos = TRUE,
                         ordenar = c("ninguno","por_ultimo","por_promedio"),
                         paleta = c("blues","viridis","cividis","magma"),
                         anotar = FALSE,
                         mostrar_leyenda = TRUE) {

  modo    <- match.arg(modo)
  ordenar <- match.arg(ordenar)
  paleta  <- match.arg(paleta)

  # deps
  req <- function(pkg) if (!requireNamespace(pkg, quietly = TRUE)) stop("Falta '", pkg, "'.")
  req("ggplot2"); req("dplyr"); req("tidyr"); req("stringr"); req("forcats")

  # Paleta continua (helpers indc_)
  fill_scale <- indc_paleta_continua(paleta)

  # --------------------------
  # MODO: indicadores (una edición)
  # --------------------------
  if (modo == "indicadores") {
    if (is.null(pilar))   stop("Para modo='indicadores' debes especificar 'pilar'.")
    if (is.null(edicion)) stop("Para modo='indicadores' debes especificar 'edicion' (una sola).")

    indc_validar_rango(edicion = edicion)

    # Trae base y normaliza
    df <- indc_leer_incore(
      edicion         = as.integer(edicion),
      usar_codigos    = usar_codigos,
      agregar_codigos = TRUE,
      verbose         = FALSE
    ) |>
      indc_normalizar_columnas() |>
      dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                    .data$indicador != "General",
                    .data$region != "Perú")

    # Resolver pilar (acepta código o nombre)
    pilar_nom <- indc_resolver_pilar(pilar, usar_codigos = usar_codigos)
    df <- dplyr::filter(
      df,
      (!is.na(.data$pilar_cod) & .data$pilar_cod == !!pilar) |
        .data$pilar == !!pilar_nom
    )
    pilar_etq <- if (nrow(df)) unique(df$pilar)[1] else pilar_nom

    # Filtrar regiones (grupos/exclusiones/códigos)
    df <- indc_filtrar_regiones(df, regiones = regiones, usar_codigos = usar_codigos)

    if (!"ind_cod" %in% names(df) || all(is.na(df$ind_cod))) {
      stop("No se encontró 'ind_cod'. Asegúrate de leer con agregar_codigos = TRUE.")
    }

    # Consolidar a (region, ind_cod)
    df <- df |>
      dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                       .by = c(.data$region, .data$ind_cod)) |>
      dplyr::mutate(
        valor   = round(.data$valor, 2),
        region  = factor(.data$region, levels = sort(unique(.data$region))),
        ind_cod = factor(.data$ind_cod, levels = sort(unique(.data$ind_cod)))
      )

    if (nrow(df) == 0L) stop("No hay datos para graficar tras los filtros.")

    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$ind_cod, y = .data$region, fill = .data$valor)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.3) +
      fill_scale +
      ggplot2::labs(
        title    = paste0("INCORE — Indicadores del pilar: ", pilar_etq),
        subtitle = paste0("Edición ", edicion),
        x = "Indicador", y = "Región", fill = "Puntaje",
        caption  = "Fuente: Instituto Peruano de Economía (IPE)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position     = if (mostrar_leyenda) "right" else "none",
        panel.grid.major    = ggplot2::element_blank(),
        panel.grid.minor    = ggplot2::element_blank(),
        plot.title.position = "plot",
        plot.margin         = ggplot2::margin(8, 12, 8, 12)
      )

    if (isTRUE(anotar)) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", .data$valor)), size = 2.9, color = "grey10"
      )
    }
    return(p)
  }

  # --------------------------
  # MODO: tiempo (múltiples ediciones, un indicador)
  # --------------------------
  if (is.null(indicador)) stop("Para modo='tiempo' debes especificar 'indicador' (código o nombre).")
  if (is.null(ediciones) || length(ediciones) < 2L) {
    stop("Para modo='tiempo' debes pasar 'ediciones' con al menos 2 años (p. ej., 2019:2025).")
  }
  indc_validar_rango(ediciones = ediciones)
  ediciones <- sort(unique(as.integer(ediciones)))

  df <- indc_leer_incore(
    edicion         = ediciones,
    usar_codigos    = usar_codigos,
    agregar_codigos = TRUE,
    verbose         = FALSE
  ) |>
    indc_normalizar_columnas() |>
    dplyr::filter(.data$unidad == "Puntaje del 0 al 10",
                  .data$indicador != "General",
                  .data$region != "Perú")

  # Resolver indicador (acepta código o nombre)
  ind_res <- indc_resolver_indicador(indicador)
  # Filtro robusto por código o nombre
  df <- dplyr::filter(
    df,
    (!is.na(.data$ind_cod) & toupper(.data$ind_cod) == toupper(ind_res$codigo)) |
      tolower(.data$indicador) == tolower(ind_res$nombre)
  )
  ind_etq <- if (nrow(df)) { if (!all(is.na(df$ind_cod))) unique(df$ind_cod)[1] else ind_res$nombre } else indicador
  if (nrow(df) == 0L) stop("No hay datos para el indicador '", indicador, "' en las ediciones seleccionadas.")

  # Filtrar regiones (grupos/exclusiones/códigos)
  df <- indc_filtrar_regiones(df, regiones = regiones, usar_codigos = usar_codigos)

  # Consolidar a (region, edicion)
  df <- df |>
    dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE),
                     .by = c(.data$region, .data$edicion)) |>
    dplyr::mutate(valor = round(.data$valor, 2))

  if (nrow(df) == 0L) stop("No hay datos para graficar tras los filtros.")

  # Orden de filas (regiones)
  orden_reg <- if (ordenar == "por_ultimo") {
    ult <- max(ediciones, na.rm = TRUE)
    df |>
      dplyr::filter(.data$edicion == ult) |>
      dplyr::arrange(dplyr::desc(.data$valor), .data$region) |>
      dplyr::pull(.data$region)
  } else if (ordenar == "por_promedio") {
    df |>
      dplyr::summarise(prom = mean(.data$valor, na.rm = TRUE), .by = .data$region) |>
      dplyr::arrange(dplyr::desc(.data$prom), .data$region) |>
      dplyr::pull(.data$region)
  } else {
    sort(unique(df$region))
  }

  df <- dplyr::mutate(
    df,
    region  = factor(.data$region, levels = unique(orden_reg)),
    edicion = factor(.data$edicion, levels = ediciones)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$edicion, y = .data$region, fill = .data$valor)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    fill_scale +
    ggplot2::labs(
      title    = paste0("INCORE | Evolución del indicador ", indicador),
      subtitle = paste0("De ", min(ediciones), " a ", max(ediciones)),
      x = "Edición", y = "Región", fill = "Puntaje",
      caption  = "Fuente: Instituto Peruano de Economía (IPE)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position     = if (mostrar_leyenda) "right" else "none",
      panel.grid.major    = ggplot2::element_blank(),
      panel.grid.minor    = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin         = ggplot2::margin(8, 12, 8, 12)
    )

  if (isTRUE(anotar)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$valor)), size = 2.9, color = "grey10"
    )
  }

  p
}
