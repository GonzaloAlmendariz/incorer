#' Traducir códigos a nombres oficiales usando un catálogo
#' @param x vector de códigos o nombres
#' @param dic diccionario con columnas (codigo, nombre) o (code, label)
#' @return vector de nombres traducidos; avisa si hay códigos no reconocidos
#' @export
traducir_codigo <- function(x, dic) {
  # Normaliza nombres de columnas del diccionario
  nms <- names(dic)
  if (all(c("codigo", "nombre") %in% nms)) {
    dic2 <- dic[, c("codigo", "nombre")]
  } else if (all(c("code", "label") %in% nms)) {
    dic2 <- dic[, c("code", "label")]
    names(dic2) <- c("codigo", "nombre")
  } else {
    stop("El diccionario debe tener columnas (codigo, nombre) o (code, label).")
  }

  # Construye un named vector: names = codigo, values = nombre
  mapa <- setNames(dic2$nombre, dic2$codigo)

  # Si el usuario ya pasó nombres (no códigos), respétalos cuando no estén en el mapa
  x_chr <- as.character(x)
  out <- ifelse(x_chr %in% names(mapa), unname(mapa[x_chr]), x_chr)

  # Warn si hay códigos que no se encontraron
  no_ok <- x_chr[!(x_chr %in% names(mapa)) & !(x_chr %in% dic2$nombre)]
  if (length(no_ok)) {
    warning("Códigos no reconocidos: ", paste(unique(no_ok), collapse = ", "))
  }
  out
}
