test_that("leer_incore descarga y limpia nombres", {
  skip_on_cran()
  url <- "https://incore-spaces.nyc3.digitaloceanspaces.com/documents/1752079464_INCORE_2025_OpenData_1752079464.xlsx"

  df <- leer_incore(url)

  # columnas clave existen
  expect_true(all(c("edicion","pilar","indicador","unidad","fuente","region","etiqueta","valor","posicion") %in% names(df)))

  # año/anio normalizado
  expect_true("anio" %in% names(df))
  expect_type(df$anio, "integer")

  # algunas filas, valores numéricos
  expect_gt(nrow(df), 1000)
  expect_type(df$valor, "double")
})
