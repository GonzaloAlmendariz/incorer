#' Descargar y preparar el mapa sf de regiones del Perú para INCORE (con caché)
#'
#' Igual a tu versión, pero guarda/lee un RDS en un directorio de caché
#' persistente para evitar descargas repetidas (y conexiones abiertas).
#'
#' @param path Carpeta cache GADM crudo (por defecto un dir de caché de incorer).
#' @param version Versión GADM, ej. "4.1".
#' @param simplificar Tolerancia de simplificación (0 = sin simplificar).
#' @return `sf` con columnas `region` y `geometry`.
#' @export
mapa_peru <- function(path = NULL, version = "4.1", simplificar = 0) {
  if (!requireNamespace("geodata", quietly = TRUE)) stop("Falta 'geodata'.")
  if (!requireNamespace("sf", quietly = TRUE))       stop("Falta 'sf'.")
  if (!requireNamespace("dplyr", quietly = TRUE))    stop("Falta 'dplyr'.")
  if (!requireNamespace("stringr", quietly = TRUE))  stop("Falta 'stringr'.")

  # --- Caché persistente del mapa procesado ---
  cache_dir <- if (is.null(path)) {
    # dir de caché del paquete (>= R 4.0)
    if (requireNamespace("tools", quietly = TRUE)) tools::R_user_dir("incorer", which = "cache") else tempdir()
  } else path
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  cache_key <- sprintf("mapa_peru_gadm%s_s%g.rds", version, simplificar)
  cache_rds <- file.path(cache_dir, cache_key)

  if (file.exists(cache_rds)) {
    mapa_sf <- readRDS(cache_rds)
    # seguridad por si el CRS quedó perdido
    if (!inherits(mapa_sf, "sf")) stop("El RDS de caché no es un objeto sf.")
    return(mapa_sf)
  }

  # --- Descarga/lectura GADM solo si no hay caché ---
  gadm2 <- geodata::gadm(country = "PER", level = 2, path = cache_dir,
                         version = version, geometry = "sf")
  if (inherits(gadm2, "SpatVector")) gadm2 <- sf::st_as_sf(gadm2)

  to_incore_dep <- function(dep_name) {
    dep <- stringr::str_squish(dep_name)
    if (grepl("^Lima(\\s|$)", dep, ignore.case = TRUE) ||
        grepl("Lima\\s*Province", dep, ignore.case = TRUE)) {
      dep <- "Lima"
    }
    switch(dep,
           "Amazonas"      = "Amazonas",
           "Ancash"        = "Áncash",
           "Apurimac"      = "Apurímac",
           "Arequipa"      = "Arequipa",
           "Ayacucho"      = "Ayacucho",
           "Cajamarca"     = "Cajamarca",
           "Cusco"         = "Cusco",
           "Huancavelica"  = "Huancavelica",
           "Huanuco"       = "Huánuco",
           "Ica"           = "Ica",
           "Junin"         = "Junín",
           "La Libertad"   = "La Libertad",
           "Lambayeque"    = "Lambayeque",
           "Lima"          = "Lima",
           "Loreto"        = "Loreto",
           "Madre de Dios" = "Madre de Dios",
           "Moquegua"      = "Moquegua",
           "Pasco"         = "Pasco",
           "Piura"         = "Piura",
           "Puno"          = "Puno",
           "San Martin"    = "San Martín",
           "Tacna"         = "Tacna",
           "Tumbes"        = "Tumbes",
           "Ucayali"       = "Ucayali",
           "Callao"        = "Callao",
           dep
    )
  }

  gadm2 <- dplyr::mutate(
    gadm2,
    dep_raw  = .data$NAME_1,
    prov_raw = .data$NAME_2,
    dep_norm = vapply(.data$NAME_1, to_incore_dep, character(1))
  )

  lima_prov <- gadm2 |>
    dplyr::filter(.data$dep_norm == "Lima Provincias",
                  stringr::str_squish(.data$prov_raw) == "Lima") |>
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop")

  lima_otros <- gadm2 |>
    dplyr::filter(.data$dep_norm == "Lima",
                  stringr::str_squish(.data$prov_raw) != "Lima") |>
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop")

  callao <- gadm2 |>
    dplyr::filter(.data$dep_norm == "Callao") |>
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop")

  if (nrow(lima_prov) == 0L) stop("No se encontró 'Provincia de Lima' en GADM.")
  if (nrow(callao)   == 0L)  stop("No se encontró 'Callao' en GADM.")

  lima_star <- sf::st_make_valid(sf::st_union(lima_prov$geometry, callao$geometry))
  lima_star <- sf::st_as_sf(data.frame(region = "Lima*", geometry = lima_star),
                            crs = sf::st_crs(gadm2))

  if (nrow(lima_otros) > 0) {
    lima_provincias <- sf::st_make_valid(lima_otros$geometry)
    lima_provincias <- sf::st_as_sf(
      data.frame(region = "Lima Provincias", geometry = lima_provincias),
      crs = sf::st_crs(gadm2)
    )
  } else {
    lima_provincias <- sf::st_as_sf(
      data.frame(region = "Lima Provincias",
                 geometry = sf::st_sfc(sf::st_polygon())),
      crs = sf::st_crs(gadm2)
    )
  }

  otros <- gadm2 |>
    dplyr::filter(!.data$dep_norm %in% c("Lima", "Callao")) |>
    dplyr::group_by(.data$dep_norm) |>
    dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
    dplyr::rename(region = .data$dep_norm)

  mapa_sf <- dplyr::bind_rows(otros, lima_star, lima_provincias) |>
    dplyr::mutate(region = stringr::str_squish(.data$region)) |>
    dplyr::arrange(.data$region)

  mapa_sf$region <- dplyr::recode(
    mapa_sf$region,
    "Ancash"    = "Áncash",
    "Apurimac"  = "Apurímac",
    "Huanuco"   = "Huánuco",
    "Junin"     = "Junín",
    "San Martin"= "San Martín"
  )

  if (is.numeric(simplificar) && simplificar > 0) {
    mapa_sf <- sf::st_simplify(mapa_sf, dTolerance = simplificar, preserveTopology = TRUE)
  }
  mapa_sf <- sf::st_make_valid(mapa_sf)

  # Guardar en caché y retornar
  saveRDS(mapa_sf, cache_rds)
  mapa_sf
}
