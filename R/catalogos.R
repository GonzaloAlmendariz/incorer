#' Catálogo de años disponibles en INCORE
#' @return tibble con columnas codigo y nombre
#' @export
catalogo_anio <- function() {
  tibble::tibble(
    codigo = as.character(2015:2025),
    nombre = as.character(2015:2025)
  )
}

#' Catálogo de pilares de INCORE
#' @return tibble con columnas codigo y nombre
#' @export
catalogo_pilar <- function() {
  tibble::tibble(
    codigo = c("GEN","ECO","LAB","INF","SAL","EDU","INS"),
    nombre = c(
      "Índice de Competitividad Regional",
      "Entorno económico",
      "Laboral",
      "Infraestructura",
      "Salud",
      "Educación",
      "Instituciones"
    )
  )
}

#' Catálogo: Indicadores (código -> nombre exacto en la base)
#' @return tibble con columnas codigo, nombre
#' @export
catalogo_indicador <- function() {
  tibble::tibble(
    codigo = c(
      "GEN","ECO1","ECO2","ECO3","ECO4","ECO5","ECO6","ECO7",
      "LAB1","LAB2","LAB3","LAB4","LAB5","LAB6","LAB7",
      "INF1","INF2","INF3","INF4","INF5","INF6","INF7",
      "SAL1","SAL2","SAL3","SAL4","SAL5","SAL6","SAL7",
      "EDU1","EDU2","EDU3","EDU4","EDU5","EDU6","EDU7",
      "INS1","INS2","INS3","INS4","INS5","INS6","INS7"
    ),
    nombre = c(
      "General",
      "1.1 PBI real en logaritmos",
      "1.2 Trabajadores en grandes empresas (más de 100 trabajadores)",
      "1.3 Gasto real pre cápita mensual",
      "1.4 Apertura externa",
      "1.5 Tenencia de cuentas",
      "1.6 Acceso al crédito",
      "1.7 Billeteras digitales",
      "2.1 Ingreso por hora por trabajo",
      "2.2 Formalidad laboral",
      "2.3 Trabajadores que ganan menos que el salario mínimo",
      "2.4 Jóvenes que no estudian ni trabajan",
      "2.5 Fuerza laboral educada",
      "2.6 Brecha de género en participación laboral",
      "2.7 Brecha de género en ingresos laborales",
      "3.1 Hogares con acceso a agua",
      "3.2 Hogares con acceso a saneamiento",
      "3.3 Continuidad en la provisión de agua",
      "3.4 Acceso a agua clorada",
      "3.5 Precio medio de electricidad",
      "3.6 Densidad del transporte aéreo nacional",
      "3.7 Uso de internet",
      "4.1 Desnutrición crónica",
      "4.2 Anemia infantil",
      "4.3 Vacunación",
      "4.4 Embarazo adolescente",
      "4.5 Cobertura del personal médico público",
      "4.6 Desabastecimiento de medicamentos",
      "4.7 Camas hospitalarias",
      "5.1 Conclusión secundaria",
      "5.2 Logro educativo en primaria en lectura",
      "5.3 Logro educativo en primaria en matemáticas",
      "5.4 Colegios con acceso a internet",
      "5.5 Locales escolares con acceso a electricidad",
      "5.6 Locales escolares con acceso a agua",
      "5.7 Locales escolares con acceso a desagüe",
      "6.1 Percepción de la gestión pública regional",
      "6.2 Recaudación municipal por habitante",
      "6.3 Atomización de proyectos",
      "6.4 Resolución de expedientes judiciales",
      "6.5 Conflictividad social",
      "6.6 Victimización por hechos delictivos",
      "6.7 Comisarías en buen estado"
    )
  )
}


#' Catálogo estandarizado de indicadores (código, nombre, pilar, número d.d)
#'
#' Envuelve tu `catalogo_indicador()` existente y devuelve columnas:
#' - codigo: "EDU3", "LAB5", ...
#' - nombre: "3.1 Logro educativo en primaria en lectura", etc.
#' - pilar_cod: "EDU", "LAB", ...
#' - pilar_nom: "Educación", "Laboral", ...
#' - numero: "3.1" (si está disponible en el nombre)
#'
#' @return tibble(codigo, nombre, pilar_cod, pilar_nom, numero)
#' @export
catalogo_indicadores <- function() {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("stringr", quietly = TRUE) ||
      !requireNamespace("tibble", quietly = TRUE)) {
    stop("Faltan paquetes: dplyr, stringr, tibble.")
  }

  cat_raw <- catalogo_indicador()  # <- tu función existente
  # Intentamos detectar nombres de columnas comunes: code/codigo, name/nombre, pilar
  nm <- names(cat_raw)
  col_code <- dplyr::coalesce(
    nm[match("code",   nm)],
    nm[match("codigo", nm)]
  )
  col_name <- dplyr::coalesce(
    nm[match("name",   nm)],
    nm[match("nombre", nm)]
  )
  col_pilar <- dplyr::coalesce(
    nm[match("pilar",  nm)],
    NA_character_
  )

  if (is.na(col_code) || is.na(col_name)) {
    stop("`catalogo_indicador()` debe exponer columnas 'codigo/code' y 'nombre/name'.")
  }

  cat <- cat_raw |>
    dplyr::transmute(
      codigo    = .data[[col_code]],
      nombre    = .data[[col_name]],
      pilar_nom = if (!is.na(col_pilar)) .data[[col_pilar]] else NA_character_
    ) |>
    dplyr::mutate(
      codigo    = stringr::str_squish(as.character(.data$codigo)),
      nombre    = stringr::str_squish(as.character(.data$nombre)),
      pilar_cod = stringr::str_sub(.data$codigo, 1L, 3L)
    )

  # completar pilar_nom si falta, usando catalogo_pilar()
  if (any(is.na(cat$pilar_nom))) {
    if (exists("catalogo_pilar", mode = "function")) {
      cat_p <- catalogo_pilar()
      # detectar nombres de columnas
      nmp <- names(cat_p)
      p_code <- dplyr::coalesce(nmp[match("codigo", nmp)], nmp[match("code", nmp)])
      p_name <- dplyr::coalesce(nmp[match("nombre", nmp)], nmp[match("name", nmp)])
      if (!is.na(p_code) && !is.na(p_name)) {
        cat <- cat |>
          dplyr::left_join(
            dplyr::rename(cat_p, pilar_cod = !!p_code, pilar_nom_ref = !!p_name),
            by = "pilar_cod"
          ) |>
          dplyr::mutate(
            pilar_nom = dplyr::coalesce(.data$pilar_nom, .data$pilar_nom_ref)
          ) |>
          dplyr::select(-dplyr::any_of("pilar_nom_ref"))
      }
    }
  }

  # extraer "d.d" si está al inicio del nombre
  cat <- cat |>
    dplyr::mutate(
      numero = stringr::str_match(.data$nombre, "^(\\d+\\.\\d+)")[, 2]
    )

  tibble::as_tibble(cat)
}

# --- helper para resolver indicador (código o nombre) ---
# devuelve list(codigo, nombre, pilar_cod, pilar_nom, numero)
.resolver_indicador <- function(x) {
  cat <- catalogo_indicadores()
  # 1) match por código
  idx <- which(cat$codigo == x)
  if (length(idx) == 1L) {
    return(as.list(cat[idx, c("codigo","nombre","pilar_cod","pilar_nom","numero")]))
  }
  # 2) match por nombre exacto
  idx <- which(cat$nombre == x)
  if (length(idx) == 1L) {
    return(as.list(cat[idx, c("codigo","nombre","pilar_cod","pilar_nom","numero")]))
  }
  # 3) match por "d.d" (si el usuario pasó solo “3.1 ...”)
  num <- tryCatch(stringr::str_match(x, "^(\\d+\\.\\d+)")[,2], error = function(e) NA_character_)
  if (!is.na(num)) {
    idx <- which(cat$numero == num)
    if (length(idx) == 1L) {
      return(as.list(cat[idx, c("codigo","nombre","pilar_cod","pilar_nom","numero")]))
    }
  }
  stop("No encuentro el indicador '", x, "' en el catálogo.")
}


#' Catálogo de regiones
#' @return tibble con columnas codigo y nombre
#' @export
catalogo_region <- function() {
  tibble::tibble(
    codigo = c("AMZ","ANC","APC","AQP","AYA","CJM","CUS","HVC","HCO","ICA",
               "JUN","LAL","LAM","LIM*","LIMPR","LOR","MDD","MOQ","PAS","PIU",
               "PUN","SNM","TAC","TUM","UCY","PER"),
    nombre = c("Amazonas","Áncash","Apurímac","Arequipa","Ayacucho","Cajamarca",
               "Cusco","Huancavelica","Huánuco","Ica","Junín","La Libertad",
               "Lambayeque","Lima*","Lima Provincias","Loreto","Madre de Dios",
               "Moquegua","Pasco","Piura","Puno","San Martín","Tacna","Tumbes",
               "Ucayali","Perú")
  )
}
