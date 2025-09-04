#' @importFrom utils modifyList
.onLoad <- function(libname, pkgname) {
  default_groups <- list(
    gr_costa = c("Tumbes","Piura","Lambayeque","La Libertad","Áncash",
                 "Lima*","Ica","Arequipa","Moquegua","Tacna", "Lima Provincias"),
    gr_sierra = c("Cajamarca","Huánuco","Pasco","Junín","Huancavelica",
                  "Ayacucho","Apurímac","Cusco","Puno","Áncash", "La Libertad"),
    gr_selva  = c("Amazonas","Loreto","San Martín","Ucayali","Madre de Dios"),
    gr_norte  = c("Tumbes","Piura","Lambayeque","La Libertad","Cajamarca",
                  "Amazonas","San Martín","Áncash"),
    gr_sur    = c("Ayacucho","Apurímac","Cusco","Puno","Arequipa",
                  "Moquegua","Tacna","Ica")
  )
  incorer_set_region_groups(default_groups, replace = FALSE)
}
