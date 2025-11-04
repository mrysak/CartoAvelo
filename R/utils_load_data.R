#' @title Conditionally load administrative spatial data with caching
#'
#' @description This function loads an administrative geographic layer (commune, EPCI, department, or region)
#' either from a GeoJSON file or from a preloaded object, and caches it in a `reactiveValues` store
#' to avoid repeated disk reads and improve Shiny app performance.
#'
#' @param typology `character` — The typology of administrative layer to load. Must be one of:
#' `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param cache `reactiveValues` — A shared reactive cache used across Shiny modules.
#' The loaded layers are stored here and reused if already present.
#'
#' @return An `sf` object corresponding to the requested administrative layer.
#' It is either read from a `.json` file (for `commune` or `epci`) or retrieved
#' from an in-memory object (`departement` or `region`).
#'
#' @importFrom sf read_sf
#' @noRd
load_admin_sf <- function(typology, cache) {

  if (!is.null(cache[[typology]])) {
    return(cache[[typology]])
  }

  sf_data <- switch(typology,
                    "commune" = sf::read_sf("inst/json_files/admin_commune.json", quiet = TRUE),
                    "epci" = sf::read_sf("inst/json_files/admin_epci.json", quiet = TRUE),
                    "departement" = admin_departement,
                    "region" = admin_region)

  cache[[typology]] <- sf_data
  sf_data
}




load_file_sf <- function(cache, file_name) {

  if (is.null(cache$data)) {
    sf_data <- sf::read_sf(paste0("inst/json_files/", file_name), quiet = TRUE)
    cache$data <- sf_data
  }

  return(cache$data)
}

