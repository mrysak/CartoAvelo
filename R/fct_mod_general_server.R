#' @title Update selectize input choices for a given typology
#' @description This function updates a selectize input control with the list of territories
#' corresponding to the selected typology (e.g. region, departement, epci, commune). It retrieves
#' choices dynamically from the provided dataset and sets the input with an empty selection by default.
#'
#' @param typology A character string specifying the typology to update. Should be one of "france", "region", "departement", "epci", or "commune".
#' @param input_id The ID of the input to update
#' @param session The shiny session object
#'
#' @return None. Updates input in place.
#' @import shiny
#'
#' @noRd
update_typology_selectize <- function(typology, input_id, session, data_cat_terr) {
  updateSelectizeInput(
    session,
    input_id,
    choices = extract_select_list_territories(typology, data_cat_terr),
    selected = "",
    server = TRUE
  )
}



#' @title Get the last validated typology from reactiveValues
#'
#' @description This function returns the last validated typology stored in a reactiveValues
#' object within a Shiny application. It is designed to be called from within the
#' server logic, by passing the reactiveValues object as an argument.
#'
#' @param validated_selection A `reactiveValues` object containing a field
#'   `last_validated_typology` representing the typology last validated by the user.
#'
#' @return A character string indicating the last validated typology (e.g. "france", "region", "departement", "epci", "commune").
#'
#' @examples
#' # Inside server
#' typology <- get_validated_typology(validated_selection)
get_validated_typology <- function(validated_selection){
  validated_selection$last_validated_typology
}
