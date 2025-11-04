#' frequentation_epci UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_frequentation_epci_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_fr"), "freq"),

    br(),

    tags$div(
      "Cet échelon territorial n'est pas inclus dans les analyses de fréquentation vélo",
      style = "
      display: flex;
      justify-content: center;
      font-size: 18px;
    "
    )


  )
}

#' frequentation_region Server Functions
#'
#' @noRd
mod_frequentation_epci_server <- function(id, validated_selection, current_year, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Introduction panels
      mod_intro_panel_server("intro_fr", "freq",
                             validated_selection, data_cat_terr,
                             DATA, "frequentation_data")


    }
  )
}

## To be copied in the UI
# mod_frequentation_epci_ui("frequentation_epci_1")

## To be copied in the server
# mod_amenagement_epci_server("amenagement_epci_1")
