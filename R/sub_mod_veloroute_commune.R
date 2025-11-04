#' veloroute_commune UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_veloroute_commune_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_vr"), "veloroute"),

    tags$div(
      "Cet échelon territorial n'est pas inclus dans les analyses de véloroutes",
      style = "
      display: flex;
      justify-content: center;
      font-size: 18px;
    "
    )
  )
}

#' veloroute_commune Server Functions
#'
#' @noRd
mod_veloroute_commune_server <- function(id, validated_selection, current_year, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_vr", "veloroute",
                             validated_selection, data_cat_terr,
                             veloroute_national, "veloroute_data")
    }
  )
}

## To be copied in the UI
# mod_veloroute_commune_ui("veloroute_commune_1")

## To be copied in the server
# mod_veloroute_commune_server("veloroute_commune_1")
