#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {

  # Reactive storage of the selected territory (after clicking on the
  # validate_territory_selection action button) for each typology
  validated_selection <- reactiveValues(
    france = NULL,
    region = NULL,
    departement = NULL,
    epci = NULL,
    commune = NULL,
    last_validated_typology = "france"  # Default : France
  )

  admin_cache <- reactiveValues()
  veloroute_cache <- reactiveValues(data = NULL)

  # Général tab
  mod_general_server("general", validated_selection = validated_selection, admin_cache, veloroute_cache,
                     current_year_ac, current_year_av, current_year_fr, current_year_vr, current_year_pm,
                     current_year_st)

  output$general_ui <- renderUI({
    req(input$navbar == "Général")
    mod_general_ui("general")
  })


  # AVELO tab
  mod_avelo_server("avelo")

  output$avelo_ui <- renderUI({
    req(input$navbar == "AVELO")
    mod_avelo_ui("avelo")
  })

}


