#' stationnement_commune UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stationnement_commune_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_st"), "stationnement"),

    # Evolution - Parking spots for 1000 residents
    card(
      card_title(paste0("Évolution du nombre de stationnements vélo pour 1 000 habitants (2022-", current_year, ")"), class = "h4"),
      card_body(
        div(
          id = "plot-container",
          plotlyOutput(ns("evolution_stat_hab"), height = "450px"),
          div(
            id = "valuebox-overlay",
            style = "width: 25%;",
            value_box(
              title = div(uiOutput(ns("vb_stat_hab_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_stat_hab_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_stat_hab_evol_cle")), style = "text-align: center;"),
              class = "mt-auto")
          )
        ),
        tags$p(
          "Seules les communes disposant d’au moins une donnée non nulle pour l'indicateur sur l’ensemble des années considérées ont été incluses dans l’analyse.",
          style = "font-style: italic; font-size: 0.8em; margin-top: -15px; margin-left: 10px",
        )
      )
    ),

    # Evolution - Parking spots per km of cycling infrastructure
    card(
      card_title(paste0("Évolution du nombre de stationnements vélo par kilomètre d'aménagement cyclable (2022-", current_year, ")"), class = "h4"),
      card_body(
        div(
          id = "plot-container",
          plotlyOutput(ns("evolution_stat_km"), height = "450px"),
          div(
            id = "valuebox-overlay",
            style = "width: 25%;",
            value_box(
              title = div(uiOutput(ns("vb_stat_km_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_stat_km_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_stat_km_evol_cle")), style = "text-align: center;"),
              class = "mt-auto")
          )
        ),
        tags$p(
          "Seules les communes disposant d’au moins une donnée non nulle pour l'indicateur sur l’ensemble des années considérées ont été incluses dans l’analyse.",
          style = "font-style: italic; font-size: 0.8em; margin-top: -15px; margin-left: 10px",
        )
      )
    ),

    scroll_arrow_ui()
  )
}



#' stationnement_commune Server Functions
#'
#' @noRd
mod_stationnement_commune_server <- function(id, validated_selection, current_year) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_st", "stationnement",
                             validated_selection, data_cat_terr,
                             stationnement_communal, "stationnement_data")

      # Filter stationnement data by selected commune
      filtered_data <- reactive(stationnement_communal |> filter(INSEE_COM == validated_selection$epci))

      # Evolution - Parking spots for 1 000 residents
      filtered_data <- reactive(stationnement_communal |> filter(INSEE_COM == validated_selection$commune))

      output$evolution_stat_hab <- renderPlotly({
        data_comparison <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_communal,
                                                                        "stat_hab", "commune", validated_selection$commune,
                                                                        2022, current_year)
        data_comparison_cat <- data_comparison$data_comparison_cat
        lib_category <- data_comparison$lib_category

        plot_evolution_stationnement(filtered_data(), data_comparison_cat, "stat_hab", territory_name(),
                                     2022, current_year, "commune", lib_category)
      })

      vb_stat_hab_content <- reactive(value_box_content_stationnement(filtered_data(), current_year, "stat_hab"))

      output$vb_stat_hab_title <- renderUI({HTML(vb_stat_hab_content()$title)})
      output$vb_stat_hab_chiffre_cle <- renderText({vb_stat_hab_content()$value})
      output$vb_stat_hab_evol_cle <- renderText({vb_stat_hab_content()$evol})

      # Evolution - Parking spots per km of cycling infrastructure
      output$evolution_stat_km <- renderPlotly({
        data_comparison <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_communal,
                                                                        "stat_km", "commune", validated_selection$commune,
                                                                        2022, current_year)
        data_comparison_cat <- data_comparison$data_comparison_cat
        lib_category <- data_comparison$lib_category

        plot_evolution_stationnement(filtered_data(), data_comparison_cat, "stat_km", territory_name(),
                                     2022, current_year, "commune", lib_category)
      })

      vb_stat_km_content <- reactive(value_box_content_stationnement(filtered_data(), current_year, "stat_km"))

      output$vb_stat_km_title <- renderUI({HTML(vb_stat_km_content()$title)})
      output$vb_stat_km_chiffre_cle <- renderText({vb_stat_km_content()$value})
      output$vb_stat_km_evol_cle <- renderText({vb_stat_km_content()$evol})

    }
  )
}

## To be copied in the UI
# mod_stationnement_commune_ui("stationnement_commune_1")

## To be copied in the server
# mod_stationnement_commune_server("stationnement_commune_1")
