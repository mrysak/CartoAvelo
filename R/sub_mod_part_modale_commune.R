#' part_modale_commune UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_part_modale_commune_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_pm"), "part_modale"),

    # Evolution - Modal Share
    card(
      card_title(textOutput(ns("evolution_title")), class = "h4"),
      card_body(
        div(style = "margin-left: 20px;",
            selectInput(ns("type_transport_evol"), "Mode de transport",
                        choices = setNames(c("velo", "pied", "car", "tcommun"),
                                           c("Vélo", "Marche", "Voiture", "Transports en commun")),
                        width = "30%")
        ),
        div(
          id = "plot-container",
          plotlyOutput(ns("evolution_part_modale"), height = "450px"),
          div(
            id = "valuebox-overlay",
            style = "width: 25%;",
            value_box(
              title = div(uiOutput(ns("vb_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_evol_cle")), style = "text-align: center;"),
              class = "mt-auto")
          )
        )
      )
    ),

    # Distribution by commune density
    card(
      card_title(paste0("Distribution des parts modales des différents modes de transport en ", current_year), class = "h4"),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          div(
            style = "margin-left: 10%;",
            plotlyOutput(ns("pm_by_milieu"), height = "170px")
          ),
          # Legend
          div(
            style = "text-align:left; margin-right:5%; margin-top: 9%;",
            HTML("<div style='display:inline-block; text-align:left;'>
                    <b style='display:block; margin-bottom:5px;'>Mode de transport</b>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#C6CE41; margin-right:5px;'></div>
                      Vélo
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#B1D6E4; margin-right:5px;'></div>
                      Marche
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#294754; margin-right:5px;'></div>
                      Transports en commun
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#8e8f91; margin-right:5px;'></div>
                      Voiture
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#cacaca; margin-right:5px;'></div>
                      Autres
                    </div>
                  </div>
              ")
          )
        )
      )
    ),

    # Distribution by gender
    card(
      card_title(paste0("Distribution par sexe des parts modales des différents modes de transport en ", current_year), class = "h4"),
      card_body(
        plotlyOutput(ns("pm_by_gender"), height = "150px"),
        # Legend
        div(
          style = "text-align:right; margin-right:2%; margin-top: -1.3%;",
          HTML("<div style='display:inline-block; text-align:left;'>
                    <b style='display:block; margin-bottom:5px;'>Genre</b>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#e0e0e0; margin-right:5px;'></div>
                      Hommes
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#8e8f91; margin-right:5px;'></div>
                      Femmes
                    </div>
                  </div>
              ")
        )
      )
    ),

    # Modal share by age group
    card(
      card_title(paste0("Part modale des différents modes de transport selon la tranche d'âge en ", current_year), class = "h4"),
      card_body(
        div(style = "display: flex; margin-right: 20px;",
            div(
              style = "flex : 4.2;",
              plotlyOutput(ns("pm_by_age_velo"), height = "170px")
            ),
            div(
              style = "flex : 2.6;",
              plotlyOutput(ns("pm_by_age_pied"), height = "170px")
            ),
            div(
              style = "flex : 2.6;",
              plotlyOutput(ns("pm_by_age_tcommun"), height = "170px")
            ),
            div(
              style = "flex : 2.6;",
              plotlyOutput(ns("pm_by_age_car"), height = "170px")
            )
        )
      )
    ),

    scroll_arrow_ui()
  )
}



#' part_modale_commune Server Functions
#'
#' @noRd
mod_part_modale_commune_server <- function(id, validated_selection, current_year) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_pm", "part_modale",
                             validated_selection, data_cat_terr,
                             part_modale, "part_modale_data")

      # Evolution - Modal Share
      output$evolution_title <- renderText({
        label <- switch(input$type_transport_evol,
                        "velo"    = "du vélo",
                        "pied"    = "de la marche",
                        "car"     = "de la voiture",
                        "tcommun" = "des transports en commun"
        )
        paste0("Évolution de la part modale ", label, " dans les déplacements domicile-travail (2018-", current_year, ")")
      })

      output$evolution_part_modale <- renderPlotly({
        data_evolution <- generate_pm_comparison_data_local(data_cat_terr, part_modale, current_year, 2018,
                                                            input$type_transport_evol, "commune", validated_selection$commune)

        plot_evolution_part_modale(part_modale |> filter(insee_niveau == "COM" & insee_code == validated_selection$commune),
                                   data_evolution$data_comparison_cat, input$type_transport_evol, 2018, current_year,
                                   territory_name(), "commune", data_evolution$lib_category)
      })

      # Value box
      value_box_content <- reactive(value_box_content_pm(part_modale, current_year, "commune", validated_selection$commune, input$type_transport_evol))

      output$vb_title <- renderUI({HTML(value_box_content()$title)})
      output$vb_chiffre_cle <- renderText({value_box_content()$value})
      output$vb_evol_cle <- renderText({value_box_content()$evol})

      # Distribution by commune density
      output$pm_by_milieu <- renderPlotly({
        data_plot <- pm_distribution_by_milieu(part_modale, data_cat_terr, current_year, "commune", validated_selection$commune)
        plot_pm_distribution_by_milieu_departement(data_plot)
      })

      # Distribution by gender
      output$pm_by_gender <- renderPlotly({
        plot_distrib_gender_pm(distrib_gender_pm, data_cat_terr, "commune", validated_selection$commune, territory_name())
      })

      # Modal share by age group
      output$pm_by_age_velo <- renderPlotly({
        plot_distrib_age_pm(distrib_age_pm, data_cat_terr, "commune", validated_selection$commune, territory_name(), "velo")
      })

      output$pm_by_age_pied <- renderPlotly({
        plot_distrib_age_pm(distrib_age_pm, data_cat_terr, "commune", validated_selection$commune, territory_name(), "pied")
      })

      output$pm_by_age_tcommun <- renderPlotly({
        plot_distrib_age_pm(distrib_age_pm, data_cat_terr, "commune", validated_selection$commune, territory_name(), "tcommun")
      })

      output$pm_by_age_car <- renderPlotly({
        plot_distrib_age_pm(distrib_age_pm, data_cat_terr, "commune", validated_selection$commune, territory_name(), "car")
      })

    }
  )
}

## To be copied in the UI
# mod_part_modale_commune_ui("part_modale_commune_1")

## To be copied in the server
# mod_part_modale_commune_server("part_modale_commune_1")
