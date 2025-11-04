#' stationnement_departement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_stationnement_departement_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_st"), "stationnement"),

    # Indicators on bicycle parking - Interactive map
    card(
      card_title(textOutput(ns("map_title")), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        leafletOutput(ns("map_stationnement"), height = "700px", width = "97%"),
        div(
          class = "d-grid gap-3",
          selectInput(ns("indicator_choice"), "Indicateur à visualiser",
                      choices = setNames(c("stat_hab", "stat_km"),
                                         c("Nombre de stationnements vélo pour 1 000 habitants",
                                           "Nombre de stationnements vélo par kilomètre d'aménagement cyclable")),
                      width = "88%"),
          selectInput(ns("display_typology"), "Affichage par",
                      choices = setNames(c("epci", "commune"),
                                         c("EPCI", "Communes")),
                      width = "88%"),
          selectInput(ns("info_to_display"), "Donnée à afficher",
                      choices = setNames(c("value", "evol"),
                                         c(paste0("Valeur ", current_year),
                                           paste0("Evolution par rapport à ", as.numeric(current_year) - 1))),
                      width = "88%"),

          div(
            style = "width: 88%;",
            value_box(
              title = div(uiOutput(ns("vb_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_evol_cle")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
      )
    ),

    # Evolution - Parking spots for 1000 residents
    card(
      card_title(paste0("Évolution du nombre de stationnements vélo pour 1 000 habitants (2022-", current_year, ")"), class = "h4"),
      card_body(
        div(style = "margin-left: 5px;",
            plotlyOutput(ns("evolution_stat_hab"))
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
        div(style = "margin-left: 5px;",
            plotlyOutput(ns("evolution_stat_km"))
        ),
        tags$p(
          "Seules les communes disposant d’au moins une donnée non nulle pour l'indicateur sur l’ensemble des années considérées ont été incluses dans l’analyse.",
          style = "font-style: italic; font-size: 0.8em; margin-top: -15px; margin-left: 10px",
        )
      )
    ),

    # Density of bicycle parking by commune density
    card(
      card_title(paste0("Répartition des communes par densité de stationnement (pour 1 000 habitants) et par type de commune en ", current_year), class = "h4"),
      card_body(
        div(
          style = "display: flex; gap: 20px; align-items: flex-end; margin-top: -2%",
          div(
            style = "flex: 8.5;",
            plotlyOutput(ns("stationnement_density"), height = "375px")
          ),
          div(
            style = "flex: 3.5; display: flex; justify-content: center; align-items: flex-end;",
            div(
              style = "width: 88%; margin-bottom: 5%; margin-left: -4%",
              value_box(
                title = div(uiOutput(ns("vb_total_title")), style = "text-align: center;"),
                value = tags$div(textOutput(ns("vb_total_chiffre_cle")),
                                 style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
                div(textOutput(ns("vb_total_evol_cle")), style = "text-align: center;"),
                class = "mt-auto"
              )
            )
          )
        ),
        # Legend
        tags$div(
          style = "display: flex; justify-content: left; align-items: left; gap: 25px; margin-top: -15px; font-size: 13px; margin-left : 20px;  color: #294754;",
          tags$div(
            style = "display: flex; flex-direction: column; line-height: 1.1;",
            tags$b("Densité du stationnement"),
            tags$span("(places/1 000 hab)", style = "font-weight: normal; font-size: 12px;")
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #E3E5F4; border-radius: 3px;"),
            "0 à 1"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #B6B9E4; border-radius: 3px;"),
            "1 à 3"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #898ED3; border-radius: 3px;"),
            "3 à 5"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #5B64C3; border-radius: 3px;"),
            "5 à 15"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #303876; border-radius: 3px;"),
            "15 à 50"
          ),
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            tags$div(style = "width: 22px; height: 14px; background-color: #1D2354; border-radius: 3px;"),
            "Plus de 50"
          )
        )
      )
    ),

    scroll_arrow_ui()
  )
}



#' stationnement_departement Server Functions
#'
#' @noRd
mod_stationnement_departement_server <- function(id, validated_selection, current_year, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_st", "stationnement",
                             validated_selection, data_cat_terr,
                             stationnement_departemental, "stationnement_data")

      # Filter stationnement data by selected departement
      filtered_data <- reactive(stationnement_departemental |> filter(INSEE_DEP == validated_selection$departement))

      # Indicators on bicycle parking - Interactive map
      output$map_title <- renderText({
        req(input$indicator_choice)

        label <- switch(input$indicator_choice,
                        "stat_hab" = "Nombre de stationnements vélo pour 1 000 habitants",
                        "stat_km"  = "Nombre de stationnements vélo par kilomètre d'aménagement cyclable"
        )

        paste0(label, " en ", current_year)
      })

      output$map_stationnement <- renderLeaflet({

        if (input$display_typology == "epci") {
          admin_sf <- load_admin_sf(input$display_typology, admin_cache)
          stationnement_data <- stationnement_epci
        } else if (input$display_typology == "commune") {
          admin_sf <- load_admin_sf(input$display_typology, admin_cache)
          stationnement_data <- stationnement_communal
        }

        map_stationnement_indicators(data_cat_terr, stationnement_data, admin_sf,
                                     "departement", input$display_typology, validated_selection$departement, input$indicator_choice,
                                     input$info_to_display, current_year, admin_cache, session$ns)
      })

      # Value boxes
      value_box_content <- reactive(value_box_content_stationnement(filtered_data(), current_year, input$indicator_choice))

      output$vb_title <- renderUI({HTML(value_box_content()$title)})
      output$vb_chiffre_cle <- renderText({value_box_content()$value})
      output$vb_evol_cle <- renderText({value_box_content()$evol})

      # Evolution - Parking spots for 1 000 residents
      output$evolution_stat_hab <- renderPlotly({
        data_comparison <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_departemental,
                                                                        "stat_hab", "departement", validated_selection$departement,
                                                                        2022, current_year)
        data_comparison_cat <- data_comparison$data_comparison_cat
        lib_category <- data_comparison$lib_category

        plot_evolution_stationnement(filtered_data(), data_comparison_cat, "stat_hab", territory_name(),
                                     2022, current_year, "departement", lib_category)
      })

      # Evolution - Parking spots per km of cycling infrastructure
      output$evolution_stat_km <- renderPlotly({
        data_comparison <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_departemental,
                                                                        "stat_km", "departement", validated_selection$departement,
                                                                        2022, current_year)
        data_comparison_cat <- data_comparison$data_comparison_cat
        lib_category <- data_comparison$lib_category

        plot_evolution_stationnement(filtered_data(), data_comparison_cat, "stat_km", territory_name(),
                                     2022, current_year, "departement", lib_category)
      })

      # Density of bicycle parking by commune density
      output$stationnement_density <- renderPlotly({
        plot_stationnement_density(stationnement_communal, data_cat_terr,
                                   "departement", validated_selection$departement, territory_name(), current_year)
      })

      # Value boxe - Total number of parking spots
      vb_total_content <- reactive(value_box_content_stationnement(filtered_data(), current_year, "nb_stat"))

      output$vb_total_title <- renderUI({HTML(vb_total_content()$title)})
      output$vb_total_chiffre_cle <- renderText({vb_total_content()$value})
      output$vb_total_evol_cle <- renderText({vb_total_content()$evol})

    }
  )
}

## To be copied in the UI
# mod_stationnement_departement_ui("stationnement_departement_1")

## To be copied in the server
# mod_stationnement_departement_server("stationnement_departement_1")
