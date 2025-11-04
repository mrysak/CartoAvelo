#' amenagement_departement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_amenagement_departement_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_ac"), "amenagement"),

    # Length of cycling infrastructure - Declined by category
    card(
      card_title(paste0("Linéaire d'aménagements cyclables par type en ", current_year), class = "h4"),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          div(
            style = "width: 101%; margin-left: -20px; margin-top: -40px",
            plotlyOutput(ns("treemap_amenagement_by_category"))
          ),
          div(
            style = "width: 88%; margin-top: 135px;",
            value_box(
              title = div("Aménagements cyclables", style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_value_amenagement")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_evol_amenagement")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
    ),

    # Development of cycling facilities
    card(
      card_title("Evolution de l'offre d'aménagements cyclables depuis 2021", class = "h4"),
      card_body(layout_columns(
        col_widths = c(6,6),

        # Number of kilometers of different types of cycling infrastructure created each year
        card(
          card_title("Linéaire d'aménagements cyclables créé chaque année"),
          card_body(
            plotlyOutput(ns("km_history"), height = "500px"))
        ),

        # Annual evolution rate of cycling infrastructures
        card(
          card_title("Taux d'évolution annuel du linéaire d'aménagements cyclables"),
          card_body(
            div(
              style = "margin-top: -5px;",
              selectInput(ns("selected_amenagement"),
                          label= tags$div("Sélection d'un type d'aménagement", style = "white-space: nowrap;"),
                          choices = setNames(amenagement_types_table()$short, amenagement_types_table()$long),
                          width = "300px")),
            div(
              style = "margin-top: -15px; height: 450px;",
              plotlyOutput(ns("evolution_rate"), height = "100%")
            ))
        )
      )
      )
    ),

    # Evolution - Linéaire de voirie cyclable par habitant ou par km²
    card(
      card_title(textOutput(ns("evolution_title")), class = "h4"),
      card_body(
        div(style = "display: flex; gap: 30px; margin-left: 20px;",
            selectInput(ns("indicator"), "Indicateur à visualiser",
                        choices = setNames(
                          c("voirie_hab", "voirie_km"),
                          c("Linéaire de voirie cyclable par habitant", "Linéaire de voirie cyclable par km²")),
                        width = "360px"),
            selectInput(ns("secure_amenagement"), "Type d'aménagement",
                        choices = setNames(
                          c(FALSE, TRUE),
                          c("Tout", "Aménagements sécurisés (pistes cyclables et voies vertes)")
                        ),
                        width = "360px")
        ),
        plotlyOutput(ns("evolution_voirie")
        )
      )
    ),

    # Cycling infrastructures indicators - Interactive map
    card(
      card_title(textOutput(ns("map_title")), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        leafletOutput(ns("map_amenagement_indicators"), height = "700px", width = "97%"),
        div(
          class = "d-grid gap-3",
          selectInput(ns("indicator_choice"), "Indicateur à visualiser",
                      choices = setNames(
                        c("taux_cyclabilite", "voirie_hab", "voirie_km", "pop_prox"),
                        c("Taux de cyclabilité de la voirie",
                          "Linéaire de voirie cyclable par habitant",
                          "Linéaire de voirie cyclable par km²",
                          "Population vivant à proximité d'une voirie cyclable")
                      ),
                      width = "88%"),
          selectInput(ns("display_typology"), "Affichage par",
                      choices = setNames(c("epci", "commune"),
                                         c("EPCI", "Communes")),
                      width = "88%"),
          conditionalPanel(
            condition = sprintf("['taux_cyclabilite','voirie_hab','voirie_km'].includes(input['%s'])", ns("indicator_choice")),
            selectInput(ns("info_to_display"), "Donnée à afficher",
                        choices = setNames(c("value", "evol"),
                                           c(paste0("Valeur ", current_year),
                                             paste0("Evolution par rapport à ", as.numeric(current_year) - 1))),
                        width = "88%")
          ),

          div(
            style = "width: 88%;",
            value_box(
              title = div(textOutput(ns("vb_title")), style = "text-align: center;"),
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

    # Evolution - Taux de cyclabilité
    card(
      card_title("Evolution du taux de cyclabilité de la voirie", class = "h4"),
      card_body(plotlyOutput(ns("evolution_taux_cyclabilite")))
    ),

    # Average cycleability rate by commune type
    card(
      card_title(paste0("Taux moyen de cyclabilité de la voirie par type de commune en ", current_year), class = "h4"),
      card_body(
        layout_columns(col_widths = c(11),
                       div(style = "margin-top: -25px; margin-left: 35px; ",
                           plotlyOutput(ns("cyclabilite_by_commune_type")))
        )
      )
    ),

    scroll_arrow_ui()
  )
}

#' amenagement_departement Server Functions
#'
#' @noRd
mod_amenagement_departement_server <- function(id, validated_selection, current_year, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Time period with aménagements data
      time_period <- as.character(seq(2021, current_year))

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Filter amenagement data by selected departement
      data_amenagement <- reactive(amenagement_departemental |> filter(INSEE_DEP == validated_selection$departement))

      # Introduction panels
      mod_intro_panel_server("intro_ac", "amenagement",
                             validated_selection, data_cat_terr,
                             amenagement_departemental, "amenagement_cyclable_data")

      # Length of cycling infrastructure - Declined by category
      output$treemap_amenagement_by_category <- renderPlotly({
        plot_treemap_amenagement(data_amenagement(), current_year, amenagement_types_table())
      })

      output$vb_value_amenagement <- renderText({
        km <- data_amenagement() |> select(all_of(paste0("NB_TOTAL_", current_year))) |> pull()

        paste0(format(round(km, 0), big.mark = " "), " km")
      })

      output$vb_evol_amenagement <- renderText({
        current <- data_amenagement() |> select(all_of(paste0("NB_TOTAL_", current_year))) |> pull()
        prev <- data_amenagement() |> select(all_of(paste0("NB_TOTAL_", as.numeric(current_year) - 1))) |> pull()
        evol <- current - prev

        paste0(if (evol >= 0) "+", format(round(evol, 0), big.mark = " "), " km par rapport à ", as.numeric(current_year) - 1)
      })

      # Number of kilometers of different types of cycling infrastructure created each year
      output$km_history <- renderPlotly({
        plot_km_history(data_amenagement())
      })

      # Annual evolution rate of cycling infrastructures
      output$evolution_rate <- renderPlotly({
        data <- generate_evolution_rate_local(data_cat_terr, amenagement_departemental, "departement",
                                              validated_selection$departement, territory_name(), time_period,
                                              input$selected_amenagement, amenagement_types_table())

        plot_annual_evolution_rate(data$data_to_display, data$lib_category, territory_name())
      })

      # Evolution - Linéaire de voirie cyclable par habitant ou par km²
      output$evolution_title <- renderText({
        paste0("Evolution du linéaire de voirie cyclable par ",
               switch(input$indicator,
                      voirie_hab = "habitant",
                      voirie_km = "km²"))
      })

      output$evolution_voirie <- renderPlotly({
        comparison_data <- generate_comparison_data_local(data_cat_terr, amenagement_departemental, "departement",
                                                          input$indicator, validated_selection$departement, time_period,
                                                          input$secure_amenagement)

        data_comparison_cat <- comparison_data$data_comparison_cat
        lib_category <- comparison_data$lib_category

        plot_evolution_amenagement(data_amenagement(), data_comparison_cat, input$indicator,
                                   territory_name(), time_period, "departement", lib_category,
                                   input$secure_amenagement)
      })

      # Cycling infrastructures indicators - Interactive map
      output$map_title <- renderText({
        req(input$indicator_choice)

        label <- switch(input$indicator_choice,
                        "taux_cyclabilite" = "Taux de cyclabilité de la voirie",
                        "voirie_hab"       = "Linéaire de voirie cyclable par habitant",
                        "voirie_km"        = "Linéaire de voirie cyclable par km²",
                        "pop_prox"         = "Population vivant à proximité d'une voirie cyclable"
        )

        paste0(label, " en ", current_year)
      })

      output$map_amenagement_indicators <- renderLeaflet({

        req(input$display_typology)

        if (input$display_typology == "epci") {
          amenagement_data <- amenagement_epci
        } else if (input$display_typology == "commune") {
          amenagement_data <- amenagement_communal
        }

        # Lazy loading
        admin_sf <- load_admin_sf(input$display_typology, admin_cache)

        map_amenagement_indicators(data_cat_terr, amenagement_data, admin_sf,
                                   "departement", input$display_typology, validated_selection$departement, input$indicator_choice,
                                   if (input$indicator_choice == "pop_prox") "value" else input$info_to_display,
                                   current_year)
      })

      value_box_content <- reactive(value_box_content_amenagement(data_amenagement(), current_year, input$indicator_choice))

      output$vb_title <- renderText({
        req(input$indicator_choice)
        value_box_content()$title
      })

      output$vb_chiffre_cle <- renderText({
        req(input$indicator_choice)
        value_box_content()$chiffre_cle
      })

      output$vb_evol_cle <- renderText({
        req(input$indicator_choice)
        value_box_content()$evol_cle
      })

      # Evolution - Taux de cyclabilité
      output$evolution_taux_cyclabilite <- renderPlotly({
        comparison_data <- generate_comparison_data_local(data_cat_terr, amenagement_departemental, "departement",
                                                          "taux_cyclabilite", validated_selection$departement, time_period)

        data_comparison_cat <- comparison_data$data_comparison_cat
        lib_category <- comparison_data$lib_category

        plot_evolution_amenagement(data_amenagement(), data_comparison_cat, "taux_cyclabilite",
                                   territory_name(), time_period, "departement", lib_category)
      })

      # Average cycleability rate by commune type
      output$cyclabilite_by_commune_type <- renderPlotly({
        data_cat_terr_filtered <- data_cat_terr |> filter(DEP == validated_selection$departement)
        plot_cyclabilite_by_commune_type(amenagement_communal, data_cat_terr_filtered, current_year, "departement")
      })

    }
  )
}

## To be copied in the UI
# mod_amenagement_departement_ui("amenagement_departement_1")

## To be copied in the server
# mod_amenagement_departement_server("amenagement_departement_1")
