#' accueil_velo_region UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gt
mod_accueil_velo_region_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_av"), "accueil_velo"),

    # Count of Accueil Vélo services and ratio by km of cycle route / Density - Interactive map
    card(
      card_title(textOutput(ns("map_title")), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        leafletOutput(ns("map_services_count"), height = "730px", width = "97%"),
        div(
          class = "d-grid gap-3",
          selectInput(ns("indicator_choice"), "Indicateur à visualiser",
                      choices = setNames(c("count", "ratio", "density"),
                                         c("Nombre total de prestations Accueil Vélo", "Nombre de prestations Accueil Vélo par kilomètre de véloroute traversant le territoire", "Densité des prestations Accueil Vélo à proximité des véloroutes nationales")),
                      width = "86%"),
          selectInput(ns("selected_type"), "Type de prestation",
                      choices = setNames(presta_types_table()$short, presta_types_table()$long),
                      width = "86%"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'ratio'", ns("indicator_choice")),
            selectInput(ns("cycle_route_type"), "Véloroutes incluses dans le périmètre d'analyse",
                        choices = c("SNV", "SRV"),
                        width = "86%")
          ),
          conditionalPanel(
            condition = paste0("['count','ratio'].indexOf(input['", ns("indicator_choice"), "']) !== -1"),
            selectInput(ns("info_to_display"), "Donnée à afficher",
                        choices = setNames(c("value", "evol"), c(paste0("Valeur ", current_year), paste0("Evolution par rapport à ", as.numeric(current_year) - 1))),
                        width = "86%")
          ),
          div(
            style = "width: 86%;",
            value_box(
              title = div(textOutput(ns("vb_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_chiffre_cle")),
                               style = "font-size: 2.5rem; font-weight: bold; align-self: center !important;"),
              div(textOutput(ns("vb_evol_cle")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
      )
    ),

    # Count of Accueil Vélo services - Declined by category
    card(
      card_title(paste0("Nombre de prestations Accueil Vélo selon la catégorie en ", current_year), class = "h4"),
      card_body(
        gt_output(ns("table_services_by_category"))
      )
    ),

    # Evolution - Count of Accueil Vélo services
    card(
       card_title("Evolution du nombre de prestations Accueil Vélo", class = "h4"),
       card_body(
         layout_columns(
                 col_widths = c(8, 3),
                 plotlyOutput(ns("evolution_nb_services")),
                 div(
                   style = "margin-left: 20px;",
                   selectInput(ns("type_for_evol"), "Type de prestation",
                               choices = setNames(presta_types_table()$short, presta_types_table()$long))
                 )
          )
       )
    ),

    # Accueil Vélo services per itinerary
    card(
      card_title(paste0("Prestations Accueil Vélo des véloroutes traversant le territoire en ", current_year), class = "h4"),
      card_body(layout_columns(
        col_widths = c(5, 7),
        plotlyOutput(ns("services_per_itinerary_nat")),
        plotlyOutput(ns("services_per_itinerary_reg"))
      ),
      tags$p(
        "Affichage limité à 8 véloroutes en simultané : les véloroutes comportant le plus de prestations Accueil Vélo sont affichées.",
        style = "font-style: italic; font-size: 0.8em; margin-top: -15px; margin-left: 10px",
      )
      )
    )
  )
}



#' accueil_velo_region Server Functions
#'
#' @noRd
mod_accueil_velo_region_server <- function(id, validated_selection, current_year) {
  moduleServer(
    id,
    function(input, output, session) {

      # Time period with Accueil Vélo data
      time_period <- as.character(seq(2022, current_year))

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Filter data by selected region
      filtered_data <- reactive(accueil_velo_regional |> filter(INSEE_REG == validated_selection$region))
      data_reg <- reactive(accueil_velo_regional_iti |> filter(INSEE_REG == validated_selection$region))

      # Introduction panels
      mod_intro_panel_server("intro_av", "accueil_velo",
                             validated_selection, data_cat_terr,
                             accueil_velo_regional, "accueil_velo_data")

      # Count of Accueil Vélo services and ratio by km of cycle route / Density - Interactive map
      output$map_title <- renderText({
        req(input$indicator_choice)

        if (input$indicator_choice == "count"){
          paste0("Nombre de prestations Accueil Vélo en ", current_year)
        } else if (input$indicator_choice == "ratio"){
          paste0("Nombre de prestations Accueil Vélo par kilomètre de véloroute traversant le territoire en ", current_year)
        } else if (input$indicator_choice == "density"){
          paste0("Densité des prestations Accueil Vélo à proximité des véloroutes nationales et régionales (5 km) en ", current_year)
        }
      })

      output$map_services_count <- renderLeaflet({
        if (input$indicator_choice %in% c("ratio", "count")){
          map_services_count(data_cat_terr, accueil_velo_departemental, admin_departement, "INSEE_DEP",
                             as.numeric(current_year), presta_types_table(), "region", validated_selection$region,
                             input$selected_type, input$indicator_choice,
                             input$info_to_display, input$cycle_route_type,
                             "departement", session$ns)
        } else if (input$indicator_choice == "density"){
          density_sf = sf::st_read("inst/json_files/accueil_velo_regional_grille5km.json", quiet = TRUE) |>
            filter(INSEE_REG == validated_selection$region)

          map_services_density(density_sf, admin_region, "region",
                               validated_selection$region, input$selected_type,
                               current_year, presta_types_table())
        }

      })

      # Value boxes
      value_box_content<- reactive(value_box_content_accueil_velo(filtered_data(), presta_types_table(), "region", current_year,
                                                                  input$indicator_choice, input$selected_type, input$cycle_route_type))

      output$vb_title <- renderText({value_box_content()$title})
      output$vb_chiffre_cle <- renderText({value_box_content()$value})
      output$vb_evol_cle <- renderText({value_box_content()$evol})

      # Count of Accueil Vélo services - Declined by category
      output$table_services_by_category <- render_gt({

        table_services <- prepare_regional_data(
            accueil_velo_departemental,
            accueil_velo_national,
            data_cat_terr,
            presta_types_table(),
            current_year,
            validated_selection$region
        )

        generate_services_count_table(table_services, "region", color_low = "#F6F9ED", color_high = "#C6CE41", font_size = 12, font_family = "Poppins")

      })

      # Evolution - Count of Accueil Vélo services
      output$evolution_nb_services <- renderPlotly({
        plot_evolution_nb_services(filtered_data(), input$type_for_evol, time_period, territory_name())
      })

      # Accueil Vélo services per itinerary
      output$services_per_itinerary_nat <- renderPlotly({
        # Select national itineraries sections crossing the region and that have at least 10 services
        data_iti <- data_reg() |>
          filter(SCHEMA == "EV/SNV") |>
          filter(!!sym(paste0("NB_TOTAL_", as.character(current_year))) >= 10)

        plot_services_per_itinerary(data_iti, "Prestations des véloroutes nationales", current_year)
      })

      output$services_per_itinerary_reg <- renderPlotly({
        # Select regional itineraries that have at least 10 services
        data_iti <- data_reg() |>
          filter(SCHEMA == "SRV") |>
          filter(!!sym(paste0("NB_TOTAL_", as.character(current_year))) >= 10)

        plot_services_per_itinerary(data_iti, "Prestations des véloroutes régionales", current_year)
      })

      # Download button
      output$download <- downloadHandler(
        filename = function() {
          paste0("accueil_velo_data_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(accueil_velo_regional, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )

    }
  )
}

## To be copied in the UI
# mod_accueil_velo_region_ui("accueil_velo_region_1")

## To be copied in the server
# mod_accueil_velo_region_server("accueil_velo_region_1")
