#' accueil_velo_departement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accueil_velo_departement_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_av"), "accueil_velo"),

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
                 col_widths = c(8, 4),
                 plotlyOutput(ns("evolution_nb_services")),
                 div(
                   style = "margin-left: 20px;",
                   selectInput(ns("type_for_evol"), "Type de prestation",
                               choices = setNames(presta_types_table()$short, presta_types_table()$long), width = "88%"),
                   div(
                     style = "margin-top: 20%; width: 88%;",
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
    ),

    # Density of Accueil Vélo services around cycle routes - Interactive map
    card(
      card_title(paste0("Densité des prestations Accueil Vélo à proximité des véloroutes nationales et régionales (5 km) en ", current_year), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        leafletOutput(ns("map_services_density"), height = "700px", width = "97%"),
        div(
          class = "d-grid gap-3",
          selectInput(ns("selected_type_density"), "Type de prestation",
                      choices = setNames(presta_types_table()$short, presta_types_table()$long),
                      width = "86%"),
        )
      )
      )
    )
  )
}



#' accueil_velo_departement Server Functions
#'
#' @noRd
mod_accueil_velo_departement_server <- function(id, validated_selection, current_year) {
  moduleServer(
    id,
    function(input, output, session) {

      # Time period with Accueil Vélo data
      time_period <- as.character(seq(2022, current_year))

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Filter data by selected departement
      filtered_data <- reactive(accueil_velo_departemental |> filter(INSEE_DEP == validated_selection$departement))
      data_dep <- reactive(accueil_velo_departemental_iti |> filter(INSEE_DEP == as.character(validated_selection$departement)))

      # Introduction panels
      mod_intro_panel_server("intro_av", "accueil_velo",
                             validated_selection, data_cat_terr,
                             accueil_velo_departemental, "accueil_velo_data")

      # Count of Accueil Vélo services - Declined by category
      output$table_services_by_category <- render_gt({

        table_services <- prepare_dep_epci_com_data(
          accueil_velo_departemental, accueil_velo_national, data_cat_terr,
          "INSEE_DEP", "DEP", "LIBDEP",
          validated_selection$departement,
          current_year, presta_types_table()
        )

        generate_services_count_table(table_services, "departement", color_low = "#F6F9ED", color_high = "#C6CE41", font_size = 12, font_family = "Poppins")

      })

      # Evolution - Count of Accueil Vélo services
      output$evolution_nb_services <- renderPlotly({
        plot_evolution_nb_services(filtered_data(), input$type_for_evol, time_period, territory_name())
      })

      # Value boxes
      value_box_content<- reactive(value_box_content_accueil_velo(filtered_data(), presta_types_table(), "departement", current_year,
                                                                  "count", input$type_for_evol, NULL))

      output$vb_title <- renderText({value_box_content()$title})
      output$vb_chiffre_cle <- renderText({value_box_content()$value})
      output$vb_evol_cle <- renderText({value_box_content()$evol})

      # Accueil Vélo services per itinerary
      output$services_per_itinerary_nat <- renderPlotly({
        # Select national itineraries sections crossing the departement and that have at least 10 services
        data_iti <- data_dep() |>
          filter(SCHEMA == "EV/SNV") |>
          filter(!!sym(paste0("NB_TOTAL_", as.character(current_year))) >= 10)

        plot_services_per_itinerary(data_iti, "Prestations des véloroutes nationales", current_year)
      })

      output$services_per_itinerary_reg <- renderPlotly({
        # Select regional itineraries that have at least 10 services
        data_iti <- data_dep() |>
          filter(SCHEMA == "SRV") |>
          filter(!!sym(paste0("NB_TOTAL_", as.character(current_year))) >= 10)

        plot_services_per_itinerary(data_iti, "Prestations des véloroutes régionales", current_year)
      })

      # Density of Accueil Vélo services around cycle routes - Interactive map
      output$map_services_density <- renderLeaflet({
        density_sf = sf::st_read("inst/json_files/accueil_velo_departemental_grille5km.json", quiet = TRUE) |>
          filter(INSEE_DEP == validated_selection$departement)

        map_services_density(density_sf, admin_departement, "departement",
                             validated_selection$departement, input$selected_type_density,
                             current_year, presta_types_table())
      })

    }
  )
}

## To be copied in the UI
# mod_accueil_velo_departement_ui("accueil_velo_departement_1")

## To be copied in the server
# mod_accueil_velo_departement_server("accueil_velo_departement_1")
