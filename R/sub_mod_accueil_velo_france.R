#' accueil_velo_france UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accueil_velo_france_ui <- function(id, current_year){
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
        div(
          leafletOutput(ns("map_services_count"), height = "700px", width = "97%"),
          # Update view when a DROM is selected
          tags$head(
            tags$script(HTML(paste0("
            $(document).on('change', '#", ns("territoire_select"), "', function() {
              Shiny.setInputValue('", ns("territoire_select"), "', this.value, {priority: 'event'});
            });

            Shiny.addCustomMessageHandler('maj_select_territoire', function(val) {
              var sel = document.getElementById('", ns("territoire_select"), "');
              if(sel) {
                sel.value = val;
                $(sel).trigger('change');
              }
            });
          ")))
          )
        ),
        div(
          class = "d-grid gap-3",
          selectInput(ns("indicator_choice"), "Indicateur à visualiser",
                      choices = setNames(c("count", "ratio", "density"),
                                         c("Nombre total de prestations Accueil Vélo", "Nombre de prestations Accueil Vélo par kilomètre de véloroute traversant le territoire", "Densité des prestations Accueil Vélo à proximité des véloroutes nationales")),
                      width = "86%"),
          conditionalPanel(
            condition = paste0("['count','ratio'].indexOf(input['", ns("indicator_choice"), "']) !== -1"),
            selectInput(ns("display_typology"), "Affichage par",
                        choices = setNames(c("region", "departement"), c("Régions", "Départements")),
                        width = "86%")
          ),
          conditionalPanel(
            condition = paste0("['count','ratio'].indexOf(input['", ns("indicator_choice"), "']) !== -1"),
            selectInput(ns("selected_type"), "Type de prestation",
                        choices = setNames(presta_types_table()$short, presta_types_table()$long),
                        width = "86%")
          ),
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
          conditionalPanel(
            condition = sprintf("input['%s'] == 'density'", ns("indicator_choice")),
            selectInput(ns("selected_type_density"), "Type de prestation",
                        choices = setNames(presta_types_table()$short, presta_types_table()$long),
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
      card_title(paste0("Prestations Accueil Vélo des véloroutes nationales en ", current_year), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 3),
        plotlyOutput(ns("services_per_itinerary")),
        selectInput(ns("select_iti"), "Sélection de véloroutes",
                    choices = accueil_velo_national_iti |> filter(SCHEMA == "EV/SNV") |> distinct(ITI_NUM) |>
                      mutate(prefix = str_extract(ITI_NUM, "^[A-Za-z]+"), num = parse_number(ITI_NUM), prefix = factor(prefix, levels = c("EV", "V"))) |>
                      arrange(prefix, num) |> pull(ITI_NUM),
                    selected = c("EV1", "EV3", "EV4", "EV5", "EV6"), multiple = TRUE, selectize = TRUE)
      ),
      tags$p(
        "Affichage limité à 8 véloroutes en simultané : les véloroutes comportant le plus de prestations Accueil Vélo sont affichées.",
        style = "font-style: italic; font-size: 0.8em; margin-top: -15px; margin-left: 10px",
      )
      )
    )
  )
}



#' accueil_velo_france Server Functions
#'
#' @noRd
mod_accueil_velo_france_server <- function(id, validated_selection, current_year) {
  moduleServer(
    id,
    function(input, output, session) {

      # Time period with Accueil Vélo data
      time_period <- as.character(seq(2022, current_year))

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_av", "accueil_velo",
                             validated_selection, data_cat_terr,
                             accueil_velo_national, "accueil_velo_data")

      # Count of Accueil Vélo services and ratio by km of cycle route / Density - Interactive map
      output$map_title <- renderText({
        req(input$indicator_choice)
        if (input$indicator_choice == "count"){
          paste0("Nombre de prestations Accueil Vélo en ", current_year)
        } else if (input$indicator_choice == "ratio"){
          paste0("Nombre de prestations Accueil Vélo par kilomètre de véloroute traversant le territoire en ", current_year)
        } else if (input$indicator_choice == "density"){
          paste0("Densité des prestations Accueil Vélo à proximité des véloroutes nationales (10 km) en ", current_year)
        }
      })

      output$map_services_count <- renderLeaflet({
        if (input$indicator_choice %in% c("count", "ratio")){

          req(input$display_typology, input$indicator_choice, input$selected_type, input$info_to_display)

          if (input$display_typology == "region"){
            admin_sf <- admin_region
            accueil_velo_data <- accueil_velo_regional
            insee_col <- "INSEE_REG"
          } else if (input$display_typology == "departement"){
            admin_sf <- admin_departement
            accueil_velo_data <- accueil_velo_departemental
            insee_col <- "INSEE_DEP"
          }

          map_services_count(data_cat_terr, accueil_velo_data, admin_sf, insee_col,
                             as.numeric(current_year), presta_types_table(), "france", "", input$selected_type,
                             input$indicator_choice, input$info_to_display, input$cycle_route_type,
                             input$display_typology, session$ns)
        } else if (input$indicator_choice == "density"){

          density_sf = sf::st_read("inst/json_files/accueil_velo_national_grille10km.json", quiet = TRUE)

          map_services_density(density_sf, NA, "france",
                               NA, input$selected_type_density,
                               current_year, presta_types_table())
        }
      })

      # Value boxes
      value_box_content<- reactive(value_box_content_accueil_velo(accueil_velo_national, presta_types_table(), "france", current_year,
                                                                  input$indicator_choice, input$selected_type, input$cycle_route_type))

      output$vb_title <- renderText({value_box_content()$title})
      output$vb_chiffre_cle <- renderText({value_box_content()$value})
      output$vb_evol_cle <- renderText({value_box_content()$evol})

      # Zoom on DROM with a select input
      observeEvent(input$territoire_select, {
        req(input$territoire_select)
        choix <- zones()[[input$territoire_select]]
        req(choix)

        leafletProxy("map_services_count") %>%
          setView(lng = choix$lng, lat = choix$lat, zoom = choix$zoom)
      })

      # Count of Accueil Vélo services - Declined by category
      output$table_services_by_category <- render_gt({

        table_services <- prepare_national_data(
          accueil_velo_national,
          current_year,
          presta_types_table()
        )$table_national

        generate_services_count_table(table_services, "france", color_low = "#F6F9ED", color_high = "#C6CE41", font_size = 12, font_family = "Poppins")

      })

      # Evolution - Count of Accueil Vélo services
      output$evolution_nb_services <- renderPlotly({
        plot_evolution_nb_services(accueil_velo_national, input$type_for_evol, time_period, territory_name())
      })

      # Accueil Vélo services per itinerary
      output$services_per_itinerary <- renderPlotly({
        # Only keep itineraries selected by the user
        data_iti <- accueil_velo_national_iti |>
          filter(ITI_NUM %in% input$select_iti)

        plot_services_per_itinerary(data_iti, "", current_year)
      })

      # Download button
      output$download <- downloadHandler(
        filename = function() {
          paste0("accueil_velo_data_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(accueil_velo_national, file, row.names = FALSE, fileEncoding = "UTF-8")
        }
      )

    }
  )
}

## To be copied in the UI
# mod_accueil_velo_france_ui("accueil_velo_france_1")

## To be copied in the server
# mod_accueil_velo_france_server("accueil_velo_france_1")
