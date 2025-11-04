#' veloroute_region UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_veloroute_region_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_vr"), "veloroute"),

    # Completion status of cycling routes
    card(
      card_title(paste0("État d'avancement des véloroutes inscrites aux différents schémas cyclables en ", current_year), class = "h4"),
      card_body(
        layout_columns(
          div(
            p("Schéma Européen (EuroVelo)", class = "h5", style = "margin: 0 0 0 8%;"),
            plotlyOutput(ns("plot_completion_ev"), height = "350px")
          ),
          div(
            p("Schéma strictement national", class = "h5", style = "margin: 0 0 0 8%;"),
            plotlyOutput(ns("plot_completion_snv"), height = "350px")
          ),
          style = "margin-left: -2.5%; margin-right: 1%;"
        ),
        layout_columns(
          div(
            p("Schéma strictement régional", class = "h5", style = "margin: 0 0 0 8%;"),
            plotlyOutput(ns("plot_completion_srv"), height = "350px")
          ),
          div(
            p("Schéma strictement départemental", class = "h5", style = "margin: 0 0 0 8%;"),
            plotlyOutput(ns("plot_completion_sdv"), height = "350px")
          ),
          style = "margin-left: -2.5%; margin-right: 1%;"
        ),
        layout_columns(
          # Value boxes
          div(
            style = "margin-left: 8%; width: 88%;",
            value_box(
              title = div(textOutput(ns("vb_total_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_total_value")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_total_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          ),
          div(
            style = "margin-left: 8%; width: 88%;",
            value_box(
              title = div(textOutput(ns("vb_status_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_status_value")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_status_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          ),
          # Plot legend
          div(
            style = "text-align:right; margin-right:2%; margin-top: -1%;",
            HTML("
                  <div style='display:inline-block; text-align:left;'>
                    <b style='display:block; margin-bottom:5px;'>Statut</b>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#294754; margin-right:5px;'></div>
                      Site propre
                    </div>
                    <div style='display:flex; align-items:center; margin-bottom:3px;'>
                      <div style='width:20px; height:10px; background:#B1D6E4; margin-right:5px;'></div>
                      Site partagé
                    </div>
                    <div style='display:flex; align-items:center;'>
                      <div style='width:20px; height:10px; background:#D3D3D3; margin-right:5px;'></div>
                      Non réalisé
                    </div>
                  </div>
                ")
          )
        )
      )
    ),

    # Progress in the development of the cycle route network - Interactive map
    card(
      card_title(textOutput(ns("map_title")), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        div(
          leafletOutput(ns("map_veloroute_indicators"), height = "700px", width = "97%")
        ),
        div(
          class = "d-grid gap-3",
          selectInput(ns("indicator_choice"), "Indicateur à visualiser",
                      choices = setNames(
                        c("taux_realisation", "km_restant", "avancement_maillage"),
                        c("Taux de réalisation des différents schémas cyclables",
                          "Kilomètres restant à réaliser",
                          "Cartographie de l'avancement du maillage cyclable régional")
                      ),
                      width = "88%"
          ),
          conditionalPanel(
            condition = sprintf("['taux_realisation', 'km_restant'].includes(input['%s'])", ns("indicator_choice")),
            selectInput(ns("schema_type"), "Type de véloroutes",
                        choices = setNames(c("SNV-EV", "EV", "SNV-STRICT", "SRV-STRICT", "SDV-STRICT"),
                                           c("Véloroutes nationales (EuroVelo inclus)", "Véloroutes strictement EuroVelo",
                                             "Véloroutes strictement nationales", "Véloroutes strictement régionales",
                                             "Véloroutes strictement départementales")),
                        width = "88%"
            )
          ),
          conditionalPanel(
            condition = sprintf("['taux_realisation'].includes(input['%s'])", ns("indicator_choice")),
            selectInput(ns("info_to_display"), "Donnée à afficher",
                        choices = setNames(c("value", "evol"),
                                           c(paste0("Valeur ", current_year),
                                             paste0("Evolution par rapport à ", as.numeric(current_year) - 1))),
                        width = "88%"
            )
          ),

          div(
            style = "width: 88%;",
            value_box(
              title = div(uiOutput(ns("vb_map_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_map_value")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_map_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
      )
    ),

    # New achievements
    card(
      card_title(paste0("Nouvelles réalisations de l'année ", current_year), class = "h4"),
      card_body(
        layout_columns(
          col_widths = c(8, 4),
          div(
            div(
              style = "display: flex; align-items: flex-start; justify-content: flex-start; gap: 25px;",
              plotlyOutput(ns("new_achievements_schema"), height = "260px", width = "70%"),
              tags$div(
                class = "custom-legend",
                style = "font-family: Poppins; font-size: 15px; line-height: 1.4;",
                HTML("<div style='font-weight:600; margin-bottom:6px;'>Véloroutes</div>
                    <div><span style='color:#303876; font-weight:bold;'>■</span> EuroVelo</div>
                    <div><span style='color:#E94F35; font-weight:bold;'>■</span> Strictement nationales</div>
                    <div><span style='color:#C6CE41; font-weight:bold;'>■</span> Strictement régionales</div>
                    <div><span style='color:#ECB2D2; font-weight:bold;'>■</span> Strictement départementales</div>"
                )
              )
            ),
            div(
              style = "display: flex; align-items: flex-start; justify-content: flex-start; gap: 25px;",
              plotlyOutput(ns("new_achievements_status"), height = "270px", width = "70%"),
              tags$div(
                class = "custom-legend",
                style = "font-family: Poppins; font-size: 15px; line-height: 1.4;",
                HTML("<div style='font-weight:600; margin-bottom:6px;'>Statut</div>
                    <div><span style='color:#294754; font-weight:bold;'>■</span> Site propre</div>
                    <div><span style='color:#B1D6E4; font-weight:bold;'>■</span> Site partagé</div>"
                )
              )
            )
          ),

          div(
            style = "display: flex; flex-direction: column; justify-content: flex-end; height: 100%; width: 88%;",
            value_box(
              title = div(textOutput(ns("vb_new_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_new_value")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_new_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
    ),

    # Historique
    card(
      card_title(paste0("Historique des réalisations (2020-", current_year, ")"), class = "h4"),
      card_body(
        layout_columns(
          col_widths = c(7, 5),
          leafletOutput(ns("map_history"), width = "97%"),
          plotlyOutput(ns("history"), height = "550px")
        )
      )
    ),

    scroll_arrow_ui()
  )
}

#' veloroute_region Server Functions
#'
#' @noRd
mod_veloroute_region_server <- function(id, validated_selection, current_year, veloroute_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_vr", "veloroute",
                             validated_selection, data_cat_terr,
                             veloroute_regional, "veloroute_data")

      # Completion status of cycling routes
      output$plot_completion_ev <- renderPlotly({
        plot_avancement_veloroutes(veloroute_regional, current_year, "EV",
                                   "region", validated_selection$region, territory_name())
      })

      output$plot_completion_snv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_regional, current_year, "SNV-STRICT",
                                   "region", validated_selection$region, territory_name())
      })

      output$plot_completion_srv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_regional, current_year, "SRV-STRICT",
                                   "region", validated_selection$region, territory_name())
      })

      output$plot_completion_sdv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_regional, current_year, "SDV-STRICT",
                                   "region", validated_selection$region, territory_name())
      })

      # Value box - Total number of kilometers
      vb_content_total <- reactive(value_box_content_veloroute(veloroute_regional, current_year, "region",
                                                               validated_selection$region, "total", ""))

      output$vb_total_title <- renderText(vb_content_total()$title)
      output$vb_total_value <- renderText(vb_content_total()$value)
      output$vb_total_evol <- renderText(vb_content_total()$evol)

      # Value box - Percentage of site propre
      vb_content_status <- reactive(value_box_content_veloroute(veloroute_regional, current_year, "region",
                                                                validated_selection$region, "status", ""))

      output$vb_status_title <- renderText(vb_content_status()$title)
      output$vb_status_value <- renderText(vb_content_status()$value)
      output$vb_status_evol <- renderText(vb_content_status()$evol)

      # Progress in the development of the cycle route network - Interactive map
      output$map_title <- renderText({
        req(input$indicator_choice, input$schema_type)

        suffix <- switch(input$schema_type,
                         "SNV-EV"     = "véloroutes nationales (EuroVelo inclus)",
                         "EV"         = "véloroutes EuroVelo",
                         "SNV-STRICT" = "véloroutes strictement nationales",
                         "SRV-STRICT" = "véloroutes strictement régionales",
                         "SDV-STRICT" = "véloroutes strictement départementales")


        label <- switch(input$indicator_choice,
                        "taux_realisation"    = paste0("Taux de réalisation des ", suffix),
                        "km_restant"          = paste0("Kilomètres restant à réaliser sur les ", suffix),
                        "avancement_maillage" = "Avancement du maillage cyclable régional"
        )

        paste0(label, " en ", current_year)
      })

      output$map_veloroute_indicators <- renderLeaflet({

        if (input$indicator_choice %in% c("taux_realisation", "km_restant")){

          info_to_display <- ifelse(input$indicator_choice == "taux_realisation", input$info_to_display, 'value')

          map_veloroute_indicators(data_cat_terr, veloroute_departemental, admin_departement, "region",
                                   "departement", validated_selection$region, input$indicator_choice,
                                   info_to_display, input$schema_type, current_year, session$ns)

        } else if (input$indicator_choice == "avancement_maillage"){

          veloroute_sf <- load_file_sf(veloroute_cache, "veloroutes_all.json")
          # veloroute_sf <- veloroute_sf |>     # TO DELETE
          #   mutate(id = "V86",
          #          nom = "La vallée du Lot à vélo")

          map_network_progress_region_dep(veloroute_sf, admin_region, "region", validated_selection$region)
        }
      })

      # Value box - Interactive map
      vb_content_map <- reactive({
        params <- switch(input$indicator_choice,
                         "taux_realisation" = list(
                           vb_type = "taux_realisation",
                           schema_type = input$schema_type
                         ),
                         "km_restant" = list(
                           vb_type = "km_restant",
                           schema_type = input$schema_type
                         ),
                         "avancement_maillage" = list(
                           vb_type = "taux_realisation",
                           schema_type = "SNV-EV"
                         )
        )
        value_box_content_veloroute(veloroute_regional, current_year, "region",
                                    validated_selection$region, params$vb_type, params$schema_type)
      })

      output$vb_map_title <- renderUI(HTML(vb_content_map()$title))
      output$vb_map_value <- renderText(vb_content_map()$value)
      output$vb_map_evol <- renderText(vb_content_map()$evol)

      # New achievements
      plot_new_achievements <- reactive(plot_new_creations(veloroute_regional, current_year, "region",
                                                           validated_selection$region, territory_name()))

      output$new_achievements_schema <- renderPlotly({
        plot_new_achievements()$plot_schema
      })

      output$new_achievements_status <- renderPlotly({
        plot_new_achievements()$plot_statut
      })

      # Value box - New achievements
      vb_content_new <- reactive(value_box_content_veloroute(veloroute_regional, current_year, "region",
                                                             validated_selection$region, "new_achievements", ""))

      output$vb_new_title <- renderText(vb_content_new()$title)
      output$vb_new_value <- renderText(vb_content_new()$value)
      output$vb_new_evol <- renderText(vb_content_new()$evol)

      # Historique
      output$map_history <- renderLeaflet({
        veloroute_sf <- load_file_sf(veloroute_cache, "veloroutes_all.json")
        # veloroute_sf <- veloroute_sf |>
        #   mutate(id = "V86",
        #          nom = "La vallée du Lot à vélo")     # TO DELETE

        map_new_achievements(veloroute_sf, admin_region, current_year,
                             "region", validated_selection$region)
      })

      output$history <- renderPlotly({
        plot_veloroute_history(veloroute_regional, current_year, 2020,
                               "region", validated_selection$region)
      })
    }
  )
}

## To be copied in the UI
# mod_veloroute_region_ui("veloroute_region_1")

## To be copied in the server
# mod_veloroute_region_server("veloroute_region_1")
