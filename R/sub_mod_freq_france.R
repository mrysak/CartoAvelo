#' frequentation_france UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_frequentation_france_ui <- function(id, current_year){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_ui(ns("intro_fr"), "freq"),

    # Counters - Interactive map
    card(
      card_title(textOutput(ns("map_title")), class = "h4"),
      card_body(layout_columns(
        col_widths = c(8, 4),
        div(
          leafletOutput(ns("map_freq_counters"), height = "700px", width = "97%"),
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
          selectInput(ns("map_choice"), "Carte à afficher",
                      choices = setNames(
                        c("milieu_counters", "freq_counters"),
                        c("Compteurs vélo partagés",
                          "Fréquentation vélo moyenne journalière par compteur")
                      ),
                      width = "88%"
          )
        )
      ),
      # Shared and analyzed counters
      layout_columns(
        col_widths = c(7, 1, 4),
        div(
          style = "height: 250px; display: flex; flex-direction: column; justify-content: flex-start;",
          plotlyOutput(ns("analyzed_compteurs"), height = "250px")
        ),
        div(),
        div(
          style = "display: flex; height: 215px; align-items: flex-end; justify-content: center;",
          div(
            style = "width: 70%; margin-left: -15%;",
            value_box(
              title = div("Compteurs analysés", style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_compteurs")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              class = "mt-auto"
            )
          )
        )
      )
     )
    ),

    # Average daily traffic
    card(
      card_title(paste0("Fréquentation vélo moyenne journalière en ", current_year), class = "h4"),
      card_body(
        # Global average daily traffic
        card(
          card_title(
            div(
              "Fréquentation moyenne journalière globale",
              span("(passages/jour)", style = "font-size: 0.8rem; color: #555; margin-left: 0px;")
            )
          ),
          card_body(
            layout_columns(
              col_widths = c(8, 4),
              div(
                style = "height: 540px; display: flex; flex-direction: column; justify-content: flex-start;",
                gt_output(ns("avg_daily_freq"))
              ),
              div(
                style = "display: flex; height: 538px; align-items: flex-end; justify-content: center;",
                div(
                  style = "width: 70%;  margin-left: -10%;",
                  value_box(
                    title = div(paste0("Évolution ", current_year, "/", current_year - 1), style = "text-align: center;"),
                    value = tags$div(
                      textOutput(ns("vb_evol")),
                      style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"
                    ),
                    class = "mt-auto"
                  )
                )
              )
            )
          )
        ),
        # Average daily traffic by week
        card(
          card_title(uiOutput(ns("title_week"))),
          card_body(
            navset_card_tab(
              id = ns("comparison_week"),
              nav_panel("France entière",
                        div(
                          style = "margin-left: 7%; margin-right: 6%;",
                          plotlyOutput(ns("passages_week_france"), height = "450px")
                        )),
              nav_panel("Grands centres urbains",
                        div(
                          style = "margin-left: 7%; margin-right: 6%;",
                          plotlyOutput(ns("passages_week_urbain"), height = "450px")
                        )),
              nav_panel("Communes intermédiaires",
                        div(
                          style = "margin-left: 7%; margin-right: 6%;",
                          plotlyOutput(ns("passages_week_periurbain"), height = "450px")
                        )),
              nav_panel("Communes rurales",
                        div(
                          style = "margin-left: 7%; margin-right: 6%;",
                          plotlyOutput(ns("passages_week_rural"), height = "450px")
                        ))
            )
          )
        )
      )
    ),

    # Evolution of global bike traffic since 2019
    card(
      card_title("Evolution de la fréquentation vélo depuis 2019", class = "h4"),
      card_body(
        div(
          style = "margin-left: 8%; margin-right: 10%; margin-top: -20px",
          plotlyOutput(ns("global_evolution"), height = "470px")
        )
      )
    ),

    # Temporal distribution of bike passages
    card(
      card_title(paste0("Répartition temporelle de la fréquentation vélo en ", current_year), class = "h4"),
      card_body(
        card(
          navset_card_tab(
            id = ns("comparison_temporal"),
            nav_panel("France entière", value = "all"),
            nav_panel("Grands centres urbains"),
            nav_panel("Communes intermédiaires"),
            nav_panel("Communes rurales"))
          ),
          # Monthly distribution
          card(
            card_title(uiOutput(ns("title_monthly_distribution"))),
            card_body(
              div(
                style = "margin-left: 8%; margin-right: 10%; margin-top: -10px;",
                plotlyOutput(ns("monthly_distribution"))
                )
              )
            ),
        # Monthly evolution
        card(
          card_title(uiOutput(ns("title_monthly_evolution"))),
          card_body(
            div(
              style = "margin-left: 8%; margin-right: 10%; margin-top: -10px;",
              plotlyOutput(ns("monthly_evolution"))
            )
          )
        ),
        layout_columns(
          # Weekday distribution
          card(
            card_title(uiOutput(ns("title_weekday_distribution"))),
            card_body(
              div(
                style = "margin-top: -10px;",
                plotlyOutput(ns("weekday_distribution"))
              )
            )
          ),
          # Season distribution
          card(
            card_title(uiOutput(ns("title_season_distribution"))),
            card_body(
              div(
                style = "margin-top: -20px;",
                plotlyOutput(ns("season_distribution"), height = "95%")
              )
            )
          )
        )
      )
    ),

    # Bike traffic by commune size
    card(
      card_title(paste0("Fréquentation vélo en fonction de la taille de la commune en ", current_year), class = "h4"),
      card_body(
        div(
          gt_output(ns("freq_by_commune_size"))
        )
      )
    ),


    scroll_arrow_ui()
  )
}

#' frequentation_france Server Functions
#'
#' @noRd
mod_frequentation_france_server <- function(id, validated_selection, current_year, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panels
      mod_intro_panel_server("intro_fr", "freq",
                             validated_selection, data_cat_terr,
                             DATA, "frequentation_data")

      # Bike counters - Interactive maps
      output$map_title <- renderText({
        req(input$map_choice)

        label <- switch(input$map_choice,
                        "milieu_counters" = "Les compteurs vélo partagés",
                        "freq_counters"   = "Fréquentation vélo moyenne journalière par compteur"
        )

        paste0(label, " en ", current_year)
      })

      output$map_freq_counters <- renderLeaflet({
        req(input$map_choice)

        if(input$map_choice == "milieu_counters"){
          map_info_compteurs(admin_compteurs, admin_region, "france", "", current_year, session$ns)
        } else if (input$map_choice == "freq_counters"){
          map_avg_traffic(freq_daily, admin_compteurs, admin_region, "france", "", "", current_year, session$ns)
        }
      })

      # Zoom on DROM with a select input
      observeEvent(input$territoire_select, {
        req(input$territoire_select)
        choix <- zones()[[input$territoire_select]]
        req(choix)

        leafletProxy("map_freq_counters") %>%
          setView(lng = choix$lng, lat = choix$lat, zoom = choix$zoom)
      })

      # Shared and analyzed counters
      analyzed_counters <- reactive(plot_analyzed_counters(freq_compteurs, current_year, "france", NULL))

      output$analyzed_compteurs <- renderPlotly({
        analyzed_counters()$plot
      })

      output$vb_compteurs <- renderText(
        paste0(round(analyzed_counters()$pct_analyzed, 0), "%")
      )

      # Global average daily traffic
      daily_traffic <- reactive(table_avg_daily_traffic(freq_global_means, current_year,"france"))

      output$avg_daily_freq <- render_gt({
        daily_traffic()$table
      })

      output$vb_evol <- renderText({
        evol <- daily_traffic()$evol
        paste0(ifelse(evol >= 0, "+", ""),
               round(evol, 0), "%")
      })

      # Average daily traffic by week

      output$title_week <- renderUI({
        # Filter df depending on the selected tab
        if(input$comparison_week %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
          df_daily <- freq_daily |> filter(milieu == input$comparison_week)
        } else { df_daily <- freq_daily }

        nb_counters <- df_daily |> distinct(site_id) |> nrow()

        card_title(
          HTML(
            paste0(
              "Fréquentation journalière moyenne par semaine en ",
              "<span style='color:#303876; font-weight:bold;'>", current_year - 1, "</span>",
              " et <span style='color:#C6CE41; font-weight:bold;'>", current_year , "</span><br>",
              "<span style='font-size:0.8rem; color:#555;'>",
              input$comparison_week,
              " - À échantillon comparable, base ", nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$passages_week_france <- renderPlotly({
        plot_weekly_passages(freq_daily, current_year, "france", "all", "France")$plot
      })

      output$passages_week_urbain <- renderPlotly({
        plot_weekly_passages(freq_daily, current_year, "france", "Grands centres urbains", "France")$plot
      })

      output$passages_week_periurbain <- renderPlotly({
        plot_weekly_passages(freq_daily, current_year, "france", "Communes intermédiaires", "France")$plot
      })

      output$passages_week_rural <- renderPlotly({
        plot_weekly_passages(freq_daily, current_year, "france", "Communes rurales", "France")$plot
      })

      # Evolution of global bike traffic since 2019
      output$global_evolution <- renderPlotly({
        data <- freq_global_evol |> filter(is.na(cog))
        plot_global_evolution(data, 2019, current_year, "france")
      })

      # Temporal distribution of bike passages

      # Monthly distribution
      monthly_passages <- reactive(plot_monthly_distribution(freq_daily, current_year, "france", input$comparison_temporal, "France"))

      output$title_monthly_distribution <- renderUI({
        card_title(
          HTML(
            paste0(
              "Répartition par mois des passages de ", current_year,
              "<span style='font-size: 0.8rem;'> (et ", current_year - 1, ")</span><br>",
              "<span style='font-size:0.8rem; color:#555;'>",
              ifelse(input$comparison_temporal == "all", "France entière", input$comparison_temporal),
              " - À échantillon comparable, base ", monthly_passages()$nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$monthly_distribution <- renderPlotly(monthly_passages()$plot)

      # Monthly evolution
      monthly_evolution <- reactive(plot_monthly_evolution(freq_daily, current_year, "france", input$comparison_temporal, "France"))

      output$title_monthly_evolution <- renderUI({
        card_title(
          HTML(
            paste0(
              "Évolution par mois des passages entre ", current_year - 1,
              " et ", current_year, "<br>",
              "<span style='font-size:0.8rem; color:#555;'>",
              ifelse(input$comparison_temporal == "all", "France entière", input$comparison_temporal),
              " - À échantillon comparable, base ", monthly_evolution()$nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$monthly_evolution <- renderPlotly(monthly_evolution()$plot)

      # Weekday distribution
      weekday_passages <- reactive(plot_weekday_distribution(freq_daily, current_year, "france", input$comparison_temporal, "France"))

      output$title_weekday_distribution <- renderUI({
        card_title(
          HTML(
            paste0(
              "Répartition par jour des passages de ", current_year, "<br>",
              "<span style='font-size:0.8rem; color:#555;'>",
              ifelse(input$comparison_temporal == "all", "France entière", input$comparison_temporal),
              " - À échantillon comparable, base ", weekday_passages()$nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$weekday_distribution <- renderPlotly(weekday_passages()$plot)

      # Season distribution
      label_coords <- reactive({
        if (input$comparison_temporal == "Communes intermédiaires") {
          data.frame(
            season = c("Printemps", "Été", "Automne", "Hiver"),
            x = c(0.18, 0.84, 0.5, 0.18), y = c(0.85, 0.75, 0, 0.15)
          )
        } else if (input$comparison_temporal == "Communes rurales") {
          data.frame(
            season = c("Printemps", "Été", "Automne", "Hiver"),
            x = c(0.18, 0.84, 0.25, 0.15), y = c(0.85, 0.75, 0, 0.25)
          )
        } else { NULL }
      })

      season_distribution <- reactive({plot_season_distribution(freq_daily, current_year, "france",
                                                                input$comparison_temporal, "France", label_coords())})

      output$title_season_distribution <- renderUI({
        card_title(
          HTML(
            paste0(
              "Répartition par saison des passages de ", current_year, "<br>",
              "<span style='font-size:0.8rem; color:#555;'>",
              ifelse(input$comparison_temporal == "all", "France entière", input$comparison_temporal),
              " - À échantillon comparable, base ", season_distribution()$nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$season_distribution <- renderPlotly(season_distribution()$plot)

      # Bike traffic by commune size
      output$freq_by_commune_size <- render_gt({
        data <- freq_commune_size |> filter(is.na(cog))
        table_traffic_by_commune_size(data, current_year)
      })
    }
  )
}

## To be copied in the UI
# mod_frequentation_france_ui("frequentation_france_1")

## To be copied in the server
# mod_frequentation_france_server("frequentation_france_1")
