#' fiche_synthese_epci UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fiche_synthese_epci_ui <- function(id, current_year_ac, current_year_st){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_fiche_synthese_ui(ns("intro_fs")),

    # Radar chart
    card(
      card_title(span("Résumé", class = "h4"),
                 tags$span(
                   bslib::popover(
                     icon("info-circle", class = "popover-info"),
                     HTML("Ce graphique en radar compare plusieurs indicateurs clés mesurant le développement du vélo sur le territoire.<br><br>
                     <ul>
                       <li>Chaque <strong>axe</strong> correspond à un indicateur (par exemple : taux de cyclabilité, voirie cyclable par habitant,
                           part modale du vélo, etc.) et va de <strong>0</strong> à <strong>100 %</strong> : 100 % correspond au maximum des
                           territoires comparables.</li>
                           <li>La <span style='color:#C6CE41; font-weight:bold;'>ligne verte</span> montre la <strong>valeur spécifique du territoire sélectionné</strong>.</li>
                       <li>La <span style='color:#E94F35; font-weight:bold;'>ligne rouge</span> représente la <strong>moyenne des territoires comparables</strong>.</li>
                       <li>En survolant un axe, un <strong>encart explicatif</strong> s'affiche.</li>
                      </ul>
                      Ce graphique permet de <strong>repérer facilement les forces et les marges d’amélioration du territoire</strong> en comparaison avec d’autres territoires similaires."),
                     options = list(
                       container = "body",
                       customClass = "popover-wide"
                     )
                   ),
                   style = "margin-left: 7px; position: relative; top: -1px;"
                 )),
      card_body(
        plotlyOutput(ns("radar_chart"), height = "500px")
      )
    ),

    # Aménagements
    card(
      card_title(span("Aménagements cyclables", class = "h4"),
                 span("Source : Contributeurs OpenStreetMap",
                      class = "fst-italic text-muted ms-2 fw-light",
                      style = "font-size: 0.80rem; margin-left: 8px;")),
      card_body(
        layout_columns(
          # Évolution - Taux de cyclabilité
          card(
            card_title("Taux de cyclabilité de la voirie"),
            div(style = "margin-top: -3%;",
                plotlyOutput(ns("evolution_taux_cyclabilite"), height = "440px"))
          ),
          # Évolution - Linéaire de voirie cyclable par habitant
          card(
            card_title("Linéaire de voirie cyclable par habitant"),
            div(style = "margin-top: -3%;",
                plotlyOutput(ns("evolution_voirie_hab"), height = "440px"))
          )
        ),

        # Legend
        uiOutput(ns("legend_evolution_ac")),

        # Values boxes
        layout_columns(
          col_widths = c(6, 6),
          div(
            style = "width: 50%; margin-left: 5%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_tc_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_tc_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_tc_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          ),
          div(
            style = "width: 50%; margin-left: 10%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_vh_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_vh_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_vh_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
    ),

    # Stationnements
    card(
      card_title(span("Stationnements vélo", class = "h4"),
                 span("Source : Contributeurs OpenStreetMap",
                      class = "fst-italic text-muted ms-2 fw-light",
                      style = "font-size: 0.80rem; margin-left: 8px;")),
      card_body(
        layout_columns(
          # Évolution - Stationnements pour 1 000 habitants
          card(
            card_title("Stationnements vélo pour 1 000 habitants"),
            div(style = "margin-top: -3%;",
                plotlyOutput(ns("evolution_stat_hab"), height = "440px"))
          ),
          # Évolution - Stationnements par km d'aménagement cyclable
          card(
            card_title("Stationnement vélo par km d'aménagement cyclable"),
            div(style = "margin-top: -3%;",
                plotlyOutput(ns("evolution_stat_km"), height = "440px"))
          )
        ),

        # Légende
        uiOutput(ns("legend_evolution_st")),
        # Values boxes
        layout_columns(
          col_widths = c(6, 6),
          div(
            style = "width: 50%; margin-left: 5%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_sh_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_sh_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_sh_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          ),
          div(
            style = "width: 50%; margin-left: 10%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_sk_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_sk_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_sk_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
    ),

    layout_columns(
      # Part modale
      card(
        card_title(span("Part modale", class = "h4"),
                   span("Source : INSEE | Recensements de la population",
                        class = "fst-italic text-muted ms-2 fw-light",
                        style = "font-size: 0.80rem; margin-left: 8px;")),
        card_body(
          card_title("Part modale du vélo dans les déplacements domicile-travail"),
          plotlyOutput(ns("evolution_part_modale"), height = "440px"),
          div(
            style = "width: 50%; margin-left: 5%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_pm_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_pm_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_pm_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      ),
      # Accueil Vélo
      card(
        card_title(span("Accueil vélo", class = "h4"),
                   span("Source : FVT, Observatoire national des véloroutes, RVM",
                        class = "fst-italic text-muted ms-2 fw-light",
                        style = "font-size: 0.80rem; margin-left: 8px;")),
        card_body(
          card_title("Nombre de prestations Accueil Vélo"),
          plotlyOutput(ns("evolution_accueil_velo"), height = "440px"),
          div(
            style = "width: 50%; margin-left: 7%; margin-top: 2%;",
            value_box(
              title = div(textOutput(ns("vb_av_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_av_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_av_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          )
        )
      )
    )
  )
}

#' fiche_synthese_epci Server Functions
#'
#' @noRd
mod_fiche_synthese_epci_server <- function(id, validated_selection, current_year_ac, current_year_st, admin_cache) {
  moduleServer(
    id,
    function(input, output, session) {

      # Introduction panel
      mod_intro_panel_server_fiche_synthese("intro_fs", validated_selection, data_cat_terr, load_admin_sf("epci", admin_cache))

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Filter data by selected EPCI
      data_amenagement <- reactive(amenagement_epci |> filter(INSEE_EPCI == validated_selection$epci))
      data_stationnement <- reactive(stationnement_epci |> filter(INSEE_EPCI == validated_selection$epci))
      data_accueil_velo <- reactive(accueil_velo_epci |> filter(INSEE_EPCI == as.numeric(validated_selection$epci)))

      # Radar chart
      output$radar_chart <- renderPlotly({
        data <- generate_data_radar_chart(data_cat_terr, amenagement_epci, stationnement_epci,
                                          part_modale, veloroute_epci, accueil_velo_epci,
                                          current_year_ac, current_year_st, current_year_pm,
                                          current_year_vr, current_year_av, "epci", validated_selection$epci)

        plot_radar_chart(data$data_to_display, data$lib_category)
      })


      # --- AMENAGEMENTS ---

      # Evolution - Taux de cyclabilité
      comparison_data <- reactive(generate_comparison_data_local(data_cat_terr, amenagement_epci, "epci",
                                                                 "taux_cyclabilite", validated_selection$epci, seq(2021, current_year_ac)))
      lib_category <- reactive(comparison_data()$lib_category)

      output$evolution_taux_cyclabilite <- renderPlotly({
        data_comparison_cat <- comparison_data()$data_comparison_cat

        plot_evolution_amenagement(data_amenagement(), data_comparison_cat, "taux_cyclabilite",
                                   territory_name(), seq(2021, current_year_ac), "epci", lib_category()) |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Evolution - Linéaire de voirie par habitant
      output$evolution_voirie_hab <- renderPlotly({
        comparison_data <- generate_comparison_data_local(data_cat_terr, amenagement_epci, "epci",
                                                          "voirie_hab", validated_selection$epci, seq(2021, current_year_ac))

        data_comparison_cat <- comparison_data$data_comparison_cat

        plot_evolution_amenagement(data_amenagement(), data_comparison_cat, "voirie_hab",
                                   territory_name(), seq(2021, current_year_ac), "epci", lib_category()) |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Legend
      territory_name_clean <- reactive({
        name <- territory_name()
        name <- gsub("Communauté d'agglomération", "CA", name, ignore.case = TRUE)
        name <- gsub("Communauté urbaine", "CU", name, ignore.case = TRUE)
        name <- gsub("Communauté de communes", "CC", name, ignore.case = TRUE)
        name
      })

      output$legend_evolution_ac <- renderUI({
        legend_histo_fiche_synthese(territory_name_clean(), paste0("Max. des ", lib_category()),
                                    paste0("Min. des ", lib_category()), paste0("Moy. pondérée des ", lib_category()))
      })

      # Value boxes
      vb_tc_content <- reactive(value_box_content_amenagement(data_amenagement(), current_year_ac, "taux_cyclabilite"))

      output$vb_tc_title <- renderText({vb_tc_content()$title})
      output$vb_tc_chiffre_cle <- renderText({vb_tc_content()$chiffre_cle})
      output$vb_tc_evol <- renderText({vb_tc_content()$evol_cle})

      vb_vh_content <- reactive(value_box_content_amenagement(data_amenagement(), current_year_ac, "voirie_hab"))

      output$vb_vh_title <- renderText({vb_vh_content()$title})
      output$vb_vh_chiffre_cle <- renderText({vb_vh_content()$chiffre_cle})
      output$vb_vh_evol <- renderText({vb_vh_content()$evol_cle})


      # --- STATIONNEMENTS ---

      # Evolution - Stationnements pour 1 000 habitants
      output$evolution_stat_hab <- renderPlotly({
        comparison_data <-generate_stationnement_comparison_data_local(data_cat_terr, stationnement_epci, "stat_hab",
                                                                       "epci", validated_selection$epci, 2022, current_year_st)

        data_comparison_cat <- comparison_data$data_comparison_cat

        plot_evolution_stationnement(data_stationnement(), data_comparison_cat, "stat_hab",
                                     territory_name(), 2022, current_year_st, "epci", lib_category()) |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Evolution - Stationnement par km d'aménagement
      output$evolution_stat_km <- renderPlotly({
        comparison_data <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_epci, "stat_km",
                                                                        "epci", validated_selection$epci, 2022, current_year_st)

        data_comparison_cat <- comparison_data$data_comparison_cat

        plot_evolution_stationnement(data_stationnement(), data_comparison_cat, "stat_km",
                                     territory_name(), 2022, current_year_st, "epci", lib_category()) |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Legend
      output$legend_evolution_st <- renderUI({
        legend_histo_fiche_synthese(territory_name(), paste0("Moyenne pondérée des ", lib_category()))
      })

      # Value boxes
      vb_sh_content <- reactive(value_box_content_stationnement(data_stationnement(), current_year_st, "stat_hab"))

      output$vb_sh_title <- renderText({vb_sh_content()$title})
      output$vb_sh_chiffre_cle <- renderText({vb_sh_content()$value})
      output$vb_sh_evol <- renderText({vb_sh_content()$evol})

      vb_sk_content <- reactive(value_box_content_stationnement(data_stationnement(), current_year_st, "stat_km"))

      output$vb_sk_title <- renderText({vb_sk_content()$title})
      output$vb_sk_chiffre_cle <- renderText({vb_sk_content()$value})
      output$vb_sk_evol <- renderText({vb_sk_content()$evol})


      # --- PART MODALE ---

      # Évolution - Part modale du vélo dans les déplacements domicile travail
      output$evolution_part_modale <- renderPlotly({
        data_evolution <- generate_pm_comparison_data_local(data_cat_terr, part_modale, current_year_pm, 2018,
                                                            "velo", "epci", validated_selection$epci)

        plot_evolution_part_modale(part_modale |> filter(insee_niveau == "EPCI" & insee_code == validated_selection$epci),
                                   data_evolution$data_comparison_cat, "velo", 2018, current_year_pm,
                                   territory_name(), "epci", data_evolution$lib_category)|>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_pm_content <- reactive(value_box_content_pm(part_modale, current_year_pm, "epci", validated_selection$epci, "velo"))

      output$vb_pm_title <- renderText({HTML(vb_pm_content()$title)})
      output$vb_pm_chiffre_cle <- renderText({vb_pm_content()$value})
      output$vb_pm_evol <- renderText({vb_pm_content()$evol})


      # --- ACCUEIL VELO ---

      # Évolution - Nombre de prestations Accueil Vélo
      output$evolution_accueil_velo <- renderPlotly({
        plot_evolution_nb_services(data_accueil_velo(), "TOTAL", seq(2022, current_year_av), territory_name()) |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_av_content<- reactive(value_box_content_accueil_velo(data_accueil_velo(), presta_types_table(), "epci", current_year_av,
                                                              "count", "TOTAL", NULL))

      output$vb_av_title <- renderText({vb_av_content()$title})
      output$vb_av_chiffre_cle <- renderText({vb_av_content()$value})
      output$vb_av_evol <- renderText({vb_av_content()$evol})
    }
  )
}

## To be copied in the UI
# mod_fiche_synthese_epci_ui("fiche_synthese_epci_1")

## To be copied in the server
# mod_fiche_synthese_epci_server("fiche_synthese_epci_1")
