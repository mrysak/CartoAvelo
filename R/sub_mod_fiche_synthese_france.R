#' fiche_synthese_france UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gt
mod_fiche_synthese_france_ui <- function(id, current_year_fr, current_year_vr){
  ns <- NS(id)
  tagList(
    br(),

    # Introduction panel
    mod_intro_panel_fiche_synthese_ui(ns("intro_fs")),

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

        # Légende
        legend_histo_fiche_synthese("France entière", "Grands centres urbains",
                                    "Communes intermédiaires", "Communes rurales"),
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
        legend_histo_fiche_synthese("France entière", "Grands centres urbains",
                                    "Communes intermédiaires", "Communes rurales"),
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
    ),

    # Véloroutes
    card(
      card_title(
        div(
          span(paste0("État d'avancement des véloroutes inscrites aux différents schémas cyclables en ", current_year_vr),
               class = "h4"),
          br(),
          span("Source : Les collectivités contributrices de l'Observatoire National des Véloroutes, Réseau Vélo et Marche | Juillet 2025",
               class = "fst-italic text-muted ms-2 fw-light",
               style = "font-size: 0.80rem;")
        )
      ),
      card_body(
        layout_columns(
          div(
            p("Schéma Européen (EuroVelo)", class = "h6", style = "margin: 0 0 0 15%;"),
            plotlyOutput(ns("plot_completion_ev"), height = "350px")
          ),
          div(
            p("Schéma strictement national", class = "h6", style = "margin: 0 0 0 15%;"),
            plotlyOutput(ns("plot_completion_snv"), height = "350px")
          ),
          div(
            p("Schéma strictement régional", class = "h6", style = "margin: 0 0 0 15%;"),
            plotlyOutput(ns("plot_completion_srv"), height = "350px")
          ),
          div(
            p("Schéma strictement départemental", class = "h6", style = "margin: 0 0 0 10%;"),
            plotlyOutput(ns("plot_completion_sdv"), height = "350px")
          ),
          style = "margin-right: 2%; margin-left: -2%"
        ),
        layout_columns(
          # Value boxes
          div(
            style = "margin-left: 5%; width: 48%;",
            value_box(
              title = div(textOutput(ns("vb_vr_title")), style = "text-align: center;"),
              value = tags$div(textOutput(ns("vb_vr_chiffre_cle")),
                               style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
              div(textOutput(ns("vb_vr_evol")), style = "text-align: center;"),
              class = "mt-auto"
            )
          ),
          # Plot legend
          div(
            style = "text-align:right; margin-right:4%; margin-top: -1%;",
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

    # Fréquentations
    card(
      card_title(span("Fréquentation vélo", class = "h4"),
                 span("Source : Plateforme nationale des fréquentations (PNF), Collectivités contributrices, Observatoire national des véloroutes (ONV)",
                      class = "fst-italic text-muted ms-2 fw-light",
                      style = "font-size: 0.80rem; margin-left: 8px;")),
      card_body(
        # Passages par semaine
        card(
          card_title(uiOutput(ns("title_week"))),
          card_body(
            div(
              style = "display: flex; gap: 20px; align-items: flex-end;",
              div(
                style = "flex: 8.5; margin-left: 2%; margin-right: -1%;",
                plotlyOutput(ns("passages_week"), height = "440px")
              ),
              div(
                style = "flex: 3.5; display: flex; justify-content: center; align-items: flex-end;",
                div(
                  style = "width: 85%; margin-bottom: 5%; margin-left: -4%;",
                  value_box(
                    title = div(paste0("Évolution ", current_year_fr, "/", current_year_fr - 1), style = "text-align: center;"),
                    value = tags$div(textOutput(ns("vb_fr_chiffre_cle")),
                                     style = "font-weight: bold; align-self: center !important; max-width: 300px; white-space: nowrap;
                               overflow: hidden; text-overflow: ellipsis; font-size: clamp(1rem, 2vw, 2.5rem);"),
                    class = "mt-auto"
                  )
                )
              )
            )
          )
        ),
        # Évolution depuis 2019
        card(
          card_title(uiOutput(ns("title_global_evolution"))),
          card_body(
            div(
              style = "width: 70%; margin: 0 auto; align-items: center; margin-top: -2%",
              plotlyOutput(ns("global_evolution"), height = "440px")
            )
          )
        )
      )
    )
  )
}



#' fiche_synthese_france Server Functions
#'
#' @noRd
mod_fiche_synthese_france_server <- function(id, validated_selection, current_year_ac, current_year_st,
                                             current_year_pm, current_year_av, current_year_fr, current_year_vr) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get infos about the selected territory
      territory_name <- reactive(get_territory_infos(validated_selection, data_cat_terr)$name)

      # Introduction panel
      mod_intro_panel_server_fiche_synthese("intro_fs", validated_selection, data_cat_terr)

      # --- AMENAGEMENTS ---

      # Evolution - Taux de cyclabilité
      output$evolution_taux_cyclabilite <- renderPlotly({
        data_comparison_cat <- generate_comparison_data_national(data_cat_terr, amenagement_communal,
                                                                 "taux_cyclabilite", seq(2021, current_year_ac))

        plot_evolution_amenagement(amenagement_national, data_comparison_cat, "taux_cyclabilite",
                                   territory_name(), seq(2021, current_year_ac), "france", "") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Evolution - Linéaire de voirie par habitant
      output$evolution_voirie_hab <- renderPlotly({
        data_comparison_cat <- generate_comparison_data_national(data_cat_terr, amenagement_communal,
                                                                 "voirie_hab", seq(2021, current_year_ac))

        plot_evolution_amenagement(amenagement_national, data_comparison_cat, "voirie_hab",
                                   territory_name(), seq(2021, current_year_ac), "france", "") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_tc_content <- reactive(value_box_content_amenagement(amenagement_national, current_year_ac, "taux_cyclabilite"))

      output$vb_tc_title <- renderText({vb_tc_content()$title})
      output$vb_tc_chiffre_cle <- renderText({vb_tc_content()$chiffre_cle})
      output$vb_tc_evol <- renderText({vb_tc_content()$evol_cle})

      vb_vh_content <- reactive(value_box_content_amenagement(amenagement_national, current_year_ac, "voirie_hab"))

      output$vb_vh_title <- renderText({vb_vh_content()$title})
      output$vb_vh_chiffre_cle <- renderText({vb_vh_content()$chiffre_cle})
      output$vb_vh_evol <- renderText({vb_vh_content()$evol_cle})


      # --- STATIONNEMENTS ---

      # Évolution - Stationnements pour 1 000 habitants
      output$evolution_stat_hab <- renderPlotly({
        data_comparison_cat <- generate_stationnement_comparison_data_national(data_cat_terr, stationnement_communal,
                                                                               "stat_hab", 2022, current_year_st)

        plot_evolution_stationnement(stationnement_national, data_comparison_cat, "stat_hab",
                                     "France entière", 2022, current_year_st, "france", "") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })


      # Évolution - Stationnements par km d'aménagement
      output$evolution_stat_km <- renderPlotly({
        data_comparison_cat <- generate_stationnement_comparison_data_national(data_cat_terr, stationnement_communal,
                                                                               "stat_km", 2022, current_year_st)

        plot_evolution_stationnement(stationnement_national, data_comparison_cat, "stat_km",
                                     "France entière", 2022, current_year_st, "france", "") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_sh_content <- reactive(value_box_content_stationnement(stationnement_national, current_year_st, "stat_hab"))

      output$vb_sh_title <- renderText({vb_sh_content()$title})
      output$vb_sh_chiffre_cle <- renderText({vb_sh_content()$value})
      output$vb_sh_evol <- renderText({vb_sh_content()$evol})

      vb_sk_content <- reactive(value_box_content_stationnement(stationnement_national, current_year_st, "stat_km"))

      output$vb_sk_title <- renderText({vb_sk_content()$title})
      output$vb_sk_chiffre_cle <- renderText({vb_sk_content()$value})
      output$vb_sk_evol <- renderText({vb_sk_content()$evol})


      # --- PART MODALE ---

      # Évolution - Part modale du vélo dans les déplacements domicile travail
      output$evolution_part_modale <- renderPlotly({
        data_comparison_cat <- generate_pm_comparison_data_national(data_cat_terr, part_modale,
                                                                    current_year_pm, 2018, "velo")

        plot_evolution_part_modale(part_modale |> filter(insee_niveau == "NAT"), data_comparison_cat,
                                   "velo", 2018, current_year_pm,
                                   "France entière", "france", "") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_pm_content <- reactive(value_box_content_pm(part_modale, current_year_pm, "france", "", "velo"))

      output$vb_pm_title <- renderText({HTML(vb_pm_content()$title)})
      output$vb_pm_chiffre_cle <- renderText({vb_pm_content()$value})
      output$vb_pm_evol <- renderText({vb_pm_content()$evol})


      # --- ACCUEIL VELO ---

      # Évolution - Nombre de prestations Accueil Vélo
      output$evolution_accueil_velo <- renderPlotly({
        plot_evolution_nb_services(accueil_velo_national, "TOTAL", seq(2022, current_year_av), "France entière") |>
          layout(showlegend = FALSE, title = list(text = ""))
      })

      # Value boxes
      vb_av_content<- reactive(value_box_content_accueil_velo(accueil_velo_national, presta_types_table(), "france", current_year_av,
                                                              "count", "TOTAL", NULL))

      output$vb_av_title <- renderText({vb_av_content()$title})
      output$vb_av_chiffre_cle <- renderText({vb_av_content()$value})
      output$vb_av_evol <- renderText({vb_av_content()$evol})


      # --- VELOROUTES ---

      # Completion status of cycling routes
      output$plot_completion_ev <- renderPlotly({
        plot_avancement_veloroutes(veloroute_national, current_year_vr, "EV",
                                   "france", "", "", "fs")
      })

      output$plot_completion_snv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_national, current_year_vr, "SNV-STRICT",
                                   "france", "", "", "fs")
      })

      output$plot_completion_srv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_national, current_year_vr, "SRV-STRICT",
                                   "france", "", "", "fs")
      })

      output$plot_completion_sdv <- renderPlotly({
        plot_avancement_veloroutes(veloroute_national, current_year_vr, "SDV-STRICT",
                                   "france", "", "", "fs")
      })

      # Value box - Total number of kilometers
      vb_vr_content <- reactive(value_box_content_veloroute(veloroute_national, current_year_vr, "france",
                                                            "", "total", ""))

      output$vb_vr_title <- renderText(vb_vr_content()$title)
      output$vb_vr_chiffre_cle <- renderText(vb_vr_content()$value)
      output$vb_vr_evol <- renderText(vb_vr_content()$evol)


      # --- FRÉQUENTATIONS ---

      # Passages par semaine
      output$title_week <- renderUI({
        nb_counters <- freq_daily |> distinct(site_id) |> nrow()

        card_title(
          HTML(
            paste0(
              "Fréquentation journalière moyenne par semaine en ",
              "<span style='color:#303876; font-weight:bold;'>", current_year_fr - 1, "</span>",
              " et <span style='color:#C6CE41; font-weight:bold;'>", current_year_fr , "</span><br>",
              "<span style='font-size:0.8rem; color:#555;'>France entière - À échantillon comparable, base ",
              nb_counters, " compteurs",
              "</span>"
            )
          )
        )
      })

      output$passages_week <- renderPlotly({
        plot_weekly_passages(freq_daily, current_year_fr, "france", "all", "France")$plot
      })

      output$vb_fr_chiffre_cle <- renderText({
        evol <- table_avg_daily_traffic(freq_global_means, current_year_fr, "france")$evol

        if (!is.numeric(evol)){
          evol
        } else{
          paste0(ifelse(evol >= 0, "+", ""),
                 round(evol, 0), "%")
        }
      })

      # Évolution depuis 2019
      output$title_global_evolution <- renderUI({
        card_title(
          HTML(
            paste0(
              "Evolution de la fréquentation vélo depuis 2019<br>",
              "<span style='font-size:0.8rem; color:#555;'>France entière - À échantillon comparable, base ",
              freq_global_evol |> filter(milieu_region == "France") |> pull(nb_compteurs),
              " compteurs",
              "</span>"
            )
          )
        )
      })

      output$global_evolution <- renderPlotly({
        data <- freq_global_evol |> filter(is.na(cog))
        plot_global_evolution(data, 2019, current_year_fr, "france")
      })
    }
  )
}

## To be copied in the UI
# mod_fiche_synthese_france_ui("fiche_synthese_france_1")

## To be copied in the server
# mod_fiche_synthese_france_server("fiche_synthese_france_1")
