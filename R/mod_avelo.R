#' avelo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_avelo_ui <- function(id){
  ns <- NS(id)
  tagList(

    # Introduction card - V1
    card(
      class = "avelo_intro",
      card_body(
        div(
          style = "display: flex; gap: 20px; align-items: stretch;",
          div(
            style = "flex: 7;",
            p(
              HTML(
                "Depuis 2018, les programmes <strong>CEE AVELO</strong>, pilotés par l'ADEME, accompagnent les territoires peu et moyennement denses dans <strong>la définition et le déploiement de leur politique cyclable</strong>, afin de faire du vélo un mode de déplacement du quotidien accessible au plus grand nombre <strong>sur tout le territoire national.</strong>"
              )
            ),
            p(
              HTML(
                "Avec AVELO 3, la dernière édition en cours, le seuil des <strong>1 000 collectivités accompagnées</strong> financièrement et techniquement a été dépassé, représentant une communauté riche de territoires engagés dans la promotion des mobilités actives."
              )
            ),
            p(
              HTML(
                "Créé en collaboration avec l’ADEME, le tableau de bord ci-dessous met en évidence les lauréats des programmes AVELO, les <strong>actions financées</strong> et les <strong>impacts générés sur la cyclabilité</strong>, à travers une cartographie et un outil de comparaison d'indicateurs clés entre territoires lauréats et non-lauréats."
              )
            )
          ),
          div(
            style = "flex: 3; display: flex; flex-direction: column; align-items: center;",
            card_image(
              src = file.path("www", "logos_ademe_avelo.png"),
              height = "60px",
              style = "margin-left: -25px; margin-top: 5px"
            ),
            div(
              class = "avelo_intro_bttn",
              style = "display: flex; gap: 20px; margin-left: 35px; margin-top: auto; margin-bottom: 7px",
              downloadButton(ns("download"), "Téléchargement"),
              contactButton(width = "180")
            )
          )
        )
        )
      ),

    # AVELO collectivities - Interactive map
    card(
      card_title(span("Cartographie des lauréats AVELO", class = "h4"),
                 tags$span(
                   bslib::popover(
                     icon("info-circle", class = "popover-info"),
                     HTML("La carte ci-dessous recense les différentes collectivités lauréates des programmes AVELO 1, 2 ou 3, ainsi que les actions financées dans leurs territoires.<br><br>
                                   À droite de la carte, vous trouverez plusieurs <strong>filtres</strong> pour affiner l’affichage des territoires selon différents critères : la région à laquelle la collectivité appartient, sa typologie, l'édition du programme dont elle est lauréate, les actions financées...<br><br>
                                   Chaque filtre permet de sélectionner un ou plusieurs éléments en déroulant le menu ou en tapant directement. Pour annuler une sélection, appuyez sur la touche <strong>Retour arrière</strong>.<br><br>
                                   Le bouton <strong>Appliquer les filtres</strong> génère la carte en fonction de vos choix, tandis que <strong>Réinitialiser les filtres</strong> permet de remettre les filtres à zéro pour tester de nouvelles combinaisons.<br><br>
                                   En bas à gauche de la carte, un menu déroulant vous permet de changer la vue, par exemple pour centrer la carte sur un territoire d’Outre-Mer."),
                     options = list(
                       container = "body",
                       customClass = "popover-wide"
                     )
                   ),
                   style = "margin-left: 7px; position: relative; top: -1px;"
                 ),
                 span("Source : ADEME | Août 2025",
                      class = "fst-italic text-muted ms-2 fw-light",
                      style = "font-size: 0.80rem; margin-left: 8px;")),
      card_body(div(
        style = "display: flex !important; flex-direction: row; gap : 2rem",
        # Map
        div(
          style = "flex: 8.5 !important; margin-left: 15px;",
          class = "avelo_map",
          leafletOutput(ns("carto_avelo"), height = "760px", width = "99%"),
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
          style = "flex: 3.5 !important; display: flex; flex-direction: column; gap: 0.1rem; margin-right: 40px !important",
          class = "d-grid",
          # Enter one or several collectivity names
          selectizeInput(
            inputId = ns("id"),
            label = tags$span("Nom de la collectivité",
                              popover(
                                tags$i(class = "fa fa-info-circle",
                                       style = "margin-left: 6px; position: relative;"),
                                HTML("<strong>À noter :</strong> ce filtre permet de saisir directement le nom d'une ou plusieurs collectivités pour les afficher sur la carte.<br>
                                     Dès qu’un nom est saisi, les autres filtres sont réinitialisés."
                                )
                              )
                    ),
            choices = generate_choices_grouped(actions_avelo),
            multiple = TRUE,
            options = list(maxOptions = 1000, placeholder = " Entrez une ou plusieurs collectivités")
          ),
          hr(style = "border-top: 2px solid #aaa; width: 350px; margin-top: 5px; margin-bottom: 15px;"),
          # Select one or several regions for display
          selectizeInput(
            inputId = ns("display_region"),
            label = "Sélection de régions",
            choices = extract_select_list_territories("region", data_cat_terr),
            multiple = TRUE,
            options = list(placeholder = " Toutes régions")
          ),
          # Select collectivity typology
          selectizeInput(
            inputId = ns("typology"),
            label = "Typologie de la collectivité",
            choices = setNames(
              c("COM", "CC", "CU","CA", "METRO", "EPT", "DEP", "REG", "PETR", "Syndicat intercommunal", "Syndicat Mixte", "Autre"),
              c("Communes", "Communautés de communes", "Communautés urbaines", "Communautés d'agglomération", "Métropoles", "Etablissements publics territoriaux", "Départements", "Régions", "Pôles d'équilibre territoriaux et ruraux", "Syndicats intercommunaux", "Syndicats mixtes", "Autres")
            ),
            multiple = TRUE,
            options = list(placeholder = " Toutes typologies")
          ),
          # If typology is département or commune, select a category
          div(#style = "width: 340px; !important",
              uiOutput(ns("category_select"))),
          # Select a program edition
          selectizeInput(
            inputId = ns("edition"),
            label = "Edition du programme",
            choices = setNames(
              c("1", "2", "3"),
              c("AVELO 1", "AVELO 2", "AVELO 3")
            ),
            multiple = TRUE,
            options = list(placeholder = " Toutes éditions")
          ),
          # Select a year
          selectizeInput(
            inputId = ns("years"),
            label = "Année d'accompagnement",
            choices = actions_avelo %>% arrange(YEAR) %>% pull(YEAR) %>% unique() %>% na.omit(),
            multiple = TRUE,
            options = list(placeholder = " Toutes années")
          ),
          # Select actions
          selectizeInput(
            inputId = ns("actions"),
            label = "Actions financées",
            choices = actions_avelo %>% distinct(Action, AXE) %>% split(.$AXE) %>% lapply(function(x) as.list(x$Action)),
            multiple = TRUE,
            options = list(maxOptions = NULL, placeholder = " Toutes actions")
          ),
          # Select targets
          selectizeInput(
            inputId = ns("targets"),
            label = "Cibles",
            choices = c("Grand public", "Jeunes et Scolaires", "Etudiants", "Employeurs", "Personnes âgées", "Personnes à mobilité réduite", "Personnes en recherche d'emploi", "Personnes en situation de précarité", "Autres cibles" ),
            multiple = TRUE,
            options = list(placeholder = " Toutes cibles")
          ),
          # Apply filters
          actionButton(ns("apply_btn"), "Appliquer les filtres", class = "btn btn-primary", style = "margin-top: 20px;"),
          # Reset filters
          div(
            class = "reset_bttn",
            actionButton(ns("reset_btn"), "Réinitialiser les filtres", style = "background-color: #294754 !important; color: #294754; border: none; margin-top: 10px;")
          )
        )
      ),
      # Display actions count for the current selection
      card(
        class = "actions_numbers",
        card_title(uiOutput(ns("action_count_title"))),
        card_body(
          uiOutput(ns("action_count_ui"))
        )
      )
      )
    ),

    # Comparison of key-indicator - AVELO vs non AVELO
    card(
      card_title("Comparaison d'indicateurs clés dans les territoires AVELO et non-AVELO", class = "h4"),
      card_body(
        div(style = "margin-left: 20px;",
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              # Select typology
              selectInput(
                inputId = ns("typology_comparison"),
                label = "Typologie de collectivité",
                choices = setNames(c("REG", "DEP", "EPCI", "COM"),
                                   c("Régions", "Départements", "EPCI", "Communes")),
                selected = "COM", width = "96%"
              ),
              # Select category (optional)
              div(style = "width: 100%; !important",
                  uiOutput(ns("category_ui"))),
              # Select territory (optional)
              selectizeInput(
                inputId = ns("territory_comparison"),
                label = HTML("Collectivité lauréate <small style='color:grey'><i>(optionnel)</i></small>"),
                choices = character(0),
                selected = character(0),
                multiple = FALSE, width = "96%",
                options = list(placeholder = " Toutes collectivités")
              ),
              div(
                class = "reset_bttn",
                actionButton(ns("reset_btn_comparison"), "Réinitialiser les filtres",
                             style = "height: 38px !important; display:flex !important; align-items:center !important;
                             width: 230px !important; margin-left: 5px; text-align:center !important; margin-top: 32px;
                             justify-content: center !important; background-color: #294754 !important;")
              )
            )
        ),
        # Evolution plot - Aménagements
        card(
          class = "graph_card",
          card_title("Aménagements cyclables",
                     tags$span(
                       bslib::popover(
                         icon("info-circle", class = "popover-info"),
                         HTML("Description et méthodologie taux de cyclabilité"),
                         options = list(
                           container = "body",
                           customClass = "popover-wide"
                         )
                       ),
                       style = "margin-left: 7px; position: relative;"
                     ),
                     span("   Source : Contributeurs OpenStreetMap | Juin 2024",
                          class = "fst-italic text-muted ms-2 fw-light",
                          style = "font-size: 0.80rem;")),
          card_body(
            div(style = "margin-left: 5px;",
                selectInput(ns("ac_indic_select"), "Indicateur à visualiser",
                            choices = setNames(
                              c("taux_cyclabilite", "voirie_hab"),
                              c("Taux de cyclabilité", "Linéaire de voirie cyclable par habitant")
                            ),
                            width = "310px")),
            layout_columns(
              col_widths = c(1, 10, 1),
              div(),
              plotlyOutput(ns("plot_ac")),
              div()
            )
          )
        ),
        # Evolution plot - Stationnements
        card(
          class = "graph_card",
          card_title("Stationnements vélo",
                     tags$span(
                       bslib::popover(
                         icon("info-circle", class = "popover-info"),
                         HTML("Description et méthodologie stationnements"),
                         options = list(
                           container = "body",
                           customClass = "popover-wide"
                         )
                       ),
                       style = "margin-left: 7px; position: relative;"
                     ),
                     span("   Source : Contributeurs OpenStreetMap | Juin 2024",
                          class = "fst-italic text-muted ms-2 fw-light",
                          style = "font-size: 0.80rem;")),
          card_body(
            div(style = "margin-left: 5px;",
                selectInput(ns("st_indic_select"), "Indicateur à visualiser",
                            choices = setNames(
                              c("stat_par_hab", "stat_par_km"),
                              c("Stationnements vélo par habitant", "Stationnements vélo par km de voirie cyclable")
                            ),
                            width = "310px")),
            layout_columns(
              col_widths = c(1, 8, 1),
              div(),
              plotlyOutput(ns("plot_st")),
              div()
            )
          )
        ),
        # Evolution plot - Part modale
        card(
          class = "graph_card",
          card_title("Part modale du vélo",
                     tags$span(
                       bslib::popover(
                         icon("info-circle", class = "popover-info"),
                         HTML("Description et méthodologie part modale"),
                         options = list(
                           container = "body",
                           customClass = "popover-wide"
                         )
                       ),
                       style = "margin-left: 7px; position: relative;"
                     ),
                     span("   Source : INSEE, Recensement de la population 2022 | Parution 2025",
                          class = "fst-italic text-muted ms-2 fw-light",
                          style = "font-size: 0.80rem;")),
          card_body(
            layout_columns(
              col_widths = c(1, 11),
              div(),
              div(
                style = "margin-right: 5%;",
                plotlyOutput(ns("plot_pm"))
              )
            )
          )
        )

      )
    ),

    scroll_arrow_ui()
  )
}

#' avelo Server Functions
#'
#' @noRd
mod_avelo_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- INTERACTIVE MAP SECTION ---

    # Dynamic UI to allow the user to choose a category when the selected typology is Départements or Communes
    output$category_select <- renderUI({
      req(input$typology)

      if (length(input$typology) != 1 || !(input$typology %in% c("DEP", "COM"))) {
        return(NULL)
      }

      df_filtered <- actions_avelo %>% filter(TYPO == input$typology) %>% filter(ID != "820") %>%
        select(CAT, LIB_CAT) %>% arrange(CAT) %>% distinct() %>% na.omit()

      choices <- if (input$typology == "COM") {
        generate_choices_grouped_communes(df_filtered)
      } else {
        setNames(df_filtered$CAT, df_filtered$LIB_CAT)
      }

      selectizeInput(
        inputId = ns("category"),
        label = paste0("Catégorie de ", ifelse(input$typology == "DEP", "départements", "communes")),
        choices = choices,
        multiple = TRUE,
        width = "340px",
        options = list(placeholder = " Toutes catégories")
      )
    })

    # Reset all selectInputs when a collectivity name is entered
    observeEvent(input$id, {
      if (!is.null(input$id) && length(input$id) > 0) {
        reset_avelo_inputs(session)
      }
    }, ignoreInit = TRUE)

    # Reset button : reset filters and map
    observeEvent(input$reset_btn, {
      reset_avelo_inputs(session)
      updateSelectizeInput(session, "id", selected = character(0))
      filtered_data(apply_filters(actions_avelo, admin_avelo))   # Comment to only reset filters
    })

    # Interactive map
    filtered_data <- reactiveVal(apply_filters(actions_avelo, admin_avelo))  # Stores data to display on the map

    # Apply button : apply filters selected by the user
    observeEvent(input$apply_btn, {
      filtered_data(apply_filters(
        actions_avelo, admin_avelo, input$id,
        input$display_region, input$typology, input$category, input$edition,
        input$years, input$actions, input$targets
      ))
    })

    # Map
    output$carto_avelo <- renderLeaflet({
      req(filtered_data())

      style <- list(
        REG    = list(fill="#e2e7a0", border="#cbd090", fillOpacity=0.7, borderOpacity=1, weight=1.5),
        DEP    = list(fill="#83bed2", border="#76abbd", fillOpacity=0.9, borderOpacity=0.9, weight=1.5),
        EPCI   = list(fill="#94a3a9", border="#768287", fillOpacity=1, borderOpacity=0.8, weight=1.5),
        COM    = list(fill="#f4a799", border="#c3867a", fillOpacity=1, borderOpacity=0.8, weight=1.5),
        POINTS = list(fill="#e0b867", border="#e0b867", fillOpacity=0.9, radius=5)
      )

      map_avelo(filtered_data(), contour_metropole, style, session$ns, admin_region_simple, isolate(input$display_region))
    })

    # Zoom on DROM with a select input
    observeEvent(input$territoire_select, {
      req(input$territoire_select)
      choix <- zones()[[input$territoire_select]]
      req(choix)

      leafletProxy("carto_avelo") %>%
        setView(lng = choix$lng, lat = choix$lat, zoom = choix$zoom)
    })

    # If a DROM is entered in selectInput "id", zoom on it
    observeEvent(input$id, {
      drom_ids <- list(
        "Réunion"   = c("166", "167", "168", "169", "170", "813", "814", "815", "816", "817", "818", "819", "883", "945", "974"),
        "Martinique"= c("034", "810"),
        "Guadeloupe"= c("031", "041", "164", "512", "807", "808", "809", "944"),
        "Mayotte"   = c("049", "250"),
        "Guyane"    = c("165", "182", "811", "812", "882"),
        "Nouvelle-Calédonie" = c("820")
      )

      if (is.null(input$id)){
        session$sendCustomMessage("maj_select_territoire", "France métropolitaine")
      } else {
        last_entered_id <- tail(input$id, 1)
        drom_name <- names(drom_ids)[sapply(drom_ids, function(x) last_entered_id %in% x)]

        if (length(drom_name) > 0){
          session$sendCustomMessage("maj_select_territoire", drom_name)
        } else {
          session$sendCustomMessage("maj_select_territoire", "France métropolitaine")
        }
      }
    })

    # If a region is entered in selectInput "display_region", zoom on it
    observeEvent(input$display_region, {
      region_ids <- list(
        "Réunion"   = c("04"),
        "Martinique"= c("02"),
        "Guadeloupe"= c("01"),
        "Mayotte"   = c("06"),
        "Guyane"    = c("03"),
        "France métropolitaine" = c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94")
      )

      if (is.null(input$display_region)){
        session$sendCustomMessage("maj_select_territoire", "France métropolitaine")
      } else {
        last_entered_region <- tail(input$display_region, 1)
        region_name <- names(region_ids)[sapply(region_ids, function(x) last_entered_region %in% x)]
        session$sendCustomMessage("maj_select_territoire", region_name)
      }
    })

    # Display actions count for the current selection
    output$action_count_title <- renderUI({
      HTML(paste0(
        "Nombre d'actions financées dans les <b style='color:#453582;'>",
        format(filtered_data() |> distinct(ID) |> nrow(), big.mark = " ", trim = TRUE),
        "</b> collectivités de la sélection courante"
      ))
    })

    output$action_count_ui <- renderUI({
      req(filtered_data())
      data <- filtered_data()

      # Check whether the filtered dataframe is empty or not
      if (nrow(data) == 0) {
        return(tags$div("Aucune collectivité correspondant aux filtres sélectionnés."))
      }

      # Compute actions count in current selection
      df_actions <- compute_action_counts(data)

      # Get top actions
      top_actions <- prepare_top_actions(df_actions, top_n = 4)

      # Generate UI
      render_action_counts_ui(session$ns, top_actions)
    })


    # --- KEY INDICATORS SECTION ---

    # Dynamic category UI for key-indicators interface : Render a selectInput for categories
    # only if the chosen typology requires it (DEP, EPCI, COM)
    output$category_ui <- renderUI({
      if (input$typology_comparison %in% c("DEP", "EPCI", "COM")) {
        selectInput(
          inputId = ns("category_comparison"),
          label = "Catégorie de collectivité",
          choices = get_category_choices(input$typology_comparison),
          selected = "", width = "96%"
        )
      }
    })

    # Dynamic territory list : Whenever typology and category change, filter the dataset accordingly
    # and update the available territories for the user to pick from.
    observe({
      req(input$typology_comparison)

      filtered_territories <- filter_territories_avelo(
        actions_avelo_for_plot_evolution,
        input$typology_comparison,
        if (input$typology_comparison %in% c("DEP","EPCI","COM")) input$category_comparison else NULL
      )

      updateSelectizeInput(session,
                           "territory_comparison",
                           choices = generate_territory_choices(filtered_territories),
                           selected = character(0),
                           server = TRUE
      )
    })

    # Reset filters
    observeEvent(input$reset_btn_comparison, {
      updateSelectInput(session, "typology_comparison", selected = "COM")
      updateSelectizeInput(session, "category_comparison", selected = character(0))
      updateSelectizeInput(session, "territory_comparison", selected = character(0))
    })

    # Selected category and territory
    selected_terr <- reactive({
      if (!is.null(input$territory_comparison) && input$territory_comparison != "") {
        input$territory_comparison
      } else {
        NULL
      }
    })

    selected_cat <- reactive({
      if (input$typology_comparison %in% c("DEP", "EPCI", "COM") &&
          !is.null(input$category_comparison) && input$category_comparison != "") {
        input$category_comparison
      } else {
        NULL
      }
    })

    # Evolution plot - Aménagements
    output$plot_ac <- renderPlotly({

      data_indicator <- switch(input$typology_comparison,
                               "REG" = amenagement_regional,
                               "DEP" = amenagement_departemental,
                               "EPCI" = amenagement_epci,
                               "COM" = amenagement_communal)

      plot_evolution_avelo(data_cat_terr, actions_avelo_for_plot_evolution,
                           data_indicator, input$typology_comparison,
                           selected_cat(), selected_terr(), input$ac_indic_select
      )
    })

    # Evolution plot - Stationnements
    output$plot_st <- renderPlotly({

      data_indicator <- switch(input$typology_comparison,
                               "REG" = stationnement_regional,
                               "DEP" = stationnement_departemental,
                               "EPCI" = stationnement_epci,
                               "COM" = stationnement_communal)

      plot_evolution_avelo(data_cat_terr, actions_avelo_for_plot_evolution,
                           data_indicator, input$typology_comparison,
                           selected_cat(), selected_terr(), input$st_indic_select
      )
    })

    # Evolution plot - Part modale
    output$plot_pm <- renderPlotly({

      data_indicator <- part_modale |>
        filter(insee_niveau == input$typology_comparison) |>
        rename(!!case_when(input$typology_comparison == "REG" ~ "INSEE_REG",
                           input$typology_comparison == "DEP" ~ "INSEE_DEP",
                           input$typology_comparison == "EPCI" ~ "INSEE_EPCI",
                           input$typology_comparison == "COM" ~ "INSEE_COM") := insee_code,
               year = annee)

      plot_evolution_avelo(data_cat_terr, actions_avelo_for_plot_evolution,
                           data_indicator, input$typology_comparison,
                           selected_cat(), selected_terr(), "part_modale"
      )
    })


    # Téléchargement CSV
    output$download <- downloadHandler(
      filename = function() {
        paste0("donnes_avelo_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        export_avelo(filtered_data(), file)
      }
    )

  })
}






## To be copied in the UI
# mod_avelo_ui("avelo_1")

## To be copied in the server
# mod_avelo_server("avelo_1")
