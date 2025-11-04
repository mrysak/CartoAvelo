#' general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_general_ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(

      # Sidebar
      sidebarPanel(
        class = "select_filters",
        width = 2,

        # The user selects a territory typology
        selectInput(ns("select_typology"), "Niveau de collectivité",
                    choices = select_list_typologies()),

        # The user selects a territory, depending on the selected typology (hence the use of conditionalPanel in `generate_territory_select_input`)
        generate_territory_select_input(ns, "france"),
        generate_territory_select_input(ns, "region"),
        generate_territory_select_input(ns, "departement"),
        generate_territory_select_input(ns, "epci"),
        generate_territory_select_input(ns, "commune"),

        # The user confirms his selection, allowing the main panel to update accordingly
        actionButton(ns("validate_territory_selection"), "Valider",
                     style = "height: 38px !important; display:flex !important; align-items:center !important;")
        ),

      # Main Panel with thematic tabs
      mainPanel(
        width = 10,
        tabsetPanel(
          id = ns("thematique"),

          # Fiche synthèse
          tabPanel("Fiche synthèse", value = "fs", uiOutput(ns("fs_ui"))),

          # Véloroutes
          tabPanel("Véloroutes", value = "vr", uiOutput(ns("vr_ui"))),

          # Aménagements
          tabPanel("Aménagements", value = "ac", uiOutput(ns("ac_ui"))),

          # Stationnements
          tabPanel("Stationnements", value = "st", uiOutput(ns("st_ui"))),

          # Accueil Vélo
          tabPanel("Accueil Vélo", value = "av", uiOutput(ns("av_ui"))),

          # Fréquentations
          tabPanel("Fréquentations", value = "fr", uiOutput(ns("fr_ui"))),

          # Parts modales
          tabPanel("Parts modales", value = "pm", uiOutput(ns("pm_ui")))
        )
      )
    )
  )
}



#' general Server Functions
#'
#' @noRd
mod_general_server <- function(id, validated_selection, admin_cache, veloroute_cache, current_year_ac, current_year_av, current_year_fr, current_year_vr, current_year_pm, current_year_st) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Tracking of modules that have already been loaded
    modules_loaded <- reactiveValues(
      fs_france = FALSE, fs_region = FALSE, fs_departement = FALSE, fs_epci = FALSE, fs_commune = FALSE,
      ac_france = FALSE, ac_region = FALSE, ac_departement = FALSE, ac_epci = FALSE, ac_commune = FALSE,
      av_france = FALSE, av_region = FALSE, av_departement = FALSE, av_epci = FALSE, av_commune = FALSE,
      fr_france = FALSE, fr_region = FALSE, fr_departement = FALSE, fr_epci = FALSE, fr_commune = FALSE,
      vr_france = FALSE, vr_region = FALSE, vr_departement = FALSE, vr_epci = FALSE, vr_commune = FALSE,
      pm_france = FALSE, pm_region = FALSE, pm_departement = FALSE, pm_epci = FALSE, pm_commune = FALSE,
      st_france = FALSE, st_region = FALSE, st_departement = FALSE, st_epci = FALSE, st_commune = FALSE
    )

    # -----------------------------
    # Fiche synthèse
    # -----------------------------
    output$fs_ui <- renderUI({
      req(input$thematique == "fs")

      tabsetPanel(
        id = ns("fs"),
        type = "hidden",
        tabPanel("fs_france", mod_fiche_synthese_france_ui(ns("fs_france"), current_year_fr, current_year_vr)),
        tabPanel("fs_region", mod_fiche_synthese_region_ui(ns("fs_region"), current_year_fr, current_year_vr)),
        tabPanel("fs_departement", mod_fiche_synthese_departement_ui(ns("fs_departement"), current_year_ac, current_year_st, current_year_vr)),
        tabPanel("fs_epci", mod_fiche_synthese_epci_ui(ns("fs_epci"), current_year_ac, current_year_st)),
        tabPanel("fs_commune", mod_fiche_synthese_commune_ui(ns("fs_commune"), current_year_ac, current_year_st))
      )
    })

    observeEvent(input$fs, ignoreInit = TRUE, {
      req(!is.null(input$fs))
      if (input$fs == "fs_france" && !modules_loaded$fs_france) {
        modules_loaded$fs_france <- TRUE
        mod_fiche_synthese_france_server("fs_france", validated_selection, current_year_ac, current_year_st, current_year_pm, current_year_av, current_year_fr, current_year_vr)
      }
      if (input$fs == "fs_region" && !modules_loaded$fs_region) {
        modules_loaded$fs_region <- TRUE
        mod_fiche_synthese_region_server("fs_region", validated_selection, current_year_ac, current_year_st, current_year_pm, current_year_av, current_year_fr, current_year_vr)
      }
      if (input$fs == "fs_departement" && !modules_loaded$fs_departement) {
        modules_loaded$fs_departement <- TRUE
        mod_fiche_synthese_departement_server("fs_departement", validated_selection, current_year_ac, current_year_st, current_year_vr)
      }
      if (input$fs == "fs_epci" && !modules_loaded$fs_epci) {
        modules_loaded$fs_epci <- TRUE
        mod_fiche_synthese_epci_server("fs_epci", validated_selection, current_year_ac, current_year_st, admin_cache)
      }
      if (input$fs == "fs_commune" && !modules_loaded$fs_commune) {
        modules_loaded$fs_commune <- TRUE
        mod_fiche_synthese_commune_server("fs_commune", validated_selection, current_year_ac, current_year_st, admin_cache)
      }
    })

    # -----------------------------
    # Aménagements
    # -----------------------------
    output$ac_ui <- renderUI({
      req(input$thematique == "ac")

      tabsetPanel(
        id = ns("ac"),
        type = "hidden",
        tabPanel("ac_france", mod_amenagement_france_ui(ns("ac_france"), current_year_ac)),
        tabPanel("ac_region", mod_amenagement_region_ui(ns("ac_region"), current_year_ac)),
        tabPanel("ac_departement", mod_amenagement_departement_ui(ns("ac_departement"), current_year_ac)),
        tabPanel("ac_epci", mod_amenagement_epci_ui(ns("ac_epci"), current_year_ac)),
        tabPanel("ac_commune", mod_amenagement_commune_ui(ns("ac_commune"), current_year_ac))
      )
    })

    observeEvent(input$ac, ignoreInit = TRUE, {
      req(!is.null(input$ac))
      if (input$ac == "ac_france" && !modules_loaded$ac_france) {
        modules_loaded$ac_france <- TRUE
        mod_amenagement_france_server("ac_france", validated_selection, current_year_ac, admin_cache)
      }
      if (input$ac == "ac_region" && !modules_loaded$ac_region) {
        modules_loaded$ac_region <- TRUE
        mod_amenagement_region_server("ac_region", validated_selection, current_year_ac, admin_cache)
      }
      if (input$ac == "ac_departement" && !modules_loaded$ac_departement) {
        modules_loaded$ac_departement <- TRUE
        mod_amenagement_departement_server("ac_departement", validated_selection, current_year_ac, admin_cache)
      }
      if (input$ac == "ac_epci" && !modules_loaded$ac_epci) {
        modules_loaded$ac_epci <- TRUE
        mod_amenagement_epci_server("ac_epci", validated_selection, current_year_ac, admin_cache)
      }
      if (input$ac == "ac_commune" && !modules_loaded$ac_commune) {
        modules_loaded$ac_commune <- TRUE
        mod_amenagement_commune_server("ac_commune", validated_selection, current_year_ac)
      }
    })

    # -----------------------------
    # Accueil Vélo
    # -----------------------------
    output$av_ui <- renderUI({
      req(input$thematique == "av")

      tabsetPanel(
        id = ns("av"),
        type = "hidden",
        tabPanel("av_france", mod_accueil_velo_france_ui(ns("av_france"), current_year_av)),
        tabPanel("av_region", mod_accueil_velo_region_ui(ns("av_region"), current_year_av)),
        tabPanel("av_departement", mod_accueil_velo_departement_ui(ns("av_departement"), current_year_av)),
        tabPanel("av_epci", mod_accueil_velo_epci_ui(ns("av_epci"), current_year_av)),
        tabPanel("av_commune", mod_accueil_velo_commune_ui(ns("av_commune"), current_year_av))
      )
    })

    observeEvent(input$av, ignoreInit = TRUE, {
      req(!is.null(input$av))
      if (input$av == "av_france" && !modules_loaded$av_france) {
        modules_loaded$av_france <- TRUE
        mod_accueil_velo_france_server("av_france", validated_selection, current_year_av)
      }
      if (input$av == "av_region" && !modules_loaded$av_region) {
        modules_loaded$av_region <- TRUE
        mod_accueil_velo_region_server("av_region", validated_selection, current_year_av)
      }
      if (input$av == "av_departement" && !modules_loaded$av_departement) {
        modules_loaded$av_departement <- TRUE
        mod_accueil_velo_departement_server("av_departement", validated_selection, current_year_av)
      }
      if (input$av == "av_epci" && !modules_loaded$av_epci) {
        modules_loaded$av_epci <- TRUE
        mod_accueil_velo_epci_server("av_epci", validated_selection, current_year_av)
      }
      if (input$av == "av_commune" && !modules_loaded$av_commune) {
        modules_loaded$av_commune <- TRUE
        mod_accueil_velo_commune_server("av_commune", validated_selection, current_year_av)
      }
    })

    # -----------------------------
    # Fréquentation
    # -----------------------------
    output$fr_ui <- renderUI({
      req(input$thematique == "fr")

      tabsetPanel(
        id = ns("fr"),
        type = "hidden",
        tabPanel("fr_france", mod_frequentation_france_ui(ns("fr_france"), current_year_fr)),
        tabPanel("fr_region", mod_frequentation_region_ui(ns("fr_region"), current_year_fr)),
        tabPanel("fr_departement", mod_frequentation_departement_ui(ns("fr_departement"), current_year_fr)),
        tabPanel("fr_epci", mod_frequentation_epci_ui(ns("fr_epci"), current_year_fr)),
        tabPanel("fr_commune", mod_frequentation_commune_ui(ns("fr_commune"), current_year_fr))
      )
    })

    observeEvent(input$fr, ignoreInit = TRUE, {
      req(!is.null(input$fr))
      if (input$fr == "fr_france" && !modules_loaded$fr_france) {
        modules_loaded$fr_france <- TRUE
        mod_frequentation_france_server("fr_france", validated_selection, current_year_fr)
      }
      if (input$fr == "fr_region" && !modules_loaded$fr_region) {
        modules_loaded$fr_region <- TRUE
        mod_frequentation_region_server("fr_region", validated_selection, current_year_fr)
      }
      if (input$fr == "fr_departement" && !modules_loaded$fr_departement) {
        modules_loaded$fr_departement <- TRUE
        mod_frequentation_departement_server("fr_departement", validated_selection, current_year_fr)
      }
      if (input$fr == "fr_epci" && !modules_loaded$fr_epci) {
        modules_loaded$fr_epci <- TRUE
        mod_frequentation_epci_server("fr_epci", validated_selection, current_year_fr)
      }
      if (input$fr == "fr_commune" && !modules_loaded$fr_commune) {
        modules_loaded$fr_commune <- TRUE
        mod_frequentation_commune_server("fr_commune", validated_selection, current_year_fr)
      }
    })

    # -----------------------------
    # Véloroutes
    # -----------------------------
    output$vr_ui <- renderUI({
      req(input$thematique == "vr")

      tabsetPanel(
        id = ns("vr"),
        type = "hidden",
        tabPanel("vr_france", mod_veloroute_france_ui(ns("vr_france"), current_year_vr)),
        tabPanel("vr_region", mod_veloroute_region_ui(ns("vr_region"), current_year_vr)),
        tabPanel("vr_departement", mod_veloroute_departement_ui(ns("vr_departement"), current_year_vr)),
        tabPanel("vr_epci", mod_veloroute_epci_ui(ns("vr_epci"), current_year_vr)),
        tabPanel("vr_commune", mod_veloroute_commune_ui(ns("vr_commune"), current_year_vr))
      )
    })

    observeEvent(input$vr, ignoreInit = TRUE, {
      req(!is.null(input$vr))
      if (input$vr == "vr_france" && !modules_loaded$vr_france) {
        modules_loaded$vr_france <- TRUE
        mod_veloroute_france_server("vr_france", validated_selection, current_year_vr, veloroute_cache)
      }
      if (input$vr == "vr_region" && !modules_loaded$vr_region) {
        modules_loaded$vr_region <- TRUE
        mod_veloroute_region_server("vr_region", validated_selection, current_year_vr, veloroute_cache)
      }
      if (input$vr == "vr_departement" && !modules_loaded$vr_departement) {
        modules_loaded$vr_departement <- TRUE
        mod_veloroute_departement_server("vr_departement", validated_selection, current_year_vr, veloroute_cache)
      }
      if (input$vr == "vr_epci" && !modules_loaded$vr_epci) {
        modules_loaded$vr_epci <- TRUE
        mod_veloroute_epci_server("vr_epci", validated_selection, current_year_vr)
      }
      if (input$vr == "vr_commune" && !modules_loaded$vr_commune) {
        modules_loaded$vr_commune <- TRUE
        mod_veloroute_commune_server("vr_commune", validated_selection, current_year_vr)
      }
    })

    # -----------------------------
    # Part modale
    # -----------------------------
    output$pm_ui <- renderUI({
      req(input$thematique == "pm")

      tabsetPanel(
        id = ns("pm"),
        type = "hidden",
        tabPanel("pm_france", mod_part_modale_france_ui(ns("pm_france"), current_year_pm)),
        tabPanel("pm_region", mod_part_modale_region_ui(ns("pm_region"), current_year_pm)),
        tabPanel("pm_departement", mod_part_modale_departement_ui(ns("pm_departement"), current_year_pm)),
        tabPanel("pm_epci", mod_part_modale_epci_ui(ns("pm_epci"), current_year_pm)),
        tabPanel("pm_commune", mod_part_modale_commune_ui(ns("pm_commune"), current_year_pm))
      )
    })

    observeEvent(input$pm, ignoreInit = TRUE, {
      req(!is.null(input$pm))
      if (input$pm == "pm_france" && !modules_loaded$pm_france) {
        modules_loaded$pm_france <- TRUE
        mod_part_modale_france_server("pm_france", validated_selection, current_year_pm, admin_cache)
      }
      if (input$pm == "pm_region" && !modules_loaded$pm_region) {
        modules_loaded$pm_region <- TRUE
        mod_part_modale_region_server("pm_region", validated_selection, current_year_pm, admin_cache)
      }
      if (input$pm == "pm_departement" && !modules_loaded$pm_departement) {
        modules_loaded$pm_departement <- TRUE
        mod_part_modale_departement_server("pm_departement", validated_selection, current_year_pm, admin_cache)
      }
      if (input$pm == "pm_epci" && !modules_loaded$pm_epci) {
        modules_loaded$pm_epci <- TRUE
        mod_part_modale_epci_server("pm_epci", validated_selection, current_year_pm, admin_cache)
      }
      if (input$pm == "pm_commune" && !modules_loaded$pm_commune) {
        modules_loaded$pm_commune <- TRUE
        mod_part_modale_commune_server("pm_commune", validated_selection, current_year_pm)
      }
    })

    # -----------------------------
    # Stationnements
    # -----------------------------
    output$st_ui <- renderUI({
      req(input$thematique == "st")

      tabsetPanel(
        id = ns("st"),
        type = "hidden",
        tabPanel("st_france", mod_stationnement_france_ui(ns("st_france"), current_year_st)),
        tabPanel("st_region", mod_stationnement_region_ui(ns("st_region"), current_year_st)),
        tabPanel("st_departement", mod_stationnement_departement_ui(ns("st_departement"), current_year_st)),
        tabPanel("st_epci", mod_stationnement_epci_ui(ns("st_epci"), current_year_st)),
        tabPanel("st_commune", mod_stationnement_commune_ui(ns("st_commune"), current_year_st))
      )
    })

    observeEvent(input$st, ignoreInit = TRUE, {
      req(!is.null(input$st))
      if (input$st == "st_france" && !modules_loaded$st_france) {
        modules_loaded$st_france <- TRUE
        mod_stationnement_france_server("st_france", validated_selection, current_year_st, admin_cache)
      }
      if (input$st == "st_region" && !modules_loaded$st_region) {
        modules_loaded$st_region <- TRUE
        mod_stationnement_region_server("st_region", validated_selection, current_year_st, admin_cache)
      }
      if (input$st == "st_departement" && !modules_loaded$st_departement) {
        modules_loaded$st_departement <- TRUE
        mod_stationnement_departement_server("st_departement", validated_selection, current_year_st, admin_cache)
      }
      if (input$st == "st_epci" && !modules_loaded$st_epci) {
        modules_loaded$st_epci <- TRUE
        mod_stationnement_epci_server("st_epci", validated_selection, current_year_st, admin_cache)
      }
      if (input$st == "st_commune" && !modules_loaded$st_commune) {
        modules_loaded$st_commune <- TRUE
        mod_stationnement_commune_server("st_commune", validated_selection, current_year_st)
      }
    })


    # -----------------------------
    # Typology and navigation management
    # -----------------------------

    # Update the input control for territory selection with auto-complete search
    observeEvent(input$select_typology, {
      if (input$select_typology %in% c("region", "departement", "epci", "commune")) {
        update_typology_selectize(
          input$select_typology,
          paste0("input_", input$select_typology),
          session,
          data_cat_terr
        )
      }
    })

    # Display the sub-tab corresponding to the selected territory after clicking "Valider"
    observeEvent(input$validate_territory_selection, {

      typology <- input$select_typology

      # Check that a territory has been selected
      switch(typology,
             "france" = req(input$input_france),
             "region" = req(input$input_region),
             "departement" = req(input$input_departement),
             "epci" = req(input$input_epci),
             "commune" = req(input$input_commune)
      )

      # Store the selected territory and typology in reactive (validated_selection)
      value <- switch(typology,
                      france = input$input_france,
                      region = input$input_region,
                      departement = input$input_departement,
                      epci = input$input_epci,
                      commune = input$input_commune)
      validated_selection[[typology]] <- value
      validated_selection$last_validated_typology <- typology

      # Corresponding sub-tab is updated
      selected_subtab <- paste0(input$thematique, "_", typology)
      updateTabsetPanel(session, input$thematique, selected = selected_subtab)
    })

    # Display the sub-tab corresponding to the selected and validated territory
    # when switching thematic tab
    observeEvent(input$thematique, {

      # Get the last validated typology (typology + territory not changing since
      # "Valider" button not clicked)
      typology <- get_validated_typology(validated_selection)

      # Corresponding sub-tab is updated
      selected_subtab <- paste0(input$thematique, "_", typology)
      updateTabsetPanel(session, input$thematique, selected = selected_subtab)
    })

  })
}



## To be copied in the UI
# mod_general_ui("general_1")

## To be copied in the server
# mod_general_server("general_1")
