#' Intro panel
#'
#' @param id Module ID.
#' @param thematique Name of the thematic tab
#'
#' @return A UI tagList to include in a module.
#'
#' @export
mod_intro_panel_ui <- function(id, thematique) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),

    card(
      card_body(
        # Introduction text
        uiOutput(ns("intro")),
        # Layout for Méthodologie, Contact and Téléchargement buttons
        tags$div(
          style = "
              display: flex;
              justify-content: space-between;
              align-items: center;
            ",
          tags$div(
            style = "
                display: flex;
                gap: 10px;
              ",
            class = "intro_bttn",
            contactButton(),
            actionButton(ns("methodology"), "Méthodologie", class = "btn btn-primary", style = "width: 140px;")
          ),
          downloadButton(ns("download"), "Téléchargement")
        )
      )
    ),

    # Institutional partners
    if (thematique == "freq") {
      card(
        includeHTML(paste0("inst/text/", thematique, "_partenaires.html")),
        layout_columns(
          card(
            card_image(
              src  = "https://reseauvelomarche.github.io/images/logo_ministere.jpg",
              href = "https://www.ecologie.gouv.fr/",
              style = "width: 75%;"
            )
          ),
          card(
            card_image(
              src    = "https://reseauvelomarche.github.io/images/logo_ademe_cee.png",
              href   = "https://avelo.ademe.fr/"
            )
          ),
          card(
            card_body(
              class = "align-items-center",
              card_image(
                src  = "https://reseauvelomarche.github.io/images/logo_eco-compteur.svg",
                href = "https://www.eco-compteur.com/"
              )
            )
          )
        )
      )
    } else if (thematique == "accueil_velo") {
      card(
        includeHTML(paste0("inst/text/", thematique, "_partenaires.html")),
        style = "margin-bottom: 0; padding-bottom: 0;",
        layout_columns(
          card(
            card_image(
              src    = "https://reseauvelomarche.github.io/images/logo_ademe_cee.png",
              href   = "https://avelo.ademe.fr/"
            )
          ),
          card(
            card_body(
              class = "align-items-center",
              card_image(
                src  = "https://reseauvelomarche.github.io/images/logo_fvt.png",
                href = "https://www.francevelotourisme.com/accueil-velo"
              )
            )
          )
        )
      )
    } else if (thematique %in% c("amenagement", "part_modale", "stationnement")) {
      card(
        includeHTML(paste0("inst/text/", thematique, "_partenaires.html")),
          card(
            card_image(
              src    = "https://reseauvelomarche.github.io/images/logo_ademe_cee.png",
              href   = "https://avelo.ademe.fr/",
              style = "display: block; margin: 0 auto; width: 55%;"
            )
          )
      )
    } else if (thematique == "veloroute") {
      card(
        includeHTML(paste0("inst/text/", thematique, "_partenaires.html")),
        style = "margin-bottom: 0; padding-bottom: 0;",
        layout_columns(
          card(
            card_image(
              src    = "https://reseauvelomarche.github.io/images/logo_ministere_transports.png",
              href   = "https://www.ecologie.gouv.fr/",
              style = "width: 65%;"
            )
          ),
          card(
            card_body(
              class = "align-items-center",
              card_image(
                src  = "https://reseauvelomarche.github.io/images/logo_ademe_cee.png",
                href = "https://avelo.ademe.fr/"
              )
            )
          )
        )
      )
    } else {
      card(includeHTML(paste0("inst/text/", thematique, "_partenaires.html")))
    }
  )
}

#' Intro panel for Fiche Synthèse tab
#'
#' @param id Module ID.
#'
#' @return A UI tagList to include in a module.
#'
#' @export
mod_intro_panel_fiche_synthese_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(8, 4),

    card(
      card_body(
        # Introduction text
        uiOutput(ns("intro")),
        div(style = "width: 140px; align-items: left;",
            contactButton())
      )
    ),

    # Institutional partners
    card(
      includeHTML(paste0("inst/text/fiche_synthese_partenaires.html")),
      card(
        card_image(
          src    = "https://reseauvelomarche.github.io/images/logo_ademe_cee.png",
          href   = "https://avelo.ademe.fr/",
          style = "display: block; margin: 0 auto; width: 55%;"
        )
      )
    )
  )
}



#' Intro panel server logic with dynamic download
#'
#' @param id Module ID
#' @param thematique Name of the thematic tab
#' @param validated_selection Reactive input used for generating the intro
#' @param data_cat_terr Data passed to `generate_text_intro`
#' @param data_object A data.frame to download (e.g., accueil_velo_epci)
#' @param download_filename Name for the downloaded file (e.g., "accueil_velo_data")
#'
#' @export
mod_intro_panel_server <- function(id,
                                   thematique,
                                   validated_selection,
                                   data_cat_terr,
                                   data_object,
                                   download_filename = "data_export") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Introduction
    output$intro <- renderUI({
      generate_text_intro(paste0("inst/text/", thematique, "_intro.html"), validated_selection, data_cat_terr)
    })

    # Méthodologie (optionnelle)
    observeEvent(input$methodology, {
      showModal(modalDialog(
        title = "Méthodologie",
        HTML(paste(readLines(paste0("inst/text/", thematique, "_methodo.html")), collapse = "")),
        easyClose = TRUE,
        footer = modalButton("Fermer"),
        size = "l"
      ))
    })

    # Téléchargement CSV
    output$download <- downloadHandler(
      filename = function() {
        paste0(download_filename, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(data_object, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}

#' Intro panel server logic for Fiche Synthèse tab
#'
#' @param id Module ID
#' @param validated_selection Reactive input used for generating the intro
#' @param data_cat_terr Data passed to `generate_text_intro`
#' @param admin_sf Administrative data, containing population and surface data
#'
#' @export
mod_intro_panel_server_fiche_synthese <- function(id,
                                                  validated_selection,
                                                  data_cat_terr,
                                                  admin_sf = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Introduction
    output$intro <- renderUI({
      generate_text_intro_fiche_synthese(paste0("inst/text/fiche_synthese_intro.html"),
                                         validated_selection, data_cat_terr, admin_sf, actions_avelo)
    })
  })
}

