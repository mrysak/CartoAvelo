#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinybusy
#' @noRd
#'
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    busy_start_up(
      loader = tags$img(
        src = "https://reseauvelomarche.github.io/images/logo_rvm_animated.gif",
        width = 1920/2
      ),
      mode = "auto"
    ),

    add_busy_bar(color = rvm_colour('cinnabar')),

    # CSS des Animals + customisation
    tags$head(
      tags$title("CartoAvelo"),
      tags$link(rel = "stylesheet", type = "text/css", href = "shiny-dashboard.css"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "https://www.reseau-velo-marche.org/shiny-dashboard.css"),
      tags$script(
        HTML('
          $(document).ready(function() {
            var containerHeight = $(".navbar .container-fluid").height() + "px";

            // Crée une nouvelle structure avec logo + titre
            $(".navbar-brand").html(
              "<img id=\'myImage\' src=\'https://reseauvelomarche.github.io/images/logo_rvm_header.png\'" +
              " height=\'" + containerHeight + "\' style=\'vertical-align: middle;\'>" +
              "<span style=\'margin-left:10px; vertical-align: middle;\'>Observatoire du vélo dans les territoires</span>"
            );
          });
        '
        ),
        HTML("
        $(document).ready(function(){
            $('[data-toggle=\"tooltip\"]').tooltip();
          });")
      ),

      tags$style(
        HTML("

            .h4 {
              font-size: 27px !important;
              font-weight: bold;
            }

            "),
        HTML('@media (max-width:992px) { .navbar-brand { padding-top: 0; padding-bottom: 0; }}')
      ),

      tags$style(
      HTML("
            #plot-container {
              position: relative;
            }

            #valuebox-overlay {
              position: absolute;
              top: 200px;
              right: 35px;
              z-index: 10;
              width: 300px;
            }
            ")
      ),

      tags$style(
      HTML("
            .popover.popover-wide {
              max-width: 5000px !important;
              width: 500px !important;
            }
            .popover.popover-wide .popover-body {
              white-space: normal;
            }
            ")
      ),

      tags$style(
      HTML("
            .card-header-tabs li:nth-child(1) {
              --color: #f2f2f2 !important;
              --color_active: #294754 !important;
            }

            .card-header-tabs li:nth-child(2) {
              --color: #f4a799 !important;
              --color_active: #c03c85 !important;
            }

            .card-header-tabs li:nth-child(3) {
              --color: #c8c2df !important;
              --color_active: #453582 !important;
            }

            .card-header-tabs li:nth-child(4) {
              --color: #e2e7a0 !important;
              --color_active: #b4be22 !important;
            }

            .card-header-tabs .nav-item .nav-link {
              color: var(--color_active) !important;
              background-color: transparent !important;
            }

            .card-header-tabs .nav-item .nav-link.active {
              color: var(--color_delft_blue_100) !important;
              background-color: var(--color) !important;
            }

            .card-header-tabs {
              margin-top: -5px;
            }
            ")
      )
    ),

    page_navbar(

      title = 'Observatoire du vélo dans les territoires',

      # Customisation Simon
      theme = bs_theme(
        bootswatch = "flatly",
        base_font = font_google("Poppins"),
        heading_font = font_google("Poppins"),
        bg = 'white',
        fg = rvm_colour('raisin_black'),
        primary =  rvm_colour('light_blue'),
        secondary = rvm_colour('light_blue')
      ),

      fillable = FALSE,

      nav_spacer(),

      nav_panel("Général",
                mod_general_ui("general")
      ),

      nav_panel("AVELO",
                mod_avelo_ui("avelo"))
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext= "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CartoAvelo"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
