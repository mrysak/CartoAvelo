#' @title Return the HTML for the contact button in the UI
#'
#' @noRd
contactButton <- function(width = "140"){
  tags$a(
    href = "mailto:mona.rysak@reseau-velo-marche.org?subject=Demande%20d'information%20-%20Tableau%20de%20bord",
    class = "btn btn-primary",
    style = paste0("width: ", width, "px; text-align: center;"),
    "Contact")
}



#' @title Scroll Arrow UI Element
#'
#' @description Creates a fixed scroll arrow element that appears at the
#' bottom-right of the page, indicating to users that the page can be scrolled down.
#' The arrow disappears automatically when the user reaches the bottom
#' of the page. It is designed for integration into Shiny modules or UI layouts.
#'
#' @return A Shiny UI element (`tags$div`) containing a circular blue background
#' with a white down arrow icon.
#'
#' @import shiny
#' @import bsicons
#' @noRd
scroll_arrow_ui <- function() {
  tags$div(
    bsicons::bs_icon("arrow-down", size = "25px"),
    id = "scrollArrow",
    tags$style(HTML("
      #scrollArrow {
        position: fixed;
        bottom: 30px;
        left: 97%;
        transform: translateX(-50%);
        background-color: #303876;
        border-radius: 50%;
        width: 40px;
        height: 40px;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        z-index: 9999;
        cursor: default;
        color: inherit;
      }

      #scrollArrow svg {
        fill: white !important;
      }

      .hidden {
        display: none !important;
      }
    ")),
    tags$script(HTML("
      document.addEventListener('scroll', function() {
        var arrow = document.getElementById('scrollArrow');
        var scrollTop = window.scrollY || document.documentElement.scrollTop;
        var windowHeight = window.innerHeight;
        var docHeight = Math.max(document.body.scrollHeight, document.documentElement.scrollHeight);

        if (scrollTop + windowHeight >= docHeight - 5) {
          arrow.classList.add('hidden');
        } else {
          arrow.classList.remove('hidden');
        }
      });
    "))
  )
}



#' @title Custom Horizontal Legend for Fiche Synthèse Tab
#'
#' @description Generates a horizontally centered legend designed to accompany
#' comparative histogram/line charts. The legend displays a filled rectangle
#' representing the selected territory, and three colored line segments (each with
#' a centered circular marker) corresponding to different comparison categories.
#'
#' @param territory Character string. Label for the selected territory (displayed
#' inside a filled dark-blue rectangle).
#' @param cat1 Character string. Label for the first comparison category
#' (displayed with an orange line and dot).
#' @param cat2 Character string. Label for the second comparison category
#' (displayed with a purple line and dot).
#' @param cat3 Character string. Label for the third comparison category
#' (displayed with a green line and dot).
#'
#' @return
#' A Shiny UI element (`tags$div`) containing a horizontal legend, styled with
#' inline CSS for alignment, spacing, and color consistency.
#'
#' @noRd
legend_histo_fiche_synthese <- function(territory, cat1, cat2 = NULL, cat3 = NULL){

  if (is.null(cat2) & is.null(cat3)){
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 25px; margin-top: -20px; font-size: 14px;",
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "width: 22px; height: 14px; background-color: #294754; border-radius: 3px;"),
        tags$span(territory)
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "position: relative; width: 32px; height: 2px; background-color: #C6CE41;",
                 tags$div(style = "position: absolute; left: 50%; top: 50%; width: 8px; height: 8px; background-color: #C6CE41; border-radius: 50%; transform: translate(-50%, -50%);")
        ),
        tags$span(cat1)
      )
    )
  } else {
    tags$div(
      style = "display: flex; justify-content: center; align-items: center; gap: 25px; margin-top: -20px; font-size: 14px;",
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "width: 22px; height: 14px; background-color: #294754; border-radius: 3px;"),
        tags$span(territory)
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "position: relative; width: 32px; height: 2px; background-color: #E94F35;",
                 tags$div(style = "position: absolute; left: 50%; top: 50%; width: 8px; height: 8px; background-color: #E94F35; border-radius: 50%; transform: translate(-50%, -50%);")
        ),
        tags$span(cat1)
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "position: relative; width: 32px; height: 2px; background-color: #9185BE;",
                 tags$div(style = "position: absolute; left: 50%; top: 50%; width: 8px; height: 8px; background-color: #9185BE; border-radius: 50%; transform: translate(-50%, -50%);")
        ),
        tags$span(cat2)
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 6px;",
        tags$div(style = "position: relative; width: 32px; height: 2px; background-color: #C6CE41;",
                 tags$div(style = "position: absolute; left: 50%; top: 50%; width: 8px; height: 8px; background-color: #C6CE41; border-radius: 50%; transform: translate(-50%, -50%);")
        ),
        tags$span(cat3)
      )
    )
  }

}


