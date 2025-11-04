#' @title Filter AVELO program data based on user selection
#'
#' @description This function filters a data frame containing AVELO program data according to
#' multiple optional criteria provided by the user. Each filter is applied only
#' if its corresponding argument is not \code{NULL}.
#' It supports filtering by identifiers, typologies,
#' categories, program editions, years, financed actions, and action targets.
#'
#' @param actions_avelo A data frame containing AVELO program data.
#' @param admin_avelo A sf object containing the administrative boundaries of AVELO territories
#' @param id Optional. A vector of administrative unit identifiers (\code{ID}) to keep.
#' @param display_region Optional. A vector of regions to display.
#' @param typology Optional. A vector of typology codes (\code{TYPO}) to keep.
#' @param category Optional. A vector of category codes (\code{CAT}) to keep.
#' @param edition Optional. A vector of AVELO program edition numbers (\code{N_AVELO}) to keep.
#' @param years Optional. A vector of years (\code{YEAR}) to keep.
#' @param actions Optional. A vector of character strings; rows are kept if \code{Action}
#'   contains (case-insensitive) at least one of these strings within the same \code{ID} group.
#' @param targets Optional. A vector of character strings; rows are kept if \code{Cible}
#'   contains (case-insensitive) at least one of these strings within the same \code{ID} group.
#'
#' @return A filtered data frame containing only rows matching the specified criteria.
#' If all arguments are \code{NULL}, the original \code{data_avelo} is returned unchanged.
#'
#' @import dplyr
#' @import purrr
#' @importFrom stringr str_detect fixed
#' @noRd
apply_filters <- function(actions_avelo, admin_avelo, id = NULL,
                          display_region = NULL, typology = NULL, category = NULL, edition = NULL,
                          years = NULL, actions = NULL, targets = NULL){

  if (!is.null(display_region)){
    actions_avelo <- actions_avelo |> filter(REG %in% display_region)
    admin_avelo <- admin_avelo |> filter(REG %in% display_region)
  }

  data_avelo <- left_join(actions_avelo, admin_avelo, by = "ID") %>%
    select(ID, everything()) %>%
    mutate(YEAR = as.character(YEAR), N_AVELO = as.character(N_AVELO)) %>%
    { if (!is.null(id))       filter(., ID %in% id)                      else . } %>%
    { if (!is.null(typology)) filter(., TYPO %in% typology)              else . } %>%
    { if (!is.null(category)) filter(., CAT %in% category)               else . } %>%
    { if (!is.null(edition))  filter(., N_AVELO %in% edition)            else . } %>%
    { if (!is.null(years))    filter(., YEAR %in% years)                  else . } %>%
    { if (!is.null(actions)) group_by(., ID) %>%
        filter(any(Action %in% actions)) %>%
        ungroup() else . } %>%
    { if (!is.null(targets))  group_by(., ID) %>%
        filter(any(reduce(map(targets, ~ str_detect(Cible, fixed(.x, ignore_case = TRUE))), `|`))) %>%
        ungroup()                                  else . }

  return(data_avelo)
}



#' @title Generate formatted choice list of AVELO collectivities
#'
#' @description This function creates a named list suitable for `selectizeInput`
#' in Shiny, where options correspond to AVELO collectivities. Each option is
#' displayed as "Name (INSEE code)" and grouped by typology of the collectivity, while
#' the returned values are the corresponding IDs.
#'
#' @param actions_avelo A data frame containing AVELO actions data. Must include
#'   columns `ID` (collectivity identifier), `COG` (code), `NOM` (name), and
#'   `TYPO` (typology code).
#'
#' @return A named list of choices grouped by typology, where each group name is
#'   the full typology name, and each element is named by "Name (INSEE code)" with the
#'   corresponding ID as value. Suitable for use directly in Shiny's
#'   `selectizeInput`.
#'
#' @import dplyr
#' @import purrr
#' @noRd
generate_choices_grouped <- function(actions_avelo) {

  typo_labels <- c(
    COM = "Communes",
    CC = "Communautés de communes",
    CU = "Communautés urbaines",
    CA = "Communautés d'agglomération",
    METRO = "Métropoles",
    EPT = "Etablissements publics territoriaux",
    DEP = "Départements",
    REG = "Régions",
    PETR = "Pôles d'équilibre territoriaux et ruraux",
    `Syndicat intercommunal` = "Syndicats intercommunaux",
    `Syndicat Mixte` = "Syndicats mixtes",
    Autre = "Autres"
  )

  choices_grouped <- actions_avelo %>%
    mutate(
      TYPO = factor(TYPO, levels = names(typo_labels)),
      label = paste0(NOM, " (", COG, ")")
    ) %>%
    arrange(TYPO, NOM) %>%
    distinct(ID, COG, NOM, TYPO, label) %>%
    split(.$TYPO) %>%
    purrr::set_names(~ typo_labels[.x]) %>%
    lapply(function(x) setNames(x$ID, x$label))

  return(choices_grouped)
}



#' @title Reset AVELO Input Filters in a Shiny Session
#'
#' @description This utility function resets all AVELO-related input widgets in
#' a Shiny application to an empty selection. It is typically used to provide a
#' "reset filters" functionality in the user interface.
#'
#' @param session A Shiny `session` object passed automatically to server functions.
#'
#' @importFrom shiny updateSelectInput updateSelectizeInput
#' @noRd
reset_avelo_inputs <- function(session) {
  updateSelectizeInput(session, "display_region", selected = character(0))
  updateSelectizeInput(session, "typology", selected = character(0))
  updateSelectizeInput(session, "edition", selected = character(0))
  updateSelectizeInput(session, "years", selected = character(0))
  updateSelectizeInput(session, "actions", selected = character(0))
  updateSelectizeInput(session, "targets", selected = character(0))
  updateSelectizeInput(session, "category", selected = character(0))
}



#' @title Display AVELO collectivities on an Interactive Leaflet Map
#'
#' @description This function generates an interactive Leaflet map of AVELO collectivities,
#' differentiated according to their territorial typology (regions, departments,
#' intercommunal structures, municipalities, and others). The input data should
#' already be pre-filtered according to the user’s selection.
#'
#' The function assigns different layers and colors to each typology,
#' adds informative tooltips, and provides a legend for interpretation.
#' Users can also download the map in various formats.
#'
#' In addition to displaying collectivities, the map embeds a custom
#' **territory selection control** (a drop-down menu integrated directly in
#' the Leaflet map) that allows users to quickly zoom between mainland France
#' ("Métropole") and French overseas territories (Guadeloupe, Martinique,
#' Guyane, Réunion, Mayotte). This menu is linked to the Shiny input
#' \code{input$territoire_select}.
#'
#' @param data_avelo A `data.frame` or `sf` object containing filtered AVELO collectivities.
#' @param contour_metropole An `sf` object containing the geographical boundaries of France
#' @param styles A named list of lists, defining colors/opacities per typology.
#' @param admin_regeion An `sf` object containing the administrative regions
#'   geometries, with at least columns `INSEE_REG` and `geometry`.
#' @param display_region A vector containing the user-selected regions whose contours are to be displayed.
#'
#' @return A `leaflet` map widget that can be rendered in a Shiny application or
#' an R Markdown document.
#'
#' @importFrom dplyr left_join select filter rename
#' @importFrom sf st_as_sf st_coordinates st_set_geometry
#' @importFrom leaflet leaflet addProviderTiles setView addMapPane addPolygons
#'   addCircleMarkers addControl highlightOptions pathOptions
#' @importFrom htmltools tags HTML
#' @importFrom leaflet.extras2 addEasyprint easyprintOptions
#' @noRd
map_avelo <- function(data_avelo, contour_metropole, style, ns, admin_region, display_region){

  # Tooltip labels
  labels_avelo <- tooltip_label_avelo(data_avelo)

  data_avelo <- data_avelo |>
    left_join(labels_avelo |> select(ID, label_html), by = c("ID"), relationship = "many-to-many")

  # Legend
  legend_html <- tags$table(
    style = "background:white; padding:5px; border-radius:8px; width:215px",
    tags$tr(
      tags$td(legend_colors(fill = "#e8ebb3", border = style$REG$border)),
      tags$td("Régions")
    ),
    tags$tr(
      tags$td(legend_colors(fill = style$DEP$fill, border = style$DEP$border)),
      tags$td("Départements")
    ),
    tags$tr(
      tags$td(legend_colors(fill = style$EPCI$fill, border = style$EPCI$border)),
      tags$td("EPCI")
    ),
    tags$tr(
      tags$td(legend_colors(fill = style$COM$fill, border = style$COM$border)),
      tags$td("Communes")
    ),
    tags$tr(
      tags$td(legend_colors(fill = style$POINTS$fill, border = style$POINTS$border, shape = "point")),
      tags$td("Autres (PETR, syndicats mixtes...)")
    )
  )

  # Convert into sf object
  data_avelo <- st_as_sf(data_avelo)

  # Divide into layers according to the territory typology
  reg <- data_avelo %>% filter(TYPO == "REG")
  dep <- data_avelo %>% filter(TYPO == "DEP")
  epci <- data_avelo %>% filter(TYPO %in% c("CC", "CA", "CU", "METRO", "EPT"))
  com <- data_avelo %>% filter(TYPO == "COM" & COG != "98817")
  points <- data_avelo %>% filter(!(TYPO %in% c("REG","DEP","CA","CC","CU","METRO","EPT","COM")) | COG == "98817")

  points_coords <- tryCatch({
    coords <- st_coordinates(points)
    cbind(points, coords) %>%
      dplyr::rename(lon = X, lat = Y) %>%
      st_set_geometry(NULL)
  }, error = function(e) NULL)

  # Map
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels",
                     options = leafletOptions(maxZoom = 9)) %>%
    setView(lng = 2.30, lat = 46.50, zoom = 5.5) %>%

    safe_addPolygons(
      data = contour_metropole,
      color = "#000000", weight = 1.5, opacity = 0.6,
      fill = FALSE
    ) %>%

    addMapPane("pane-outline-reg", 400) %>%
    addMapPane("pane-reg", 410) %>%
    addMapPane("pane-dep", 420) %>%
    addMapPane("pane-epci", 430) %>%
    addMapPane("pane-com", 480) %>%
    addMapPane("pane-points", 490) %>%
    addMapPane("pane-dep_peri", 500) %>%
    addMapPane("pane-reg_peri", 510)

  if (!is.null(display_region)) {
    map <- map %>%
      safe_addPolygons(
        data = admin_region |> filter(INSEE_REG %in% display_region),
        color = "#000000", weight = style$REG$weight, opacity = style$REG$borderOpacity,
        fill = FALSE,
        options = pathOptions(pane = "pane-outline-reg")
      )
    }

  map <- map %>%
    # REG
    safe_addPolygons(
      data = reg,
      color = style$REG$border, weight = style$REG$weight, opacity = style$REG$borderOpacity,
      fillColor = style$REG$fill, fillOpacity = style$REG$fillOpacity,
      highlightOptions = highlightOptions(color = "#000000", weight = 2.5, bringToFront = TRUE, opacity = 1, fillColor = "#e2e7a0", fillOpacity = 0.6),
      label = ~lapply(label_html, HTML),
      labelOptions = labelOptions(textsize = "12px"),
      layerId = ~ID, options = pathOptions(pane = "pane-reg")
    ) %>%

    # DEP
    safe_addPolygons(
      data = dep,
      color = style$DEP$border, weight = style$DEP$weight, opacity = style$DEP$borderOpacity,
      fillColor = style$DEP$fill, fillOpacity = style$DEP$fillOpacity,
      highlightOptions = highlightOptions(color = "#000000", weight = 2.5, bringToFront = TRUE, opacity = 1, fillColor = "#83bed2", fillOpacity = 0.7),
      label = ~lapply(label_html, HTML),
      labelOptions = labelOptions(textsize = "12px"),
      layerId = ~ID, options = pathOptions(pane = "pane-dep")
    ) %>%

    # EPCI
    safe_addPolygons(
      data = epci,
      color = style$EPCI$border, opacity = style$EPCI$borderOpacity,
      fillColor = style$EPCI$fill, fillOpacity = style$EPCI$fillOpacity, weight = style$EPCI$weight,
      highlightOptions = highlightOptions(color = "#1b2d36", weight = 2.5, bringToFront = TRUE, opacity = 1, fillColor = "#94a3a9", fillOpacity = 0.7),
      label = ~lapply(label_html, HTML),
      labelOptions = labelOptions(textsize = "12px"),
      layerId = ~ID,
      options = pathOptions(pane = "pane-epci")
    ) %>%

    # COM
    safe_addPolygons(
      data = com,
      color = style$COM$border, opacity = style$COM$borderOpacity,
      fillColor = style$COM$fill, fillOpacity = style$COM$fillOpacity, weight = style$COM$weight,
      highlightOptions = highlightOptions(color = "#8c2f20", weight = 2.5, bringToFront = TRUE, opacity = 1, fillColor = "#f4a799", fillOpacity = 0.7),
      label = ~lapply(label_html, HTML),
      labelOptions = labelOptions(textsize = "12px"),
      layerId = ~ID,
      options = pathOptions(pane = "pane-com")
    ) %>%

    # Other typologies : points
    safe_addCircleMarkers(
      data = points_coords,
      lng = ~lon, lat = ~lat,
      color = style$POINTS$border, fillColor = style$POINTS$fill, fillOpacity = style$POINTS$fillOpacity, radius = style$POINTS$radius,
      label = ~lapply(label_html, HTML),
      labelOptions = labelOptions(textsize = "12px"),
      layerId = ~ID,
      options = pathOptions(pane = "pane-points")
    ) %>%

    addControl(html = as.character(legend_html), position = "bottomleft") %>%

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carto_avelo",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  # Add select input to set the view on a DROM
  map <- map %>%
    addControl(
      html = HTML(
        paste0(
          '<label for="', ns("territoire_select"), '" style="font-weight:bold; display:block; margin-bottom:5px;">Vue (métropole ou DROM) :</label>',
          '<select id="', ns("territoire_select"), '" ',
          'style="font-size:14px; padding:2px; border: 1px solid #ccc; width: 215px">',
          paste0('<option value="', names(zones()), '">', names(zones()), '</option>', collapse = ''),
          '</select>'
        )
      ),
      position = "bottomleft"
    )

  return(map)

}




#' @title Create a Legend Symbol for Leaflet Maps
#'
#' @description This helper function generates a small HTML element representing
#' a colored symbol (square or circle) to be used inside a Leaflet map legend.
#' It is typically combined with [htmltools::tags] and placed in a custom legend
#' created via [leaflet::addControl()].
#'
#' @param fill A character string specifying the fill color of the symbol.
#'   Defaults to `"transparent"`.
#' @param border A character string specifying the border color of the symbol.
#'   Defaults to `"black"`.
#' @param shape A character string specifying the shape of the symbol.
#'   Must be either `"square"` (default) or `"point"` (circle).
#'
#' @return An HTML `div` element created with [htmltools::tags].
#'
#' @importFrom htmltools tags
#' @noRd
legend_colors <- function(fill = "transparent", border = "black", shape = "square") {
  if (shape == "point") {
    tags$div(
      style = paste0(
        "width:12px; height:12px; border-radius:50%;",
        "background:", fill, "; border:2px solid ", border, ";"
      )
    )
  } else {
    tags$div(
      style = paste0(
        "width:12px; height:12px;",
        "background:", fill, "; border:2px solid ", border, ";"
      )
    )
  }
}



#' @title Generate HTML Tooltips for AVELO collectivities
#'
#' @description This function creates HTML-formatted labels to be used as tooltips in the
#' interactive AVELO map. Each label contains the collectivity’s name, the AVELO
#' program edition, the year, and the list of funded actions. If the action is
#' not "Recrutement chargé de mission vélo", its target audience (`Cible`) is also shown.
#'
#' @param data_avelo A `data.frame` or `sf` object containing information on AVELO collectivities.
#'
#' @return A `data.frame` with one row per collectivity, containing:
#' \describe{
#'   \item{ID}{Awardee identifier.}
#'   \item{NOM}{Awardee name.}
#'   \item{N_AVELO}{AVELO edition.}
#'   \item{YEAR}{Year of award.}
#'   \item{actions_html}{HTML-formatted description of funded actions.}
#'   \item{label_html}{Full HTML label for use in [leaflet::addPolygons()] or
#'   [leaflet::addCircleMarkers()].}
#' }
#'
#' @importFrom dplyr select group_by summarise mutate
#' @noRd
tooltip_label_avelo <- function(data_avelo){

  labels_avelo <- data_avelo %>%
    select(-geometry) %>%
    group_by(ID, NOM, N_AVELO, YEAR) %>%
    summarise(
      actions_html = paste0(
        "<b>Actions financées :</b><br>",
        paste0(
          Action,
          ifelse(
            Action == "Recrutement chargé·e de mission vélo",
            "",
            paste0("<br><small style='margin-left:10px; display:inline-block;'>Cible : ", ifelse(is.na(Cible), "-", Cible), "</small>")
          ),
          collapse = "<br>")
      ),
      .groups = "drop"
    ) %>%
    mutate(
      label_html = paste0(
        "<b>", NOM, "</b><br>",
        "Lauréat AVELO ", N_AVELO, " en ", YEAR,
        ifelse(N_AVELO == 3, " (projets toujours en cours)", ""),
        "<br><br>",
        actions_html
      )
    )

  return(labels_avelo)
}



#' @title Transform an action label for UI display
#'
#' @description This function transforms a given AVELO action label
#' into a formatted version for display in the UI.
#'
#' @param text A single character string representing the category to transform.
#'
#' @return A character string with the transformed action title. If the input
#' does not match any known category, it is returned unchanged.
#'
#' @noRd
transform_avelo_actions <- function(text) {
  dico <- c(
    "Etude de stratégie /de planification /plan vélo" = "Etudes de stratégie / planification",
    "Schéma Directeur Cyclable / Modes actifs" = "Schémas directeurs cyclables",
    "Plan de circulation" = "Plans de circulation",
    "Etude de maîtrise d'œuvre /de la mise en œuvre d'aménagements" = "Etudes de maîtrise d'œuvre / mise en œuvre",
    "Etude jalonnement" = "Etudes de jalonnement",
    "Etude d'évaluation" = "Etudes d'évaluation",
    "Etude stationnement" = "Etudes de stationnement",
    "Autres études (topographie, aménagements tectiques, faune/flore, point noir, etc.)" = "Autres études",
    "Achat flotte de vélos (hors location)" = "Achats de flotte de vélos",
    "Mise en place d’un service de location longue ou moyenne durée" = "Services de location longue ou moyenne durée",
    "Arceaux simples de stationnement" = "Installations d'arceaux",
    "Totems gonflage et/ou réparation" = "Installations de totems de gonflage / réparation",
    "Compteurs vélo" = "Installations de compteurs vélo",
    "Ateliers réparation / auto-réparation / réparation mobile" = "Ateliers de réparation",
    "Maisons du vélo" = "Maisons du vélo",
    "Vélo-écoles" = "Vélo-écoles",
    "Convoi d'enfants avec leurs vélos" = "Convois d'enfants avec leurs vélos",
    "Vélo-bus" = "Vélo-bus",
    "Signalétique / jalonnement / marquage" = "Signalétiques / jalonnements / marquages",
    "Services intermodalité vélo (prêt de vélos pliants et antivol, casiers en gare, emport des vélos dans les bus/car, etc.)" = "Services intermodalité vélo",
    "Cartographie dynamique" = "Cartographies dynamiques",
    "Autres" = "Autres actions",
    "Événements / animation" = "Événements / animations",
    "Actions de communications (hors événementiel)" = "Actions de communications",
    "Recrutement chargé·e de mission vélo" = "Recrutements chargé·e de mission vélo"
  )

  if (text %in% names(dico)) {
    return(dico[[text]])
  } else {
    return(text)
  }
}



#' @title Compute counts per funded action
#'
#' @description Given a filtered AVELO dataset, computes the counts for each action.
#' If the action is "Recrutement chargé de mission vélo", sum the ETP column;
#' otherwise, count distinct IDs.
#'
#' @param data A data.frame returned by filtered_data().
#'
#' @return A data.frame with two columns: Action and Nombre.
#' @noRd
compute_action_counts <- function(data) {
  actions <- unique(data$Action)

  nombres <- sapply(actions, function(action) {
    df <- data %>% filter(Action == action)
    df %>% distinct(ID) %>% nrow()
  })

  nb_etp <- data %>% filter(Action == "Recrutement chargé·e de mission vélo") |>
    summarise(total_ETP = sum(ETP, na.rm = TRUE)) %>%
    pull(total_ETP) %>% round(1) %>% format(decimal.mark = ",")

  df_actions <- data.frame(
    Action = actions,
    Nombre = nombres,
    stringsAsFactors = FALSE
  ) %>% arrange(desc(Nombre))

  # Clean action titles
  df_actions$Action <- sapply(df_actions$Action, function(a) {
    transform_avelo_actions(a)
  }, USE.NAMES = FALSE)

  df_actions <- df_actions %>%
    mutate(Nombre = paste0("<strong>", Nombre, "</strong>")) %>%
    mutate(
      Action = ifelse(Action == "Recrutements chargé·e de mission vélo",
                      paste0(Action, "<br>(", nb_etp, " ETP financés)"),
                      Action))
  # mutate(Nombre = ifelse(Action == "Recrutements chargé·e de mission vélo",
  #                        paste0(Nombre, " (", nb_etp, " ETP financés)"),
  #                        Nombre))

  return(df_actions)
}



#' @title Prepare top actions with toggle information
#'
#' @description Prepares a data.frame suitable for UI rendering of top actions.
#'
#' @param df_actions A data.frame returned by compute_action_counts().
#' @param top_n Number of top actions to show before toggle. Default is 5.
#'
#' @return A list with elements:
#'   - df_top: data.frame of top actions (up to top_n)
#'   - df_extra: data.frame of remaining actions (to show in toggle)
#'   - show_toggle: logical, TRUE if there are more actions than top_n
#' @noRd
prepare_top_actions <- function(df_actions, top_n = 5) {
  show_toggle <- nrow(df_actions) > top_n
  df_top <- df_actions[seq_len(min(top_n, nrow(df_actions))), , drop = FALSE]
  df_extra <- if (show_toggle) df_actions[(top_n + 1):nrow(df_actions), , drop = FALSE] else NULL
  list(df_top = df_top, df_extra = df_extra, show_toggle = show_toggle)
}



#' @title Render actions count UI
#'
#' @description Generates the Shiny UI for displaying the top actions with a toggle button
#' for extra actions.
#'
#' @param ns Shiny namespace function
#' @param top_actions List returned by prepare_top_actions()
#'
#' @return A Shiny tagList with the UI elements
#' @noRd
render_action_counts_ui <- function(ns, top_actions) {
  tagList(
    div(
      id = ns("actions_list"),
      lapply(seq_len(nrow(top_actions$df_top)), function(i) {
        #tags$div(HTML(paste0(top_actions$df_top$Action[i], " : ", top_actions$df_top$Nombre[i])))
        tags$div(HTML(paste0(top_actions$df_top$Nombre[i], top_actions$df_top$Action[i])))
      }),
      if (top_actions$show_toggle) {
        div(
          id = ns("extra_actions"),
          style = "display:none;",
          lapply(seq_len(nrow(top_actions$df_extra)), function(i) {
            #tags$div(HTML(paste0(top_actions$df_extra$Action[i], " : ", top_actions$df_extra$Nombre[i])))
            tags$div(HTML(paste0(top_actions$df_extra$Nombre[i], top_actions$df_extra$Action[i])))
          })
        )
      }
    ),
    if (top_actions$show_toggle) {
      tags$button(
        id = ns("toggle_top"),
        class = "btn btn-link p-0 mt-0.5",
        style = "text-align:left;",
        type = "button",
        onclick = sprintf("
          var extra = document.getElementById('%s');
          var btn = document.getElementById('%s');
          if (extra.style.display === 'none') {
            extra.style.display = 'block';
            btn.innerText = 'Voir moins';
          } else {
            extra.style.display = 'none';
            btn.innerText = 'Voir plus';
          }
        ", ns("extra_actions"), ns("toggle_top")),
        "Voir plus"
      )
    }
  )
}



#' @title Generate formatted legend labels for AVELO categories
#'
#' @description This function builds human-readable labels for AVELO categories (AVELO 1, 2, 3, and non-AVELO)
#' adapted to the selected territorial typology and, if applicable, a sub-category.
#' The labels are intended to be used in plots and legends to ensure consistent display.
#'
#' @param typology A character string specifying the territorial typology.
#'   Supported values are:
#'   \itemize{
#'     \item \code{"REG"}: Regions
#'     \item \code{"DEP"}: Departments
#'     \item \code{"EPCI"}: Intercommunalities
#'     \item \code{"COM"}: Municipalities
#'   }
#' @param category An optional sub-category within the typology.
#'
#' @noRd

generate_legend_label_avelo <- function(typology, category = NULL) {

  # Définir le préfixe en fonction de typology et category
  base <- switch(typology,
                 REG = "Régions ",
                 DEP = if (!is.null(category)) {
                   switch(as.character(category),
                          "1" = "Départements urbains ",
                          "2" = "Départements intermédiaires ",
                          "3" = "Départements ruraux ",
                          stop("Catégorie départementale non reconnue"))
                 } else {
                   "Départements "
                 },
                 EPCI = if (!is.null(category)) {
                   switch(as.character(category),
                          "CC" = "CC ",
                          "CA" = "CA ",
                          "CU" = "CU ",
                          "METRO" = "Métropoles ",
                          "EPT" = "EPT ",
                          stop("Catégorie EPCI non reconnue"))
                 } else {
                   "EPCI "
                 },
                 COM = if (!is.null(category)) {
                   switch(as.character(category),
                          "1" = "Grands centres urbains ",
                          "2" = "Communes intermédiaires ",
                          "3" = "Communes rurales ",
                          stop("Catégorie communale non reconnue"))
                 } else {
                   "Communes "
                 },
                 stop("Typologie non reconnue")
  )

  # Créer les labels AVELO
  labels <- list(
    av_1 = paste0(base, "AVELO 1"),
    av_2 = paste0(base, "AVELO 2"),
    av_3 = paste0(base, "AVELO 3"),
    not_av = paste0(base, "non AVELO")
  )

  return(labels)
}



#' @title Retrieve INSEE codes for AVELO program laureates and non-laureates
#'
#' @description This function extracts the INSEE codes of territories belonging to a given typology
#' (and optionally a specific category within that typology) and classifies them into:
#' - AVELO 1 laureates
#' - AVELO 2 laureates
#' - AVELO 3 laureates
#' - Non-AVELO territories
#'
#' It also generates properly formatted legend labels for each of these categories
#' to be displayed in bar charts or other visualizations.
#'
#' @param data_cat_terr A data frame containing territorial categorisation information
#'   for French administrative units.
#' @param actions_avelo A data frame containing information on AVELO program laureates,
#'   including typology (\code{TYPO}), category (\code{CAT}), edition of the program (\code{N_AVELO}),
#'   and corresponding INSEE codes (\code{COG}).
#' @param typology A character string specifying the territorial typology selected by the user.
#'   Accepted values include \code{"COM"} (communes), \code{"EPCI"}, \code{"DEP"} (départements), \code{"REG"} (régions).
#' @param category An optional category within the selected typology (e.g., type of commune or EPCI).
#'   If \code{NULL}, all categories within the typology are considered.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{list_av_1}}{Character vector of INSEE codes of AVELO 1 laureates.}
#'   \item{\code{list_av_2}}{Character vector of INSEE codes of AVELO 2 laureates.}
#'   \item{\code{list_av_3}}{Character vector of INSEE codes of AVELO 3 laureates.}
#'   \item{\code{list_not_av}}{Character vector of INSEE codes of non-AVELO territories.}
#'   \item{\code{legend_labels}}{A named list of labels for legend display in charts.}
#' }
#'
#' @import dplyr
#' @importFrom rlang sym
#' @noRd
get_insee_codes_avelo <- function(data_cat_terr, actions_avelo,
                                  typology, category){

  # Preliminary formatting
  # La manip commune devra être faite dans l'appli pour le selectInput, pourra être supprimée ici
  data_cat_terr <- data_cat_terr |>
    mutate(LIB_CAT_COM = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                                   CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                                   CAT_COM %in% c(5, 6, 7) ~ "Communes rurales"),
           CAT_COM = case_when(CAT_COM == 1 ~ 1,
                               CAT_COM %in% c(2, 3, 4) ~ 2,
                               CAT_COM %in% c(5, 6, 7) ~ 3,
                               TRUE ~ CAT_COM)) |>
    mutate(NATURE_EPCI = ifelse(NATURE_EPCI == "Etablissement public territorial", "EPT", NATURE_EPCI))

  # Retrieve the list of INSEE codes in each category (AVELO 1, 2 and 3 + not AVELO)
  if(!is.null(typology) & is.null(category)){

    # List territories in each AVELO program
    list_av_full <- actions_avelo |> filter(TYPO == typology) |> distinct(COG) |> pull(COG)
    list_av_1 <- actions_avelo |> filter(TYPO == typology) |> distinct(COG, N_AVELO) |> filter(N_AVELO == 1) |> pull(COG)
    list_av_2 <- actions_avelo |> filter(TYPO == typology) |> select(COG, N_AVELO) |> distinct() |> filter(N_AVELO == 2) |> pull(COG)
    list_av_3 <- actions_avelo |> filter(TYPO == typology) |> select(COG, N_AVELO) |> distinct() |> filter(N_AVELO == 3) |> pull(COG)
    list_not_av <- data_cat_terr |> select(all_of(typology)) |> distinct() |> filter(!(!!sym(typology) %in% list_av_full)) |> pull(!!sym(typology))

    legend_labels <- generate_legend_label_avelo(typology)

  } else if (!is.null(category)){
    col_to_filter <- switch(typology,
                            COM = "CAT_COM",
                            EPCI = "NATURE_EPCI",
                            DEP = "CAT_DEP")

    list_av_full <- actions_avelo |> filter(TYPO == typology & CAT == category) |> distinct(COG) |> pull(COG)
    list_av_1 <- actions_avelo |> filter(TYPO == typology & CAT == category) |> distinct(COG, N_AVELO) |> filter(N_AVELO == 1) |> pull(COG)
    list_av_2 <- actions_avelo |> filter(TYPO == typology & CAT == category) |> select(COG, N_AVELO) |> distinct() |> filter(N_AVELO == 2) |> pull(COG)
    list_av_3 <- actions_avelo |> filter(TYPO == typology & CAT == category) |> select(COG, N_AVELO) |> distinct() |> filter(N_AVELO == 3) |> pull(COG)
    list_not_av <- data_cat_terr |> filter(!!sym(col_to_filter) == category) |> select(all_of(typology)) |> distinct() |> filter(!(!!sym(typology) %in% list_av_full)) |> pull(!!sym(typology))

    legend_labels <- generate_legend_label_avelo(typology, category)
  }

  return(list(list_av_1 = list_av_1, list_av_2 = list_av_2, list_av_3 = list_av_3, list_not_av = list_not_av, legend_labels = legend_labels))
}




#' @title Compute and format user-selected indicator for visualization
#'
#' @description This function computes the value of a user-selected indicator
#' (e.g., cycling rate, road length per inhabitant) across groups of territories
#' defined by AVELO program participation (AVELO 1, 2, 3, or non-AVELO),
#' and formats the data into a long format suitable for ggplot visualizations.
#' Optionally, the function also computes the indicator for a single,
#' user-selected territory.
#'
#' @param data_indicator A data frame containing the raw data required to
#'   compute the selected indicator for the chosen typology.
#' @param list_insee_codes A named list of INSEE codes produced by
#'   \code{\link{get_insee_codes_avelo}}, containing:
#'   \itemize{
#'     \item \code{list_av_1}, \code{list_av_2}, \code{list_av_3}: AVELO laureate codes
#'     \item \code{list_not_av}: non-AVELO codes
#'     \item \code{legend_labels}: labels for each category
#'   }
#' @param typology A character string specifying the territorial typology
#'   (e.g., \code{"COM"}, \code{"EPCI"}, \code{"DEP"}, \code{"REG"}).
#' @param territory An optional identifier of a specific territory selected by the user.
#'   If not \code{NULL}, the indicator will also be computed and added for this territory.
#' @param indicator A character string specifying which indicator to compute.
#'   Currently supported values:
#'   \itemize{
#'     \item \code{"taux_cyclabilite"}: Cycling rate (weighted average of segments, expressed in \%)
#'     \item \code{"voirie_hab"}: Cyclable road length per inhabitant
#'     \item \code{"part_modale"} : Modal share of cycling in commuting (in \%)
#'     \item \code{"stat_par_hab"} : Number of bicycle parking spaces per inhabitant
#'     \item \code{"stat_par_km"} : Number of bicycle parking spaces per km of cyclable road
#'   }
#' @param indicator_title A character string giving a human-readable name
#'   for the indicator (used in tooltips and chart labels).
#' @param unit A character string specifying the unit of the indicator, used for display.
#'
#' @return A data frame in long format with the following columns:
#' \describe{
#'   \item{\code{year}}{Year of observation (derived from column suffix).}
#'   \item{\code{category}}{Territory group (AVELO 1, AVELO 2, AVELO 3, non-AVELO, or a specific territory).}
#'   \item{\code{value}}{Computed indicator value for the group or territory.}
#'   \item{\code{tooltip}}{A formatted string for interactive display in plots.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Assigns each territory to an AVELO category or non-AVELO.
#'   \item Pivots the data to long format and computes the selected indicator
#'         (weighted or normalized as appropriate).
#'   \item Optionally adds the computed indicator for a user-selected territory.
#'   \item Returns the final "ggplot-friendly" dataset.
#' }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom rlang sym
#' @noRd
compute_indicator_and_format <- function(data_indicator, actions_avelo, list_insee_codes,
                                         typology, territory, indicator,
                                         indicator_title, unit){

  list_av_1 = list_insee_codes$list_av_1
  list_av_2 = list_insee_codes$list_av_2
  list_av_3 = list_insee_codes$list_av_3
  list_not_av = list_insee_codes$list_not_av
  legend_labels = list_insee_codes$legend_labels

  prefix <- switch(indicator,
                   taux_cyclabilite = "TAUX_CYCL_",
                   voirie_hab = "NB_TOTAL_",
                   stat_par_hab = "NB_STAT_1000HAB_",
                   stat_par_km = "NB_STAT_1KM-AC_")

  # Assign the correct category (AVELO 1, 2, or 3, or non-AVELO) to each row of the dataframe
  # containing the aménagement data, and discard non-relevant territories
  df_cat <- data_indicator %>%
    mutate(COG = !!sym(paste0("INSEE_", typology))) %>%
    mutate(
      category = case_when(
        COG %in% list_av_1 ~ legend_labels$av_1,
        COG %in% list_av_2 ~ legend_labels$av_2,
        COG %in% list_av_3 ~ legend_labels$av_3,
        COG %in% list_not_av ~ legend_labels$not_av,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(category))

  # Format the dataframe (pivot longer if needed) and compute the selected indicator
  df_long <- if (indicator %in% c("taux_cyclabilite", "voirie_hab")) {

    df_cat %>%
      pivot_longer(
        cols = starts_with(prefix),
        names_to = "year",
        names_prefix = prefix,
        values_to = "value"
      ) %>%
      mutate(value = ifelse(is.infinite(value), NA_real_, value)) %>%   # To handle communes with Inf values
      group_by(year, category) %>%
      summarise(
        value = if (indicator == "taux_cyclabilite") {
          (sum(value * VOIRIE, na.rm = TRUE) / sum(VOIRIE, na.rm = TRUE)) * 100
        } else if (indicator == "voirie_hab") {
          sum(value, na.rm = TRUE) / sum(POP, na.rm = TRUE) * 1000
        },
        .groups = "drop"
      )

  } else if (indicator == "part_modale") {

    df_cat %>%
      group_by(year, category) %>%
      summarise(
        value = sum(pm_velo * nb_individu, na.rm = TRUE) / sum(nb_individu, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  } else if (indicator %in% c("stat_par_hab", "stat_par_km")){

    df_long <- df_cat %>%
      pivot_longer(
        cols = matches(paste0("(", prefix, "|KM_AMGT_)\\d{4}$")),
        names_to = c(".value", "year"),
        names_pattern = "(.*)_(\\d{4})"
      ) %>%
      rename(
        value = !!sub("_$", "", prefix),
        km_amgt = KM_AMGT
      ) %>%
      group_by(year, category) %>%
      summarise(
        value = if (indicator == "stat_par_hab") {
          sum(value * POP, na.rm = TRUE) / sum(POP, na.rm = TRUE)
        } else if (indicator == "stat_par_km") {
          sum(value * km_amgt, na.rm = TRUE) / sum(km_amgt, na.rm = TRUE)
        },
        .groups = "drop"
      )
  }

  # Add tooltips
  if (typology == "EPCI"){
    df_long <- df_long |>
      mutate(category_lib = case_when(
        str_detect(category, "CC") ~ str_replace(category, "CC", "communautés de communes"),
        str_detect(category, "CU") ~ str_replace(category, "CU", "communautés urbaines"),
        str_detect(category, "CA") ~ str_replace(category, "CA", "communautés d'agglomération"),
        str_detect(category, "EPT") ~ str_replace(category, "EPT", "établissements publics territoriaux"),
        str_detect(category, "METRO") ~ str_replace(category, "METRO", "métropoles"),
        TRUE ~ category)
      )
  } else {
    df_long <- df_long |>
      mutate(category_lib = paste0(tolower(substr(category, 1, 1)), substr(category, 2, nchar(category))))
  }

  df_long <- df_long %>%
    mutate(
      tooltip = paste0(
        indicator_title, " dans les \n",
        category_lib,
        " en ", year, " : <span style='font-weight:bold'>",
        format(round(value, 2), decimal.mark = ","), unit, "</span>"
      )
    )

  # Case when the user has selected a precise territory
  if (!is.null(territory)) {

    # Get the name and cog of the selected territory
    territory_infos <- actions_avelo %>%
      filter(ID == territory) %>%
      distinct(NOM, COG)
    territory_infos <- list(
      name = territory_infos |> pull(NOM),
      cog  = territory_infos |> pull(COG)
    )

    if (indicator %in% c("taux_cyclabilite", "voirie_hab")) {

      df_territory <- data_indicator %>%
        pivot_longer(
          cols = starts_with(prefix),
          names_to = "year",
          names_prefix = prefix,
          values_to = "raw_value"
        ) %>%
        filter(!!sym(paste0("INSEE_", typology)) == territory_infos$cog) %>%
        mutate(
          value = if (indicator == "taux_cyclabilite") {
            raw_value * 100
          } else if (indicator == "voirie_hab") {
            raw_value / POP
          },
          category = territory_infos$name,
          tooltip = paste0(
            indicator_title, " du territoire ", category,
            "\nen ", year, " : <span style='font-weight:bold'>",
            format(round(value, 2), decimal.mark = ","), unit, "</span>"
          )
        ) %>%
        filter(!is.na(value)) %>%
        select(year, category, value, tooltip)

    } else if (indicator == "part_modale") {

      df_territory <- data_indicator %>%
        filter(!!sym(paste0("INSEE_", typology)) == territory_infos$cog) %>%
        group_by(year) %>%
        summarise(
          value = sum(pm_velo * nb_individu, na.rm = TRUE) / sum(nb_individu, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        mutate(
          category = territory_infos$name,
          tooltip = paste0(
            indicator_title, " du territoire ", category,
            "\nen ", year, " : <span style='font-weight:bold'>",
            format(round(value, 2), decimal.mark = ","), unit, "</span>"
          )
        )
    } else if (indicator %in% c("stat_par_hab", "stat_par_km")){

      df_territory <- data_indicator |>
        pivot_longer(
          cols = matches(paste0("(", prefix, "|KM_AMGT_)\\d{4}$")),
          names_to = c(".value", "year"),
          names_pattern = "(.*)_(\\d{4})"
        ) %>%
        rename(
          value = !!sub("_$", "", prefix),
          km_amgt = KM_AMGT
        ) %>%
        filter(!!sym(paste0("INSEE_", typology)) == territory_infos$cog) %>%
        mutate(
          category = territory_infos$name,
          tooltip = paste0(
            indicator_title, " du territoire ", category,
            "\nen ", year, " : <span style='font-weight:bold'>",
            format(round(value, 2), decimal.mark = ","), unit, "</span>"
          )
        ) %>%
        filter(!is.na(value)) %>%
        select(year, category, value, tooltip)

      if (nrow(df_territory) == 0) {
        years <- df_long |> distinct(year) |> pull()

        df_territory <- tibble(
          year = years,
          category = territory_infos$name,
          value = 0) %>%
          mutate(
            tooltip = paste0(
              indicator_title, " du territoire ", territory_infos$name,
              "\nen ", year, " : <span style='font-weight:bold'>0", unit, "</span>"
            )
          )
      }
    }

    df_long <- bind_rows(df_long, df_territory)
  }

  return(df_long)
}



#' @title Plot the evolution of cycling indicators by AVELO category
#'
#' @description This function creates an interactive bar chart (using \pkg{ggplot2} and
#' \pkg{plotly}) showing the evolution of a selected cycling-related indicator over time,
#' for groups of territories (AVELO 1, AVELO 2, AVELO 3, non-AVELO) in the chosen typology
#' and category. Optionally, the indicator is also displayed for a specific user-selected territory.
#'
#' @param data_cat_terr A data frame containing territorial information required
#'   to classify each territory into AVELO categories (1, 2, 3, or non-AVELO).
#'   Typically corresponds to the dataset used for category-level grouping.
#' @param actions_avelo A data frame of AVELO laureates and related metadata
#'   (IDs, INSEE codes, names of territories, program edition, etc.).
#' @param data_indicator A data frame containing the infrastructure or indicator-related data,
#'   with variables needed for computing the indicator.
#' @param typology A character string specifying the territorial typology
#'   (e.g., \code{"COM"}, \code{"EPCI"}, \code{"DEP"}, \code{"REG"}).
#' @param category A category within the selected typology, used to filter
#'   and assign territories to AVELO groups (optional).
#' @param territory An optional identifier of a specific territory selected by the user.
#'   If not \code{NULL}, its indicator values are added as a separate bar in the chart.
#' @param indicator A character string specifying the indicator to display.
#'   Currently supported values:
#'   \itemize{
#'     \item \code{"taux_cyclabilite"}: Cycling rate of the road network (in \%)
#'     \item \code{"voirie_hab"}: Cyclable road length per inhabitant (in km/hab)
#'     \item \code{"part_modale"} : Modal share of cycling in commuting (in \%)
#'     \item \code{"stat_par_hab"} : Number of bicycle parking spaces per inhabitant
#'     \item \code{"stat_par_km"} : Number of bicycle parking spaces per km of cyclable road
#'   }
#'
#' @return A \code{plotly} object representing an interactive bar chart
#'   where bars correspond to indicator values by AVELO category (and optionally by territory)
#'   across years. Tooltips provide additional contextual information.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Retrieves AVELO category groupings using \code{\link{get_insee_codes_avelo}}.
#'   \item Computes the requested indicator with \code{\link{compute_indicator_and_format}}.
#'   \item Defines a consistent custom color palette for AVELO categories and the optional territory.
#'   \item Builds a grouped bar chart with \pkg{ggplot2}.
#'   \item Converts the chart into an interactive \pkg{plotly} object with tooltips.
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @importFrom tidyr pivot_longer
#' @noRd
plot_evolution_avelo <- function(data_cat_terr, actions_avelo, data_indicator,
                                 typology, category, territory, indicator){

  # Elements for display (name of indicator and corresponding unit)
  indicator_title <- switch(indicator,
                            taux_cyclabilite = "Taux de cyclabilité de la voirie",
                            voirie_hab = "Linéaire de voirie cyclable par habitant",
                            part_modale = "Part modale du vélo",
                            stat_par_hab = "Stationnements pour 1 000 habitants",
                            stat_par_km = "Stationnements par km d'aménagement")
  unit <- switch(indicator,
                 taux_cyclabilite = "%",
                 voirie_hab = " m/hab",
                 part_modale = "%",
                 stat_par_hab = " /1 000 hab",
                 stat_par_km = " /km")

  # Get lists of INSEE codes of the different categories of territories (AVELO 1, 2 or 3, or non-AVELO, in the selected typology + category)
  list_insee_codes <- get_insee_codes_avelo(data_cat_terr, actions_avelo,
                                            typology, category)

  # Format data and compute selected indicator
  data_to_display <- compute_indicator_and_format(data_indicator, actions_avelo, list_insee_codes,
                                                  typology, territory, indicator,
                                                  indicator_title, unit)


  legend_labels = list_insee_codes$legend_labels

  # Get the name and cog of the selected territory, if any
  if (!is.null(territory)){
    territory_infos <- actions_avelo |> filter(ID == territory) |> distinct(NOM, COG)
    territory_infos <- list(name = territory_infos |> pull(NOM), cog = territory_infos |> pull(COG))
  }

  # Set a custom color palette
  custom_colors <- setNames(
    c("#ef7858", "#294754", "#9185BE", "#b1d6e4", "#c6ce41"),
    c(if (is.null(territory)) "" else territory_infos$name, legend_labels$av_1, legend_labels$av_2, legend_labels$av_3, legend_labels$not_av)
  )

  # Set the order of the bars
  data_to_display <- data_to_display %>%
    mutate(category = factor(category, levels = names(custom_colors)))

  # ggplot chart
  p <- ggplot(data_to_display, aes(x = factor(year), y = value, fill = category, text = tooltip)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = custom_colors) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = " ")) +
    labs(
      x = "",
      y = paste0(indicator_title, " (", gsub(" ", "", unit), ")"),
      fill = ""
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "right",
      plot.margin = margin(t = 12, b = 0, l = 20, r = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = "#294754"),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 9)
    )

  # plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    plotly::layout(
      title = list(
        text = paste0(
          switch(indicator,
                 taux_cyclabilite = "Taux de cyclabilité de la voirie",
                 voirie_hab = "Linéaire de voirie cyclable par habitant",
                 part_modale = "Part modale du vélo dans les déplacements domicile-travail",
                 stat_par_hab = "Nombre de stationnements vélo par habitant",
                 stat_par_km = "Nombre de stationnements vélo par km d'aménagement cyclable"),
          "<span style='font-size:80%; font-weight:normal;'>",
          # " - ",
          # "To do",
          "</span>"
        ),
        x = 0.06,
        y = 1.1,
        font = list(size = 20, color = "#294754")
      ))

  return(g)
}



#' @title Get category choices for a given typology
#'
#' @description Returns the set of sub-category labels and codes to be used in a `selectInput`
#' depending on the selected territorial typology (DEP, EPCI, COM).
#'
#' @param typology A character string indicating the typology of collectivity.
#'   Supported values are `"DEP"`, `"EPCI"`, `"COM"`. If `"REG"`, returns `NULL`.
#'
#' @return A named character vector, where the names are the human-readable
#'   labels (e.g., "Communautés de communes") and the values are the internal
#'   codes (e.g., "CC"). Returns `NULL` if the typology does not require categories.
#'
#' @noRd
get_category_choices <- function(typology) {
  switch(typology,
         "DEP" = c(
           "Départements à prédominance urbaine" = "1",
           "Départements intermédiaires" = "2",
           "Départements à prédominance rurale" = "3",
           "Toutes catégories" = ""
         ),
         "EPCI" = c(
           "Communautés de communes" = "CC",
           "Communautés urbaines" = "CU",
           "Communauté d'agglomération" = "CA",
           "Métropoles" = "METRO",
           "Etablissements publics territoriaux" = "EPT",
           "Toutes catégories" = ""
         ),
         "COM" = c(
           "Grands centres urbains" = "1",
           "Communes intermédiaires" = "2",
           "Communes rurales" = "3",
           "Toutes catégories" = ""
         ),
         NULL
  )
}



#' @title Filter territories by typology and category
#'
#' @description Filters a dataset of territories according to the selected
#' typology and, optionally, a category.
#'
#' @param data A data frame containing at least the columns `TYPO` and `CAT`.
#' @param typology A character string indicating the typology (e.g., "DEP", "EPCI", "COM", "REG").
#' @param category A character string specifying the category code to filter by.
#'   If `NULL` or empty, no category filtering is applied.
#'
#' @return A filtered data frame, keeping only the rows matching the specified
#'   typology (and category if provided).
#'
#' @noRd
filter_territories_avelo <- function(data, typology, category = NULL) {
  filtered <- data %>% dplyr::filter(TYPO == typology)

  if (!is.null(category) && category != "") {
    filtered <- filtered %>% dplyr::filter(CAT == category)
  }
  filtered
}



#' @title Build territory choices list for a Selectize input
#'
#' @description Creates a named vector suitable for use as the `choices` argument in
#' `selectizeInput`. The labels are human-readable names (territory name + code),
#' and the values are the internal IDs. The list is sorted alphabetically.
#'
#' @param filtered_territories A data frame containing at least the columns
#'   `NOM`, `COG`, and `ID`. Typically the result of [filter_territories()].
#'
#' @return A named character vector where:
#'   - names are labels like `"Paris (75056)"`
#'   - values are the corresponding IDs.
#'   If no territories are provided, returns an empty character vector.
#'
#' @noRd
generate_territory_choices <- function(filtered_territories) {
  if (nrow(filtered_territories) == 0) return(character(0))

  labels <- paste0(filtered_territories$NOM, " (", filtered_territories$COG, ")")
  ord <- order(labels)

  setNames(filtered_territories$ID[ord], labels[ord])
}




#' @title Generate formatted choice list of communes types
#'
#' @description This function creates a named list suitable for `selectizeInput`
#' in Shiny, where options correspond to different types of municipalities.
#'
#' @param df A data frame containing the label of municipality category (LIB_CAT) and corresponding id (CAT)
#'
#' @return A named list of choices grouped by municipality types.
#'
#' @import dplyr
#' @import purrr
#' @noRd
generate_choices_grouped_communes <- function(df) {
  list(
    "Grands centres urbains" = setNames(df$CAT[df$CAT == 1], df$LIB_CAT[df$CAT == 1]),
    "Communes intermédiaires" = setNames(df$CAT[df$CAT %in% 2:4], df$LIB_CAT[df$CAT %in% 2:4]),
    "Communes rurales" = setNames(df$CAT[df$CAT %in% 5:7], df$LIB_CAT[df$CAT %in% 5:7])
  )
}



#' @title Export AVELO data to an Excel workbook for download
#'
#' @description This function filters, formats, and exports AVELO data into a
#' two-sheet Excel file. The first sheet ("Données AVELO") contains the cleaned
#' dataset, and the second sheet ("Informations") provides metadata with variable
#' descriptions. It is designed for use inside a Shiny `downloadHandler`.
#'
#' @param filtered_data A reactive dataframe containing AVELO data for territories selected by the user.
#' @param file A character string specifying the output Excel file path.
#'
#' @return No return value. The function creates an Excel file with two sheets:
#'   - **Données AVELO**: cleaned and formatted AVELO dataset
#'   - **Informations**: dictionary describing each exported variable
#'
#' @importFrom dplyr filter distinct pull mutate arrange select rename
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @noRd
export_avelo <- function(filtered_data, file) {

  ## Prepare dataset

  # download_avelo <- actions_avelo |>
  #   filter(ID %in% (filtered_data() |> distinct(ID) |> pull(ID))) |>
  download_avelo <- filtered_data |>
    mutate(TYPO = factor(
      TYPO,
      levels = c("REG", "DEP", "CC", "CU", "CA", "METRO", "EPT", "COM", "PETR",
                 "Syndicat intercommunal", "Syndicat Mixte", "Autre")
    )) |>
    arrange(TYPO, COG) |>
    select(COG, NOM, TYPO, LIB_CAT, N_AVELO, DATE, AXE, Action, Cible, ETP, `Durée`) |>
    rename(
      "Code INSEE"   = COG,
      "Nom"          = NOM,
      "Typologie"    = TYPO,
      "Catégorie"    = LIB_CAT,
      "Edition"      = N_AVELO,
      "Date"         = DATE,
      "Axe"          = AXE,
      "Durée ETP"    = `Durée`
    )

  ## Create variable dictionary : metadata explaining each column in the exported dataset.
  dict <- data.frame(
    Variable = c(
      "Source : ADEME | Août 2025", "",
      "Code INSEE", "Nom", "Typologie", "Catégorie", "Edition", "Date",
      "Axe", "Action", "Cible", "ETP", "Durée ETP"
    ),
    Description = c(
      "",
      "",
      "Code INSEE de la collectivité",
      "Nom de la collectivité",
      "Typologie de la collectivité (région, département, EPCI (communauté d'agglomération, communauté urbaine, communauté de communes, métropole, établissement public territorial), commune, collectivité à statut particulier (PETR, syndicats mixtes))",
      "Catégorie d'une collectivité au sein de sa typologie (par exemple : catégories de communes basées sur la grille de densité de l'INSEE)",
      "Édition du programme AVELO 1, 2 ou 3",
      "Date de contractualisation",
      "Axe concerné par l'action financée, parmi quatre axes",
      "Intitulé de l'action financée",
      "Cible de l'action financée",
      "Pour l'action 'Recrutement de chargé·e de mission vélo', nombre d'ETP financés dans la collectivité. NA pour les autres actions.",
      "Pour l'action 'Recrutement de chargé·e de mission vélo', durée pendant laquelle le poste est financé. NA pour les autres actions."
    ),
    stringsAsFactors = FALSE
  )

  ## Build Excel workbook
  wb <- openxlsx::createWorkbook()

  # Add first sheet with cleaned AVELO data
  openxlsx::addWorksheet(wb, "Données AVELO")
  openxlsx::writeData(wb, "Données AVELO", download_avelo)

  # Add second sheet with metadata dictionary
  openxlsx::addWorksheet(wb, "Informations")
  openxlsx::writeData(wb, "Informations", dict)

  # Save workbook to the specified file path
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}



