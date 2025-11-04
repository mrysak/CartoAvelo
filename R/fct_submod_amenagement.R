#' @title Generate a correspondence table for cycling facilities types
#'
#' @description This function retrieves the full name of the different cycling facilities
#' types from the abbreviations used in the columns of the source files.
#'
#' @return Dataframe with abbreviations as first column and full names as second column
#'
#' @noRd
amenagement_types_table <- function(){
  data.frame(
    short = c("TOTAL", "PISTE", "VV", "BANDE", "DBSS", "CBV", "CVCB", "AUTRE"),
    long = c("Tout", "Piste cyclable", "Voie verte", "Bande cyclable",
             "Double sens cyclable", "Couloir bus+vélo", "CVCB", "Autres")
  )
}



#' @title Generate a tooltip label for an interactive cycling facilities map
#'
#' @description Creates a formatted HTML tooltip label string to display on a leaflet
#' map, showing the indicator value and its evolution from the previous year.
#'
#' @param value Numeric. The current value of the selected indicator, or its evolution.
#' @param label Numeric. Additional information to display in the label.
#' @param info_to_display Character. Either `"value"` or `"evol"` to determine which value to display as the main value.
#' @param indicator_choice Character. One of `"taux_cyclabilite"`, `"voirie_hab"`, `"voirie_km"`, or `"pop_prox"`.
#' Determines which indicator is displayed and the corresponding units and label.
#' @param current_year Character. The latest year with data.
#'
#' @return A character string with HTML formatting, ready to be used in a leaflet tooltip.
#' The string includes the main value, unit, and, if applicable, the evolution from the previous year.
#'
#' @noRd
tooltip_label_amenagement <- function(value, label, info_to_display, indicator_choice, current_year){

  general_label <- switch(indicator_choice,
                          taux_cyclabilite = "Taux de cyclabilité de la voirie : <span style='font-weight:bold'>",
                          voirie_hab = "Linéaire de voirie cyclable par habitant : <span style='font-weight:bold'>",
                          voirie_km = "Linéaire de voirie cyclable par km² : <span style='font-weight:bold'>",
                          pop_prox = "Pourcentage de population vivant à proximité</br>d'un aménagement cyclable : <span style='font-weight:bold'>")

  unit <- switch(indicator_choice,
                 taux_cyclabilite = c("%", " points"),
                 voirie_hab = c(" m/hab", "%"),
                 voirie_km = c(" m/km²", "%"),
                 pop_prox = c("%", ""))

  val <- ifelse(info_to_display == "value", value, label)

  evol <- ifelse(info_to_display == "value", label, value)

  if (indicator_choice != "pop_prox") {
    prefix <- ifelse(!is.na(evol) && evol >= 0, "+", "")
    evolution <- paste0("Evolution /", as.numeric(current_year) - 1, " : <span style='font-weight:bold'>",
                        prefix, format_val(evol, unit[2]), "</span>")
  }

  legend <- paste0(general_label, format_val(val), unit[1], "</span></br>", evolution)

  return(legend)
}



#' @title Interactive map of cycling infrastructure indicators using Leaflet
#'
#' @description Generates an interactive choropleth leaflet map of cycling infrastructure indicators
#' (e.g. cyclability rate, km of infrastructure per inhabitant, per km², or population proximity with cycling infrastructures),
#' including tooltips, legends, and export options.
#'
#' @param data_cat_terr A dataframe containing territorial categorisation information for French administrative units.
#' @param amenagement_data A dataframe containing cycling facilities data.
#' @param admin_sf sf object. Spatial administrative boundaries (communes, EPCI, départements, régions).
#' @param tab_typology Character. Typology of the displayed territory. One of `"france"`, `"region"`, `"departement"`, or `"epci"`.
#' @param display_typology Character. Administrative scale used for display. One of `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param selected_territory Character. INSEE code of the territory selected by the user.
#' @param indicator_choice Character. The cycling indicator to display. One of:
#' \itemize{
#'   \item `"taux_cyclabilite"` — Cyclability rate of the road network (%).
#'   \item `"voirie_hab"` — Length of cycling infrastructure per inhabitant (km/hab).
#'   \item `"voirie_km"` — Length of cycling infrastructure per square kilometer (km/km²).
#'   \item `"pop_prox"` — % of population living near cycling infrastructure.
#' }
#' @param info_to_display Character. Whether to display the value itself ("value") or its evolution from the previous year ("evol").
#' @param current_year Numeric. The latest year with data.
#' @param admin_cache `reactiveValues` — A shared reactive cache used across Shiny modules.
#' The loaded layers are stored here and reused if already present. Only useful when tab_typology == "epci", else set to NULL.
#'
#' @return A leaflet map object showing the chosen indicator across the selected geographic level,
#' with an appropriate tooltip and legend.
#'
#' @import dplyr
#' @import leaflet
#' @import htmltools
#' @import sf
#' @import leaflet.extras2
#' @importFrom htmltools HTML
#' @noRd
map_amenagement_indicators <- function(data_cat_terr, amenagement_data, admin_sf,
                                       tab_typology, display_typology,
                                       selected_territory, indicator_choice, info_to_display,
                                       current_year, admin_cache = NULL, ns){


  insee_col_admin <- switch(display_typology,
                            region = "INSEE_REG",
                            departement = "INSEE_DEP",
                            epci = "INSEE_EPCI",
                            commune = "INSEE_COM")

  insee_col_cat_terr <- switch(display_typology,
                               region = "REG",
                               departement = "DEP",
                               epci = "EPCI",
                               commune = "COM")


  # ---- PREPARE DATA ----

  # Join administrative data with aménagement data
  admin_sf <- admin_sf %>%
    left_join(amenagement_data %>% mutate(across(all_of(insee_col_admin), as.character)),
              by = insee_col_admin)

  # Extract data corresponding to the territory selected by the user, according to the selected display scale
  if (tab_typology != "france"){
    if(tab_typology == "region"){
      list <- data_cat_terr %>%
        filter(REG == selected_territory)
    } else if(tab_typology == "departement"){
      list <- data_cat_terr %>%
        filter(DEP == selected_territory)
    } else if(tab_typology == "epci"){
      list <- data_cat_terr %>%
        filter(EPCI == selected_territory)
    }
    list <- list %>% pull(.data[[insee_col_cat_terr]]) %>% unique()
    admin_sf <- admin_sf %>% filter(.data[[insee_col_admin]] %in% list)
  }

  # Get territory name depending on the selected display scale
  admin_sf <- admin_sf %>%
    mutate(territory_name = .data[[switch(
      display_typology,
      "commune" = "NOM_COM",
      "epci" = "NOM_EPCI",
      "departement" = "NOM_DEP",
      "region" = "NOM_REG"
    )]])


  # ---- ZOOM AND MAP CENTERING ----

  if(tab_typology == "france"){
    lng <- 2.30; lat <- 46.50; zoom <- 6
  } else if (tab_typology == "region") {
    centroids <- get_centroids(selected_territory, tab_typology)
    lng <- centroids$lng; lat <- centroids$lat
    if (selected_territory %in% c("75")){
      zoom <- 7
    } else if (selected_territory %in% c("76", "84", "03", "94", "44", "27")){
      zoom <- 7.5
    } else if (selected_territory %in% c("01", "02", "04", "06", "11")){
      zoom <- 9
    } else {zoom <- 8}
  } else if (tab_typology == "departement"){
    centroids <- get_centroids(selected_territory, tab_typology)
    lng <- centroids$lng; lat <- centroids$lat
    if (selected_territory %in% c("6AE", "26", "20R", "33")){
      zoom <- 8.5
    } else if (selected_territory %in% c("973")){
      zoom <- 8
    } else {zoom <- 9}
  } else if (tab_typology == "epci"){
    centroids <- get_centroids(selected_territory, tab_typology, admin_cache)
    lng <- centroids$lng; lat <- centroids$lat
    zoom <- 11
  }


  # ---- LEGEND TITLE ----

  legend_title <- case_when(
    info_to_display == "value" & indicator_choice == "taux_cyclabilite" ~
      "Taux de cyclabilité de la voirie (%)",
    info_to_display == "evol" & indicator_choice == "taux_cyclabilite" ~
      paste0("Evolution du taux de cyclabilité<br/>par rapport à ", as.numeric(current_year) - 1, " (points)"),
    info_to_display == "value" & indicator_choice == "voirie_hab" ~
      "Linéaire de voirie cyclable par<br/>habitant (m/hab)",
    info_to_display == "evol" & indicator_choice == "voirie_hab" ~
      paste0("Evolution du linéaire de voirie cyclable<br/>par habitant par rapport à ", as.numeric(current_year) - 1, " (%)"),
    info_to_display == "value" & indicator_choice == "voirie_km" ~
      "Linéaire de voirie cyclable par<br/>km² (m/km²)",
    info_to_display == "evol" & indicator_choice == "voirie_km" ~
      paste0("Evolution du linéaire de voirie cyclable<br/>par km² par rapport à ", as.numeric(current_year) - 1, " (%)"),
    indicator_choice == "pop_prox" ~
      "Pourcentage de population<br/>vivant à proximité d'un<br/>aménagement cyclable (%)",
    TRUE ~ "Légende"
  )


  # ----- ADDITIONAL DATA PREPARATION -----

  if (indicator_choice == "taux_cyclabilite"){

    # Compute evolution
    selected_col <- paste0("TAUX_CYCL_", current_year)
    previous_col <- paste0("TAUX_CYCL_", as.numeric(current_year) - 1)

    admin_sf <- admin_sf %>%
      mutate(!!selected_col := .data[[selected_col]] * 100) %>%
      mutate(!!previous_col := .data[[previous_col]] * 100) %>%
      mutate(EVOL = get(selected_col) - get(previous_col))

    # Rename columns
    admin_sf <- admin_sf  %>%
      rename(!!ifelse(info_to_display == "value", "value", "label") := all_of(selected_col),
             !!ifelse(info_to_display == "value", "label", "value") := EVOL)

  } else if (indicator_choice == "voirie_hab"){

    # Compute evolution
    selected_col <- paste0("NB_TOTAL_", current_year)
    previous_col <- paste0("NB_TOTAL_", as.numeric(current_year) - 1)

    admin_sf <- admin_sf %>%
      mutate(VOIRIE_HAB_CURRENT := .data[[selected_col]] / POP * 1000) %>%
      mutate(VOIRIE_HAB_PREVIOUS := .data[[previous_col]] / POP * 1000) %>%
      mutate(EVOL = if_else(
        VOIRIE_HAB_PREVIOUS != 0,
        (VOIRIE_HAB_CURRENT - VOIRIE_HAB_PREVIOUS) / VOIRIE_HAB_PREVIOUS * 100,
        NA_real_
      ))

    # Rename columns
    admin_sf <- admin_sf  %>%
      rename(!!ifelse(info_to_display == "value", "value", "label") := VOIRIE_HAB_CURRENT,
             !!ifelse(info_to_display == "value", "label", "value") := EVOL)

  } else if (indicator_choice == "voirie_km"){

    # Compute evolution
    selected_col <- paste0("NB_TOTAL_", current_year)
    previous_col <- paste0("NB_TOTAL_", as.numeric(current_year) - 1)

    admin_sf <- admin_sf %>%
      mutate(VOIRIE_KM_CURRENT := .data[[selected_col]] / SUPERFICIE * 1000) %>%
      mutate(VOIRIE_KM_PREVIOUS := .data[[previous_col]] / SUPERFICIE * 1000) %>%
      mutate(EVOL = if_else(
        VOIRIE_KM_PREVIOUS != 0,
        (VOIRIE_KM_CURRENT - VOIRIE_KM_PREVIOUS)/VOIRIE_KM_PREVIOUS * 100,
        NA_real_
      ))

    # Rename columns
    admin_sf <- admin_sf  %>%
      rename(!!ifelse(info_to_display == "value", "value", "label") := VOIRIE_KM_CURRENT,
             !!ifelse(info_to_display == "value", "label", "value") := EVOL)
  } else if (indicator_choice == "pop_prox"){

    selected_col <- paste0("POP_PROX_", current_year)

    # Rename columns
    admin_sf <- admin_sf  %>%
      mutate(value := .data[[selected_col]] * 100)
  }


  # ---- COLOR PALETTE AND LEGEND ----

  if (indicator_choice %in% c("voirie_hab", "voirie_km") & info_to_display == "evol") {

    color_palette_legend <- color_palette_map_evol(-10, 10,
                                                   c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                   c("< -10%", "-10% – -5%", "-5% – 0%", "0% – 5%", "5% – 10%", "> 10%"),
                                                   "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                   admin_sf)

    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "taux_cyclabilite" & info_to_display == "evol") {

    color_palette_legend <- color_palette_map_evol(-1, 1,
                                                   c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                   c("< -1", "-1 – -0,5", "-0,5 – 0", "0 – 0,5", "0,5 – 1", "> 1"),
                                                   "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                   admin_sf)

    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "taux_cyclabilite" & info_to_display == "value"){

    color_palette_legend <- color_palette_map_value(10,
                                                    c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                    c("0% – 3%", "3% – 6%", "6% – 10%", "> 10%"),
                                                    "#e9ecf3", "#303876",
                                                    admin_sf)

    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "voirie_hab" & info_to_display == "value" & display_typology %in% c("region", "departement")){

    color_palette_legend <- color_palette_map_value(3,
                                                    c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                    c("0 – 1,0", "1,0 – 2,0", "2,0 – 3,0", "> 3,0"),
                                                    "#f4f5d9", "#9ea534",
                                                    admin_sf)
    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "voirie_hab" & info_to_display == "value" & display_typology %in% c("epci", "commune")){

    color_palette_legend <- color_palette_map_value(6,
                                                    c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                    c("0 – 2,0", "2,0 – 4,0", "4,0 – 6,0", "> 6,0"),
                                                    "#f4f5d9", "#9ea534",
                                                    admin_sf)

    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "voirie_km" & info_to_display == "value" & display_typology %in% c("region", "departement", "epci", "commune")){

    color_palette_legend <- color_palette_map_value(300,
                                                    c("#e8f3f7", "#d9e6eb", "#bcd4dc", "#9fc1cd"),
                                                    c("0 – 100", "100 – 200", "200 – 300", "> 300"),
                                                    "#e8f3f7", "#9fc1cd",
                                                    admin_sf)

    pal <- color_palette_legend$pal
    legend_colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  }  else {

    colors_pal <- list(
      taux_cyclabilite = c(low = "#e9ecf3", high = "#303876"),
      voirie_hab = c(low = "#F6F9ED", high = "#C6CE41"),
      voirie_km = c(low = "#F1F7Fa", high = "#9fc1cd"),
      pop_prox = c(low = "#e8e6f4", high = "#9185be")
    )

    pal <- colorNumeric(
      palette = colorRampPalette(colors_pal[[indicator_choice]])(100),
      domain = admin_sf$value[is.finite(admin_sf$value)],
      na.color = "#d3d3d3"
    )

    finite_values <- admin_sf$value[is.finite(admin_sf$value)]
    if(length(finite_values) == 0){
      legend_colors <- "#d3d3d3"
      legend_labels <- "NA"
    } else {
      breaks <- pretty(finite_values, n = 5)
      if(length(breaks) < 2) breaks <- c(min(finite_values), max(finite_values))
      midpoints <- pmin(pmax((breaks[-length(breaks)] + breaks[-1]) / 2, min(finite_values)), max(finite_values))
      legend_colors <- pal(midpoints)

      if(indicator_choice %in% c("taux_cyclabilite", "pop_prox")){
        legend_labels <- paste0(
          format(breaks[-length(breaks)], decimal.mark = ","),
          ifelse(info_to_display == "value", "% – ", " – "),
          format(breaks[-1], decimal.mark = ","),
          ifelse(info_to_display == "value", "%", "")
        )
      } else if (indicator_choice %in% c("voirie_hab", "voirie_km")){
        legend_labels <- paste0(
          format(breaks[-length(breaks)], decimal.mark = ","),
          ifelse(info_to_display == "evol", "% – ", " – "),
          format(breaks[-1], decimal.mark = ","),
          ifelse(info_to_display == "evol", "%", "")
        )
      }
    }
  }


  # ---- LEAFLET MAP ----

  if (indicator_choice == "pop_prox") {
    val <- admin_sf$value; lab <- admin_sf$value
  } else {
    val <- admin_sf$value; lab <- admin_sf$label
  }

  map <- leaflet(admin_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%

    draw_territory_outline(
      selected_territory = selected_territory,
      typology = tab_typology,
      admin_cache = admin_cache) %>%

    addPolygons(
      fillColor = ~pal(value),
      color = "#ffffff",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.8,
      highlightOptions = highlightOptions(
        weight = 2.5,
        color = "#4d4d4d",
        fillOpacity = 1,
        bringToFront = TRUE),

      label = mapply(function(val, lab, name) {
        paste0(
          "<div style='margin-bottom:2px;'>",
          "<b><span style='font-size:1em;'>", name, "</span></b></div>",
          "<span style='font-size:0.9em;'>",
          tooltip_label_amenagement(val, lab, info_to_display, indicator_choice, current_year), "</span>"
        )
      }, val, lab, admin_sf$territory_name) %>%

        lapply(htmltools::HTML),

      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "14px",direction = "auto")
    ) %>%

    addLegend(
      colors = legend_colors,
      labels = legend_labels,
      opacity = 0.7,
      title = legend_title,
      position = "bottomleft",
      labFormat = labelFormat(big.mark = "")) %>%

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carte_amenagement",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  # Add select input to set the view on a DROM
  select_width <- case_when(
    indicator_choice == "taux_cyclabilite" & info_to_display == "value" ~ 237,
    indicator_choice %in% c("voirie_hab", "voirie_km") & info_to_display == "value" ~ 208,
    indicator_choice == "taux_cyclabilite" & info_to_display == "evol" ~ 215,
    indicator_choice %in% c("voirie_hab", "voirie_km") & info_to_display == "evol" ~ 265,
    indicator_choice == "pop_prox" ~ 180)

  if (tab_typology == "france"){
    map <- map %>%
      addControl(
        html = HTML(
          paste0(
            '<label for="', ns("territoire_select"), '" style="font-weight:bold; display:block; margin-bottom:5px;">Vue (métropole ou DROM) :</label>',
            '<select id="', ns("territoire_select"), '" ',
            'style="font-size:14px; padding:2px; border: 1px solid #ccc; width: ', select_width, 'px">',
            paste0('<option value="', names(zones()), '">', names(zones()), '</option>', collapse = ''),
            '</select>'
          )
        ),
        position = "bottomleft"
      )
  }

  return(map)
}



#' @title Generate key figures for cycling infrastructure value box
#'
#' @description This function computes and formats the key values to be displayed
#' in an interactive value box related to cycling infrastructure indicators.
#' It returns the title, the main numeric value (formatted), and its evolution
#' compared to the previous year.
#'
#' @param amenagement_data A data frame containing cycling infrastructure data,
#'   including yearly values for road length and population/surface metrics.
#' @param current_year Latest year with values.
#' @param ind A character string indicating which indicator to display.
#'   Must be one of `"taux_cyclabilite"`, `"voirie_hab"`, `"voirie_km"`, or `"pop_prox"`.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`title`}{A character string: the title to display for the indicator.}
#'   \item{`chiffre_cle`}{A formatted character string representing the main numeric value.}
#'   \item{`evol_cle`}{A formatted character string showing the change compared to the previous year, if applicable.}
#' }
#'
#' @noRd
value_box_content_amenagement <- function(amenagement_data, current_year, ind){

  # ----- TITLE -----

  title <- switch(ind,
                  "taux_cyclabilite" = "Taux de cyclabilité",
                  "voirie_hab" = "Voirie cyclable par habitant",
                  "voirie_km" = "Voirie cyclable par km²",
                  "pop_prox" = "Population proche d'une voirie cyclable")


  # ----- KEY FIGURE -----

  if (ind %in% c("taux_cyclabilite", "pop_prox")) {
    col_prefix <- switch(ind,
                         "taux_cyclabilite" = "TAUX_CYCL_",
                         "pop_prox" = "POP_PROX_"
    )
    value <- amenagement_data[[paste0(col_prefix, current_year)]] * 100
    chiffre_cle <- format_val(value, "%")

  } else if (ind %in% c("voirie_hab", "voirie_km")) {
    denom <- switch(ind,
                    "voirie_hab" = "POP",
                    "voirie_km" = "SUPERFICIE"
    )
    value <- amenagement_data %>%
      mutate(val = (.data[[paste0("NB_TOTAL_", current_year)]] / .data[[denom]]) * 1000) %>%
      pull(val)

    suffix <- switch(ind,
                     "voirie_hab" = " m/hab",
                     "voirie_km" = " m/km²"
    )
    chiffre_cle <- ifelse(ind == "voirie_hab",
                          paste0(format(round(value, 2), decimal.mark = ",", big.mark = " "), suffix),
                          paste0(format(round(value, 0), decimal.mark = ",", big.mark = " "), suffix)
    )
  }


    # ----- EVOLUTION -----

    year <- as.numeric(current_year)
    col_cur <- paste0("NB_TOTAL_", year)
    col_prev <- paste0("NB_TOTAL_", year - 1)

    if (ind == "taux_cyclabilite") {
      cur <- amenagement_data[[paste0("TAUX_CYCL_", year)]] * 100
      prev <- amenagement_data[[paste0("TAUX_CYCL_", year - 1)]] * 100
      evol <- cur - prev
      unit <- ifelse(abs(evol) < 2, " point", " points")

      evol_cle <- paste0(ifelse(evol < 0, "", "+"),
                         format_val(evol),
                         unit,
                         " par rapport à ", year - 1
                         )

    } else if (ind %in% c("voirie_hab", "voirie_km")) {
      denom <- switch(ind,
                      "voirie_hab" = "POP",
                      "voirie_km" = "SUPERFICIE"
      )

      evol_data <- amenagement_data %>%
        mutate(
          current = .data[[col_cur]] / .data[[denom]],
          previous = .data[[col_prev]] / .data[[denom]]
        )

      evol <- with(evol_data, (current - previous) / previous * 100)

      evol_cle <- paste0(ifelse((is.na(evol) | evol < 0), "", "+"),
                         format_val(evol, "%"),
                         " par rapport à ", year - 1
                         )
    } else if(ind == "pop_prox"){
      evol_cle <- ""
    }

    return(list(title = title, chiffre_cle = chiffre_cle, evol_cle = evol_cle))
}



#' @title Generate a formatted gt table for cycling infrastructures lengths with heatmap coloring
#'
#' @description This function generates a gt table with custom heatmap color scaling,
#' formatting, and styling, specifically designed to display cycling infrastructures lengths.
#'
#' @param data A dataframe containing the table data.
#' @param typology A character string specifying the typology of the selected territory.
#' @param color_low A string specifying the low color of the heatmap gradient. Default is "#F6F9ED".
#' @param color_high A string specifying the high color of the heatmap gradient. Default is "#C6CE41".
#' @param font_size An integer specifying font size in pixels. Default is 15.
#' @param font_family A string specifying the font family for the table. Default is "Poppins".
#'
#' @return A gt table object ready for display.
#' @import dplyr
#' @import gt
#' @import scales
#' @import stringr
#' @noRd
generate_amenagement_table <- function(data, typology, color_low = "#F6F9ED", color_high = "#C6CE41", font_size = 15, font_family = "Poppins") {

  # Identify useful rows
  total_row <- which(stringr::str_detect(data$LIB, "^Total"))
  evolution_rows <- which(stringr::str_detect(data$LIB, "^Evolution"))

  # Sort intermediate columns based on Total row values
  intermediate_cols <- names(data)[2:(ncol(data) - 1)]
  total_values <- data[total_row, intermediate_cols] %>% unlist(use.names = TRUE)
  sorted_cols <- names(sort(total_values, decreasing = TRUE))

  # Reorder data: first column + sorted intermediates + last column
  data <- data %>%
    select(
      LIB,
      all_of(sorted_cols),
      last_col()
    )

  # Recalculate heatmap rows and cols after reordering
  if (typology == "region") {
    heatmap_rows <- 1:(which(grepl("^Total", data$LIB)) - 1)
  } else {
    heatmap_rows <- 1
  }
  heatmap_cols <- 2:(ncol(data) - 1)
  heatmap_cols_names <- names(data)[heatmap_cols]
  cols_names <- names(data)[2:(ncol(data))]

  # Generate color scaler
  scaler <- heatmap_scaler(data[heatmap_rows, heatmap_cols], color_low, color_high)

  # Convert target columns to numeric for formatting safety
  data <- data %>%
    mutate(across(all_of(cols_names), ~ suppressWarnings(as.numeric(.))))

  # Build gt table
  gt_table <- data %>%
    gt() %>%

    # Heatmap coloring
    data_color(
      columns = heatmap_cols_names,
      rows = setdiff(heatmap_rows, evolution_rows),
      fn = function(x) {
        ifelse(x == 0 | is.na(x), "#FFFFFF", scaler(x))
      }
    ) %>%

    # Alignment and widths
    cols_align(align = "center") %>%
    cols_width(
      LIB ~ px(170),
      everything() ~ px(110)
    ) %>%
    cols_label(
      LIB = ""
    ) %>%

    # Bold style for Total row and Total column label
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_body(rows = total_row),
        cells_column_labels(columns = "Total")
      )
    ) %>%

    # White border before Evolution rows
    tab_style(
      style = cell_borders(sides = "top", color = "white", weight = px(10)),
      locations = cells_body(rows = evolution_rows)
    ) %>%

    # Format non-evolution rows: "x km", hide 0
    text_transform(
      locations = cells_body(
        rows = setdiff(1:total_row, evolution_rows),
        columns = cols_names
      ),
      fn = function(x) {
        x_num <- suppressWarnings(as.numeric(x))
        formatted <- ifelse(
          is.na(x_num) | x_num == 0,
          "",
          paste0(formatC(x_num, format = "f", digits = 0, big.mark = " "), " km")
        )
        formatted
      }
    ) %>%

    # Format Evolution rows as percentages with + or - and one decimal
    text_transform(
      locations = cells_body(
        rows = evolution_rows,
        columns = cols_names
      ),
      fn = function(x) {
        x <- suppressWarnings(as.numeric(x))
        formatted <- ifelse(
          is.na(x) | !is.finite(x),
          "-",
          paste0(ifelse(as.numeric(x) > 0, "+", ""), formatC(as.numeric(x), format = "f", digits = 0), "%")
        )
        formatted
      }
    ) %>%

    # Global table aesthetics
    tab_options(
      table.font.size = px(font_size),
      table.font.names = font_family,
      data_row.padding = px(6),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      heading.border.bottom.color = "white",
      column_labels.border.top.color = "white",
      column_labels.border.bottom.color = "white",
      row_group.border.top.color = "white",
      row_group.border.bottom.color = "white",
      summary_row.border.color = "white",
      grand_summary_row.border.color = "white",
      table_body.border.top.color = "white",
      table_body.border.bottom.color = "white",
      table_body.hlines.color = "white",
      table_body.vlines.color = "white"
    )

  return(gt_table)
}



#' @title Generate National Comparison Data for Cycling Infrastructure Indicators
#'
#' @description This function computes aggregated comparison statistics for a selected cycling infrastructure indicator
#' across three categories of French communes: *Grands centres urbains*, *Communes intermédiaires*,
#' and *Communes rurales*. The aggregation is done at the national level (i.e., all of France),
#' and returns a time series per category suitable for benchmarking visualizations.
#'
#' The commune typology is based on `CAT_COM` codes:
#' - `1` for *Grands centres urbains*
#' - `2, 3, 4` for *Communes intermédiaires*
#' - `5, 6, 7` for *Communes rurales*
#'
#' The indicators can be:
#' - `"taux_cyclabilite"`: cycling infrastructure share (as a weighted average over road length)
#' - `"voirie_hab"`: cycling infrastructure per inhabitant
#' - `"voirie_km"`: cycling infrastructure per km²
#'
#' @param data_cat_terr Dataframe containing territorial categorisation, including department codes, labels, region codes and labels.
#' @param amenagement_communal A data frame with cycling infrastructure data for each commune, including `VOIRIE`, `POP`, `SUPERFICIE`, and indicator values by year.
#' @param indicator A string specifying the indicator to compute. Must be one of: `"taux_cyclabilite"`, `"voirie_hab"`, or `"voirie_km"`.
#' @param time_period A character vector of years to include in the comparison (e.g., `c("2019", "2020", "2021")`).
#'
#' @return A data frame with one row per year and commune category (`LIBCAT`), including:
#' - `CAT`: numerical category code
#' - `LIBCAT`: category label (e.g., "Communes rurales")
#' - `year`: year as character
#' - `indicator`: computed aggregated value of the selected indicator
#'
#' @importFrom dplyr select mutate left_join case_when group_by summarise all_of
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @noRd
generate_comparison_data_national <- function(data_cat_terr, amenagement_communal,
                                              indicator, time_period, secure_amenagement = FALSE){

  # If only secure aménagements are considered, only keep pistes cyclables and voies vertes in total
  if (secure_amenagement){
    for (year in time_period) {
      amenagement_communal <- amenagement_communal %>%
        mutate(
          !!paste0("NB_TOTAL_", year) := .data[[paste0("NB_PISTE_", year)]] + .data[[paste0("NB_VV_", year)]]
        )
    }
  }

  # Columns containing the selected indicator
  comparison_cols <- switch(indicator,
                            taux_cyclabilite = c("TAUX_CYCL_"),
                            voirie_hab = c("NB_TOTAL_"),
                            voirie_km = c("NB_TOTAL_"))

  cols_to_select <- paste0(comparison_cols, time_period)

  # Retrieve relevant data and label communes categories
  data_comparison <- data_cat_terr |>
    mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                           CAT_COM %in% c(2, 3, 4) ~ 2,
                           CAT_COM %in% c(5, 6, 7) ~ 3),
           LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                              CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                              CAT_COM %in% c(5, 6, 7) ~ "Communes rurales")) |>
    select(COM, CAT, LIBCAT) |>
    left_join(amenagement_communal |> select(all_of(c("INSEE_COM", "VOIRIE", "POP", "SUPERFICIE", cols_to_select))), by = c("COM" = "INSEE_COM"))

  # Reformat to ease computation
  data_comparison_long <- data_comparison |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    mutate(indicator = ifelse(is.infinite(indicator), NA_real_, indicator))    # To handle communes with Inf values

  # Compute chosen indicator by category
  if (indicator == "taux_cyclabilite"){
    data_comparison_cat <- data_comparison_long |>
      group_by(CAT, LIBCAT, year) |>
      summarise(
        indicator = sum(indicator * VOIRIE, na.rm = TRUE) / sum(VOIRIE, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(year = as.character(year),
             indicator = indicator * 100)
  } else if (indicator %in% c("voirie_hab", "voirie_km")){
    denominator <- if (indicator == "voirie_hab") "POP" else "SUPERFICIE"

    data_comparison_cat <- data_comparison_long |>
      group_by(CAT, LIBCAT, year) |>
      summarise(
        indicator = 1000 * sum(indicator, na.rm = TRUE) / sum(.data[[denominator]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(year = as.character(year))
  }

  return(data_comparison_cat)

}




#' @title Generate Local Comparison Data for Cycling Infrastructure Indicators
#'
#' @description This function computes comparison statistics for a selected cycling infrastructure indicator across
#' all territories belonging to the same category as a selected territory (e.g., same type of EPCI, département, or region).
#' It enables localized benchmarking by calculating the **minimum**, **maximum**, and **weighted average** of the indicator
#' for comparable territories.
#'
#' The function also returns the formatted label of the comparison group (e.g., "communautés d'agglomération",
#' or "régions d'outre-mer") for use in visualization.
#'
#' The indicators supported are:
#' - `"taux_cyclabilite"`: Cycling infrastructure rate (as % of road length)
#' - `"voirie_hab"`: Infrastructure per inhabitant (km/hab)
#' - `"voirie_km"`: Infrastructure per area (km/km²)
#'
#' @param data_cat_terr A data frame describing all territories and their categories (e.g., `COM`, `EPCI`, `DEP`, `REG`, and associated category labels).
#' @param amenagement_data A data frame containing cycling infrastructure data by territory and year (including `POP`, `SUPERFICIE`, `VOIRIE`, and indicator columns).
#' @param typology A string defining the level of analysis. One of `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param indicator A string indicating the indicator to calculate. Must be one of: `"taux_cyclabilite"`, `"voirie_hab"`, or `"voirie_km"`.
#' @param selected_cog A string or code identifying the selected territory (e.g., INSEE code).
#' @param time_period A character vector specifying the years to include (e.g., `c("2019", "2020", "2021")`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`data_comparison_cat`}{A data frame with columns `year`, `LIBCAT` (category: Min, Max, Moy), and `indicator` (computed value)}
#'   \item{`lib_category`}{A character string representing the label of the category to which the selected territory belongs, formatted for display (e.g., "communes rurales")}
#' }
#'
#' @importFrom dplyr select mutate left_join filter pull group_by summarise case_when all_of
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract
#' @noRd
generate_comparison_data_local <- function(data_cat_terr, amenagement_data,
                                           typology, indicator,
                                           selected_cog,
                                           time_period,
                                           secure_amenagement = FALSE){

  # Columns containign the selected indicator
  comparison_cols <- switch(indicator,
                            taux_cyclabilite = c("TAUX_CYCL_"),
                            voirie_hab = c("NB_TOTAL_"),
                            voirie_km = c("NB_TOTAL_"))

  cols_to_select <- paste0(comparison_cols, time_period)

  # If only secure aménagements are considered, only keep pistes cyclables and voies vertes in total
  if (secure_amenagement){
    for (year in time_period) {
      amenagement_data <- amenagement_data %>%
        mutate(
          !!paste0("NB_TOTAL_", year) := .data[[paste0("NB_PISTE_", year)]] + .data[[paste0("NB_VV_", year)]]
        )
    }
  }

  # Retrieve relevant data and filter to only keep territories belonging to the same category
  colname <- switch(typology,
                    commune = c("COM", "LIB_CAT_COM", "INSEE_COM"),
                    epci = c("EPCI", "NATURE_EPCI", "INSEE_EPCI"),
                    departement = c("DEP", "LIB_CAT_DEP", "INSEE_DEP"),
                    region = c("REG", "LIB_CAT_REG", "INSEE_REG"))

  if (typology == "region"){
    data_cat_terr <- data_cat_terr |>
      mutate(LIB_CAT_REG = ifelse(REG %in% c("01", "02", "03", "04", "06"), "régions d'outre-mer", "régions de métropole"))
  }

  lib_category <- data_cat_terr |> filter(.data[[colname[1]]] == selected_cog) |> pull(.data[[colname[2]]]) |> unique()

  data_comparison <- data_cat_terr |>
    filter(.data[[colname[2]]] == lib_category) |>
    select(all_of(colname[1])) |>
    unique() |>
    left_join(amenagement_data |> select(all_of(c(colname[3], "VOIRIE", "POP", "SUPERFICIE", cols_to_select))),
              by = setNames(colname[3], colname[1]))

  # Reformat to ease computation
  data_comparison_long <- data_comparison |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    mutate(indicator = ifelse(is.infinite(indicator), NA_real_, indicator))    # To handle communes with Inf values

  # Compute chosen indicator by category
  if (indicator == "taux_cyclabilite"){
    data_comparison_cat <- data_comparison_long |>
      mutate(
        voirie_cyclable = indicator * VOIRIE
      ) |>
      group_by(year) |>
      summarise(
        Moy = sum(voirie_cyclable, na.rm = TRUE) / sum(VOIRIE, na.rm = TRUE),
        Min = min(indicator, na.rm = TRUE),
        Max = max(indicator, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = c("Moy", "Min", "Max"),
        names_to = "LIBCAT",
        values_to = "indicator"
      ) |>
      mutate(
        year = as.character(year),
        indicator = round(indicator * 100, 1)
      )
  } else if (indicator %in% c("voirie_hab", "voirie_km")){
    denominator <- if (indicator == "voirie_hab") "POP" else "SUPERFICIE"

    data_comparison_cat <- data_comparison_long |>
      mutate(indicator_ratio = 1000 * indicator / .data[[denominator]]) |>
      group_by(year) |>
      summarise(
        Moy = sum(1000 * indicator, na.rm = TRUE) / sum(.data[[denominator]], na.rm = TRUE),
        Min = min(indicator_ratio, na.rm = TRUE),
        Max = max(indicator_ratio, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(
        cols = c("Moy", "Min", "Max"),
        names_to = "LIBCAT",
        values_to = "indicator"
      ) |>
      mutate(
        year = as.character(year)
      )
  }

  lib_category <- transform_category(lib_category)

  return(list(data_comparison_cat = data_comparison_cat, lib_category = lib_category))
}



#' @title Plot the Evolution of a Cycling Infrastructure Indicator with Comparison
#'
#' @description This function generates an interactive `plotly` bar and line chart showing the temporal evolution
#' of a cycling infrastructure indicator (e.g., cycling rate, infrastructure per capita or per km²)
#' for a selected territory. It also overlays comparative reference lines from other territories in
#' the same category (e.g., similar communes, EPCI, or regions).
#'
#' @param data A data frame containing annual values of the indicator for the selected territory.
#' @param data_comparison_cat A data frame containing indicator values for the comparison categories (e.g., min, max, average of similar territories).
#' @param indicator A string specifying the indicator to be plotted. Must be one of: `"taux_cyclabilite"`, `"voirie_hab"`, or `"voirie_km"`.
#' @param selected_territory A character string naming the territory of interest (e.g., a commune, EPCI, département, or région).
#' @param time_period A character vector of the years (e.g., `"2019":"2021"`) to include in the time series.
#' @param typology A string indicating the comparative typology. Typically `"france"` for national categories (e.g., urban/rural), or a grouping like `"region"`, `"epci"`, etc.
#' @param lib_category A label identifying the category or group the selected territory belongs to (e.g., "Bourgs ruraux") for display purposes.
#'
#' @return A `plotly` interactive chart combining:
#' - A bar chart showing the indicator value over time for the selected territory.
#' - One or more line plots representing comparison benchmarks (e.g., min, max, average).
#' - Customized tooltips with units, evolution rates, and contextual information.
#'
#' @importFrom dplyr select mutate left_join filter pull case_when all_of
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove str_to_lower
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point scale_color_manual scale_fill_manual labs theme_minimal theme element_blank element_text margin
#' @importFrom plotly ggplotly layout style
#' @noRd
plot_evolution_amenagement <- function(data, data_comparison_cat,
                                       indicator, selected_territory,
                                       time_period,
                                       typology, lib_category, secure_amenagement = FALSE){

  root <- switch(indicator,
                 taux_cyclabilite = c("TAUX_CYCL_"),
                 voirie_hab = c("NB_TOTAL_"),
                 voirie_km = c("NB_TOTAL_"))

  indicator_title <- switch(indicator,
                            taux_cyclabilite = "Taux de cyclabilité de la voirie",
                            voirie_hab = "Linéaire de voirie cyclable par habitant",
                            voirie_km = "Linéaire de voirie cyclable par km²")

  n_decimals <- switch(indicator,
                       taux_cyclabilite = 1,
                       voirie_hab = 2,
                       voirie_km = 0)

  cols_to_display <- paste0(root, time_period)


  # If only secure aménagements are considered, only keep pistes cyclables and voies vertes in total
  if (secure_amenagement){
    for (year in time_period) {
      data <- data %>%
        mutate(
          !!paste0("NB_TOTAL_", year) := .data[[paste0("NB_PISTE_", year)]] + .data[[paste0("NB_VV_", year)]]
        )
    }
  }

  # Compute indicator

  data_to_display <- data |>
    select(all_of(c("SUPERFICIE", "POP", cols_to_display))) |>
    pivot_longer(
      cols = all_of(cols_to_display),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = str_remove(year, root),
      value = case_when(
        indicator == "taux_cyclabilite" ~ value * 100,
        indicator == "voirie_hab" ~ 1000 * value / POP,
        indicator == "voirie_km" ~ 1000 * value / SUPERFICIE,
        TRUE ~ NA_real_
      )
    ) |>
    select(-POP, -SUPERFICIE)


  ## Custom tooltip for main data, with evolution

  # Units to display
  unit <- switch(indicator,
                 taux_cyclabilite = "%",
                 voirie_hab = " m/hab",
                 voirie_km = " m/km²")

  # Create a year_numeric column
  data_to_display <- data_to_display |>
    mutate(year_numeric = as.numeric(year))

  # Create an offset dataframe to join
  previous_values <- data_to_display |>
    mutate(year_numeric = year_numeric + 1) |>  # Shift the year by +1 for direct join
    select(year_numeric, value_previous = value)

  # Join over the previous year
  data_to_display <- data_to_display |>
    left_join(previous_values, by = "year_numeric") |>
    mutate(
      evolution_rate = case_when(
        indicator == "taux_cyclabilite" ~ value - value_previous,
        TRUE ~ (value - value_previous) / value_previous * 100
      ),
      tooltip = ifelse(year == "2021",
                       paste0(indicator_title,
                              " en ", year, " : <span style='font-weight:bold'>",
                              format(round(value, n_decimals), decimal.mark = ",", big.mark = " "), unit, "</span>"),
                       paste0(indicator_title,
                              " en ", year, " : <span style='font-weight:bold'>",
                              format(round(value, n_decimals), decimal.mark = ",", big.mark = " "), unit, "</span>\n",
                              "Évolution par rapport à ", as.character(as.numeric(year) - 1), " : <span style='font-weight:bold'>",
                              ifelse(!is.finite(evolution_rate),
                                     "-",
                                     paste0(ifelse(evolution_rate >= 0, "+", ""), format(round(evolution_rate, 1), decimal.mark = ",", trim = TRUE),
                                            switch(indicator,
                                                   taux_cyclabilite = ifelse(abs(evolution_rate) < 2, " point", " points"),
                                                   "%"))), "</span>")

      )
    ) |>
    mutate(territoire = selected_territory)

  # Custom tooltip and colors for comparison data
  if (typology == "france"){
    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(indicator_title,
                         " dans les\n", str_to_lower(LIBCAT),
                         " en ", year, " : <span style='font-weight:bold'>",
                         format(round(indicator, n_decimals), decimal.mark = ",", big.mark = " "), unit, "</span>")
      )

    custom_colors <- c(
      "Grands centres urbains" = "#E94F35",
      "Communes intermédiaires" = "#9185BE",
      "Communes rurales" = "#C6CE41"
    )
  } else {
    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(case_when(LIBCAT == "Moy" ~ "Moyenne pondérée du ",
                                   LIBCAT == "Min" ~ "Minimum du ",
                                   LIBCAT == "Max" ~ "Maximum du ",
                                   TRUE ~ ""),
                         str_to_lower(indicator_title), "\ndans les ", str_to_lower(lib_category),
                         " en ", year, " : <span style='font-weight:bold'>",
                         format(round(indicator, n_decimals), decimal.mark = ",", big.mark = " "), unit, "</span>")
      ) |>
      mutate(LIBCAT = ifelse(LIBCAT == "Moy", paste0("Moyenne pondérée des ", str_to_lower(lib_category)), LIBCAT),
             LIBCAT = ifelse(LIBCAT == "Min", paste0("Minimum des ", str_to_lower(lib_category)), LIBCAT),
             LIBCAT = ifelse(LIBCAT == "Max", paste0("Maximum des ", str_to_lower(lib_category)), LIBCAT)
      )

    custom_colors <- setNames(
      c("#E94F35", "#9185BE", "#C6CE41"),
      c(paste0("Maximum des ", str_to_lower(lib_category)),
        paste0("Minimum des ", str_to_lower(lib_category)),
        paste0("Moyenne pondérée des ", str_to_lower(lib_category)))
    )
  }

  ## Custom y label
  y_label <- paste0(indicator_title, " (", gsub(" ", "", unit), ")")

  ## ggplot chart
  p <- ggplot(data_to_display, aes(x = year, y = value, text = tooltip)) +
    geom_bar(data = data_to_display,
             aes(fill = territoire),
             stat = "identity", width = 0.4) +
    geom_line(data = data_comparison_cat,
              aes(x = year, y = indicator, group = LIBCAT, color = LIBCAT, text = tooltip),
              inherit.aes = FALSE,
              linewidth = 1) +
    geom_point(data = data_comparison_cat,
               aes(x = year, y = indicator, group = LIBCAT, color = LIBCAT, text = tooltip),
               inherit.aes = FALSE,
               size = 2.5) +
    scale_color_manual(values = custom_colors, name = NULL) +
    scale_fill_manual(
      values = setNames("#294754", selected_territory),
      name = NULL) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = " ")) +
    labs(
      title = "",
      x = "",
      y = y_label
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

  ## Return plotly chart

  # Clean territory_name for title
  selected_territory_clean <- selected_territory

  if (grepl("Communauté de communes", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté de communes", "CC", selected_territory_clean)
  } else if (grepl("Communauté urbaine", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté urbaine", "CU", selected_territory_clean)
  } else if (grepl("Communauté d'agglomération", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté d'agglomération", "CA", selected_territory_clean)
  }

  line_categories <- unique(data_comparison_cat$LIBCAT)

  # Plotly chart with custom title
  g <- ggplotly(p, tooltip = "text") |>
    plotly::layout(
      title = list(
        text = paste0(
          selected_territory_clean,
          "<span style='font-size:80%; font-weight:normal;'>",
          " - ",
          ifelse(secure_amenagement,
                 switch(indicator,
                        voirie_hab = "Linéaire de voirie cyclable sécurisée par habitant",
                        voirie_km = "Linéaire de voirie cyclable sécurisée par km²"),
                 indicator_title),
          "</span>"
        ),
        x = 0.06,
        y = 1.1,
        font = list(size = 20, color = "#294754")
      ),
      legend = list(title = list(text = ""), font = list(size = if (typology %in% c("france", "commune")) 15 else 13))
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png",  filename = "plot_export")
    )

  if (typology == "france") {
    g <- g %>%
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 4, showlegend = TRUE) %>%
      style(name = line_categories[2], traces = 2, showlegend = TRUE) %>%
      style(name = line_categories[3], traces = 3, showlegend = TRUE)
  } else {
    g <- g %>%
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[3], traces = 2, showlegend = TRUE) %>%
      style(name = line_categories[2], traces = 3, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 4, showlegend = TRUE)
  }

  return(g)
}



#' @title Plot Average Cycleability Rate by Type of Commune
#'
#' @description Generates an interactive Plotly bar chart showing the average
#' `taux de cyclabilité` (weighted by total road length) for different types of communes.
#' The chart is color-coded by commune milieu (rural, intermediate, urban) and includes
#' tooltips with precise values.
#'
#' @param amenagement_communal A data frame containing cycling infrastructure data by territory and year (including `VOIRIE`, and `TAUX_CYCL_` columns).
#' @param data_cat_terr A data frame describing all territories and their categories (e.g., `COM`, `EPCI`, `DEP`, `REG`, and associated category labels).
#' @param current_year Integer. Latest year with data. The function will dynamically select `TAUX_CYCL_<current_year>` for computation.
#' @param typology Character. Typology selected by the user
#'
#' @return A `plotly` object representing a horizontal bar chart with:
#'   - Bars representing weighted average cycling rates per commune type.
#'   - Color fill according to commune milieu.
#'   - Text labels showing the cycling rate in percent.
#'   - Tooltips displaying detailed cycling rates.
#'
#'
#' @importFrom dplyr select left_join group_by summarise rename mutate
#' @importFrom dplyr %>%
#' @importFrom rlang sym !!
#' @importFrom stringr str_to_lower
#' @import ggplot2
#' @import plotly
#' @noRd
plot_cyclabilite_by_commune_type <- function(amenagement_communal, data_cat_terr, current_year, typology){

  # Compute weighted cycleability rate and format data
  col_tc <- paste0("TAUX_CYCL_", current_year)

  data <- amenagement_communal |>
    select(INSEE_COM, VOIRIE, !!sym(col_tc)) |>
    inner_join(data_cat_terr |> select(COM, LIB_CAT_COM), by = c("INSEE_COM" = "COM")) |>
    group_by(LIB_CAT_COM) |>
    summarise(taux_cyclabilite = 100 * sum(!!sym(col_tc) * VOIRIE, na.rm = TRUE) / sum(VOIRIE, na.rm = TRUE)) |>
    rename(type_commune = LIB_CAT_COM) |>
    mutate(type_commune = ifelse(type_commune == "Rural à habitat très dispersé", "Communes rurales à habitat très dispersé", type_commune),
           type_commune = ifelse(type_commune == "Rural à habitat dispersé", "Communes rurales à habitat dispersé", type_commune),
           milieu = case_when(type_commune == "Grands centres urbains" ~ "Grands centres urbains",
                              type_commune %in% c("Centres urbains intermédiaires", "Petites villes", "Ceintures urbaines") ~ "Communes intermédiaires",
                              type_commune %in% c("Bourgs ruraux", "Communes rurales à habitat dispersé", "Communes rurales à habitat très dispersé") ~ "Communes rurales"),
           milieu = factor(milieu),
           type_commune = factor(type_commune, levels = rev(c("Communes rurales à habitat très dispersé", "Communes rurales à habitat dispersé", "Bourgs ruraux",
                                                              "Ceintures urbaines", "Petites villes", "Centres urbains intermédiaires", "Grands centres urbains")))
    ) |>
    mutate(tooltip = paste0("Taux de cyclabilité moyen dans\nles ", str_to_lower(type_commune), " : <span style='font-weight:bold'>",
                            format(round(taux_cyclabilite, 1), decimal.mark = ",", nsmall = 1), "%</span>"))

  # Create color palette
  custom_colors <- c(
    "Grands centres urbains" = "#E94F35",
    "Communes intermédiaires" = "#9185BE",
    "Communes rurales" = "#c6ce41"
  )

  # Compute max cycleability rate for label positioning
  max_rate <- max(data$taux_cyclabilite)

  # ggplot chart
  p <- ggplot(data, aes(x = type_commune, y = taux_cyclabilite, fill = milieu, text = tooltip)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = paste0(format(round(taux_cyclabilite, 1), decimal.mark = ",", nsmall = 1), "%")),
      nudge_y = 0.06 * max_rate,
      size = 3
    ) +
    coord_flip() +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(labels = label_number(decimal.mark = ",")) +
    labs(
      title = "",
      x = "",
      y = "Taux moyen de cyclabilité de la voirie (%)",
      fill = ""
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.margin = margin(t = 12, b = 7, l = 20, r = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = "#294754"),
      axis.title.y = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )

  # Return plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    plotly::layout(
      legend = list(font = list(size = if (typology == "departement") 13 else 15))
    ) |>
    config(displayModeBar = FALSE)

  return(g)

}



#' @title Plot Annual Kilometers of Cycling Infrastructure by Type
#'
#' @description This function computes the number of kilometers of different types
#' of cycling infrastructure created each year (Voie verte, Piste cyclable, Bande cyclable,
#' and Other types) based on cumulative data, and visualizes them in a stacked
#' bar chart. The output is an interactive `plotly` chart.
#'
#' @param data_amenagement A data frame containing cumulative annual values of
#' cycling infrastructure. It must include:
#' \itemize{
#'   \item Columns starting with \code{NB_VV_YYYY}, \code{NB_PISTE_YYYY},
#'   \code{NB_BANDE_YYYY} for cumulative kilometers of each type, where
#'   \code{YYYY} is the year.
#'   \item Columns starting with \code{NB_TOTAL_YYYY} for the cumulative total
#'   kilometers of all infrastructures combined.
#' }
#'
#' @return A `plotly` object representing the interactive stacked bar chart.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @importFrom scales label_number
#' @noRd
plot_km_history <- function(data_amenagement){

  # Compute km created each year for Voie verte, Piste cyclable and Bande cyclable
  df_long <- data_amenagement %>%
    pivot_longer(
      cols = starts_with("NB_VV_") | starts_with("NB_PISTE_") | starts_with("NB_BANDE_"),
      names_to = c("category", "year"),
      names_pattern = "NB_(VV|PISTE|BANDE)_(\\d{4})",
      values_to = "km_cumul"
    ) %>%
    mutate(
      category = case_when(
        category == "VV" ~ "Voie verte",
        category == "PISTE" ~ "Piste cyclable",
        category == "BANDE" ~ "Bande cyclable"
      ),
      year = as.integer(year)
    ) %>%
    group_by(category) %>%
    mutate(
      km = km_cumul - dplyr::lag(km_cumul)
    ) %>%
    ungroup() %>%
    filter(!is.na(km)) %>%
    select(year, category, km)

  # Compute "Other" : total created each year - VV - PISTE - BANDE
  df_total <- data_amenagement %>%
    pivot_longer(
      cols = starts_with("NB_TOTAL_"),
      names_to = "year",
      names_pattern = "NB_TOTAL_(\\d{4})",
      values_to = "total_cumul"
    ) %>%
    mutate(year = as.integer(year)) %>%
    arrange(year) %>%
    mutate(
      total_annuel = total_cumul - dplyr::lag(total_cumul)
    ) %>%
    filter(!is.na(total_annuel))

  df_other <- df_total %>%
    left_join(
      df_long %>%
        group_by(year) %>%
        summarise(used_km = sum(km, na.rm = TRUE)),
      by = "year"
    ) %>%
    mutate(
      km = total_annuel - used_km,
      category = "Autre"
    ) %>%
    select(year, category, km)

  # Combine every categories
  df_final <- bind_rows(df_long, df_other) %>%
    arrange(year, category)

  # Format data for display
  realisation_long <- df_final %>%
    mutate(
      category = factor(category, levels = c("Autre", "Bande cyclable", "Voie verte", "Piste cyclable")),
      year = factor(year),
      tooltip = case_when(
        category == "Autre" ~ paste0(
          "Linéaire d'autres types d'aménagement créé en ", year, " : <span style='font-weight:bold'>",
          format(round(km, 1), big.mark = " ", decimal.mark = ","), " km</span>"
        ),
        category == "Voie verte" ~ paste0(
          "Linéaire de voies vertes créé en ", year, " : <span style='font-weight:bold'>",
          format(round(km, 1), big.mark = " ", decimal.mark = ","), " km</span>"
        ),
        category == "Piste cyclable" ~ paste0(
          "Linéaire de pistes cyclables créé en ", year, " : <span style='font-weight:bold'>",
          format(round(km, 1), big.mark = " ", decimal.mark = ","), " km</span>"
        ),
        category == "Bande cyclable" ~ paste0(
          "Linéaire de bandes cyclables créé en ", year, " : <span style='font-weight:bold'>",
          format(round(km, 1), big.mark = " ", decimal.mark = ","), " km</span>"
        )
      )
    ) %>%
    mutate(km = ifelse(km < 0, 0, km))

  # Height threshold for bar labels display
  df_threshold <- realisation_long %>%
    group_by(year) %>%
    mutate(sum_km = sum(km, na.rm = TRUE))

  seuil <- max(df_threshold$sum_km) * 0.029

  # Custom color palette
  custom_colors <- c(
    "Voie verte" = "#B1D6E4",
    "Piste cyclable" = "#3b5461",
    "Bande cyclable" = "#C6CE41",
    "Autre" = "#ec6446"
  )

  # ggplot chart
  p <- ggplot(realisation_long, aes(x = year, y = km, fill = category, text = tooltip)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(
      data = filter(realisation_long, km > seuil),
      aes(label = paste0(format(round(km, 0), big.mark = " "), " km")),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(labels = label_number(accuracy = 1, decimal.mark = ",")) +
    labs(
      title = "",
      x = "",
      y = "Kilomètres réalisés",
      fill = ""
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal(base_size = 15, base_family = "Poppins")  +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.margin = margin(t = 0, b = 0, l = 10, r = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = "#294754"),
      axis.title.y = element_text(size = 12, margin = margin(r = 50)),
      axis.title.x = element_text(size = 12),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )

  # Return plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.07,
        traceorder = "reversed",
        font = list(size = 15, color = "#294754")
      )
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png",  filename = "plot_export")
    )

  return(g)
}



#' @title Generate Yearly Evolution Rates of Cycling Infrastructure, at Local Level
#'
#' @description For a user-selected territory, specified by its typology (`typology`),
#' INSEE code (`selected_cog`), and name (`selected_territory`), this function calculates
#' the annual evolution rate of cycling infrastructure or a specific type of infrastructure
#' (`selected_amenagement`). It also computes the evolution rate for the entire category
#' to which the territory belongs.
#'
#' @param data_cat_terr A `data.frame` containing information on territories and their categories.
#'   Must include columns corresponding to the territory codes, category labels, and INSEE codes.
#' @param amenagement_data A `data.frame` containing the infrastructure km counts. Columns should
#'   include the territory INSEE code and yearly counts of the selected infrastructure type(s).
#' @param typology Character. One of `"commune"`, `"epci"`, `"departement"`, `"region"`.
#'   Determines the territorial level of analysis.
#' @param selected_cog Character. The INSEE code of the selected territory.
#' @param selected_territory Character. Name of the selected territory.
#' @param time_period Character. Suffix for the columns in `amenagement_data` corresponding to the years of interest.
#' @param selected_amenagement Character. The short code of the type of cycling infrastructure to analyze.
#'   Use `"TOTAL"` to compute for all types.
#' @param amenagement_types A `data.frame` with two columns `short` and `long`, providing
#'   a mapping from short codes to full descriptions of infrastructure types.
#'
#' @return A `list` containing:
#'   \describe{
#'     \item{data_to_display}{A `data.frame` combining evolution rates for the selected territory and its category.
#'           Columns include `year`, `indicator`, `evolution`, `category`, and `tooltip`.}
#'     \item{lib_category}{A character string with the label of the category to which the territory belongs.}
#'   }
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_to_lower str_to_upper str_sub str_c
#' @noRd
generate_evolution_rate_local <- function(data_cat_terr, amenagement_data,
                                          typology, selected_cog, selected_territory,
                                          time_period, selected_amenagement, amenagement_types){

  # Columns containign the data for the selected type of aménagement
  cols_to_select <- paste0("NB_", selected_amenagement, "_", time_period)

  # Retrieve relevant data and filter to only keep territories belonging to the same category
  colname <- switch(typology,
                    commune = c("COM", "LIB_CAT_COM", "INSEE_COM"),
                    epci = c("EPCI", "NATURE_EPCI", "INSEE_EPCI"),
                    departement = c("DEP", "LIB_CAT_DEP", "INSEE_DEP"),
                    region = c("REG", "LIB_CAT_REG", "INSEE_REG"))

  if (typology == "region"){
    data_cat_terr <- data_cat_terr |>
      mutate(LIB_CAT_REG = ifelse(REG %in% c("01", "02", "03", "04", "06"), "Régions d'outre-mer", "Régions de métropole"))
  }

  # Get category label
  lib_category <- data_cat_terr |> filter(.data[[colname[1]]] == selected_cog) |> pull(.data[[colname[2]]]) |> unique()

  # Get aménagement data corresponding to the category of the selected territory
  data_category <- data_cat_terr |>
    filter(.data[[colname[2]]] == lib_category) |>
    select(all_of(colname[1])) |>
    unique() |>
    left_join(amenagement_data |> select(all_of(c(colname[3], cols_to_select))),
              by = setNames(colname[3], colname[1]))

  lib_category <- str_c(str_to_upper(str_sub(transform_category(lib_category), 1, 1)), str_sub(transform_category(lib_category), 2))

  # Compute evolution rate for whole category
  data_category_long <- data_category |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    group_by(year) |>
    summarise(indicator = sum(indicator), .groups = "drop") |>
    arrange(year) |>
    mutate(
      evolution = 100 * (indicator - dplyr::lag(indicator)) / dplyr::lag(indicator)
    ) |>
    filter(!is.na(evolution)) |>
    mutate(category = lib_category) |>
    mutate(tooltip = paste0("Taux d'évolution global du linéaire ",
                            ifelse(selected_amenagement == "TOTAL",
                                   "d'aménagements cyclables",
                                   paste0("de ", str_to_lower(amenagement_types |> filter(short == selected_amenagement) |> pull(long)))),
                            " dans les\n", str_to_lower(lib_category), " en ", year, " par rapport à ", year-1, " : <span style='font-weight:bold'>",
                            format(round(evolution, 2), decimal.mark = ","), "%</span>"))


  # Compute evolution rate for territory
  data_territory_long <- data_category |>
    filter(.data[[colname[1]]] == selected_cog) |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    select(-all_of(colname[1])) |>
    arrange(year) |>
    mutate(evolution = 100 * ((indicator / dplyr::lag(indicator)) - 1)) |>
    filter(!is.na(evolution)) |>
    mutate(category = selected_territory) |>
    mutate(tooltip = paste0("Taux d'évolution du linéaire ",
                            ifelse(selected_amenagement == "TOTAL",
                                   "d'aménagements cyclables",
                                   paste0("de ", str_to_lower(amenagement_types |> filter(short == selected_amenagement) |> pull(long)))),
                            " dans le territoire\n", selected_territory, " en ", year, " par rapport à ", year-1, " : <span style='font-weight:bold'>",
                            format(round(evolution, 2), decimal.mark = ","), "%</span>"))

  data_to_display <- rbind(data_category_long, data_territory_long)

  return(list(data_to_display = data_to_display, lib_category = lib_category))
}


#' @title Generate Yearly Evolution Rates of Cycling Infrastructure at National Level
#'
#' @description For a user-selected category of municipalities (`selected_milieu`) and
#' at the national level, this function calculates the annual evolution rate of cycling
#' infrastructure or a specific type of infrastructure (`selected_amenagement`).
#' It computes evolution rates for both the selected milieu and for the whole country.
#'
#' @param data_cat_terr A `data.frame` containing information on all communes and their categories (`CAT_COM`).
#' @param amenagement_communal A `data.frame` containing the counts of infrastructure per commune,
#'   with columns for INSEE code and yearly counts of the selected infrastructure type(s).
#' @param amenagement_national A `data.frame` containing national totals for the selected infrastructure type(s).
#' @param selected_territory Character. Name of the national territory (e.g., `"France entière"`).
#' @param selected_milieu Character. One of `"Grands centres urbains"`, `"Communes intermédiaires"`, `"Communes rurales"`.
#' @param time_period Character. Suffix for the columns in `amenagement_communal` / `amenagement_national` corresponding to the years of interest.
#' @param selected_amenagement Character. The short code of the type of cycling infrastructure to analyze. Use `"TOTAL"` to compute for all types.
#' @param amenagement_types A `data.frame` with two columns `short` and `long`, providing
#'   a mapping from short codes to full descriptions of infrastructure types.
#'
#' @return A `list` containing:
#'   \describe{
#'     \item{data_to_display}{A `data.frame` combining evolution rates for the selected milieu and for the national level.
#'           Columns include `year`, `indicator`, `evolution`, `category`, and `tooltip`.}
#'     \item{lib_category}{A character string with the label of the selected milieu.}
#'   }
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_extract str_to_lower
#' @noRd
generate_evolution_rate_national <- function(data_cat_terr, amenagement_communal, amenagement_national,
                                             selected_milieu,
                                             time_period, selected_amenagement, amenagement_types){

  # Columns containign the data for the selected type of aménagement
  cols_to_select <- paste0("NB_", selected_amenagement, "_", time_period)

  # Convert commune categories into grands centres urbains, intermédiaires and rurales
  milieu <- switch(selected_milieu,
                   "Grands centres urbains" = c(1),
                   "Communes intermédiaires" = c(2, 3, 4),
                   "Communes rurales" = c(5, 6, 7))

  # Get aménagement data corresponding to the communes of the selected milieu
  data_category <- data_cat_terr |>
    filter(CAT_COM %in% milieu) |>
    select(COM) |>
    unique() |>
    left_join(amenagement_communal |> select(all_of(c("INSEE_COM", cols_to_select))),
              by = c("COM" = "INSEE_COM"))

  # Compute evolution rate for whole milieu
  data_category_long <- data_category |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    group_by(year) |>
    summarise(indicator = sum(indicator), .groups = "drop") |>
    arrange(year) |>
    mutate(
      evolution = 100 * (indicator - dplyr::lag(indicator)) / dplyr::lag(indicator)
    ) |>
    filter(!is.na(evolution)) |>
    mutate(category = selected_milieu) |>
    mutate(tooltip = paste0("Taux d'évolution global du linéaire ",
                            ifelse(selected_amenagement == "TOTAL",
                                   "d'aménagements cyclables",
                                   paste0("de ", str_to_lower(amenagement_types |> filter(short == selected_amenagement) |> pull(long)))),
                            " dans les\n", str_to_lower(selected_milieu), " en ", year, " par rapport à ", year-1, " : <span style='font-weight:bold'>",
                            format(round(evolution, 2), decimal.mark = ","), "%</span>"))


  # Compute evolution rate for France
  data_territory_long <- amenagement_national |>
    select(all_of(cols_to_select)) |>
    pivot_longer(
      cols = all_of(cols_to_select),
      names_to = "year",
      values_to = "indicator"
    ) |>
    mutate(
      year = str_extract(year, "\\d+"),
      year = as.integer(year)
    ) |>
    arrange(year) |>
    mutate(evolution = 100 * ((indicator / dplyr::lag(indicator)) - 1)) |>
    filter(!is.na(evolution)) |>
    mutate(category = "France entière") |>
    mutate(tooltip = paste0("Taux d'évolution du linéaire ",
                            ifelse(selected_amenagement == "TOTAL",
                                   "d'aménagements cyclables",
                                   paste0("de ", str_to_lower(amenagement_types |> filter(short == selected_amenagement) |> pull(long)))),
                            " en France en ", year, "\npar rapport à ", year-1, " : <span style='font-weight:bold'>",
                            format(round(evolution, 2), decimal.mark = ","), "%</span>"))

  data_to_display <- rbind(data_category_long, data_territory_long)

  return(list(data_to_display = data_to_display, lib_category = selected_milieu))

}


#' @title Plot Annual Evolution Rates of Cycling Infrastructure
#'
#' @description This function creates an interactive Plotly bar chart showing
#' the annual evolution rates of cycling infrastructure for a selected territory
#' and its associated category.
#' Labels above bars indicate the yearly evolution percentages, and tooltips provide detailed information.
#'
#' @param data_to_display A `data.frame` containing evolution rates for both the selected territory and its category.
#'   Must include columns `year`, `evolution`, `category`, and `tooltip`.
#' @param lib_category Character. Name of the category associated with the selected territory.
#' @param selected_territory Character. Name of the selected territory.
#'
#' @return A `plotly` object representing an interactive bar chart with:
#' \itemize{
#'   \item Bars for the selected territory and its category, colored differently.
#'   \item Evolution values labeled above the bars.
#'   \item Tooltips displaying detailed evolution information.
#'   \item Year labels formatted as "year / previous year" with the previous year in smaller font.
#' }
#'
#' @import ggplot2
#' @import plotly
#' @import dplyr
#' @importFrom stringr str_sub str_to_upper
#' @noRd
plot_annual_evolution_rate <- function(data_to_display, lib_category, selected_territory){

  # Set position for bar labels
  y_range <- max(data_to_display$evolution, na.rm = TRUE) - min(data_to_display$evolution, na.rm = TRUE)

  data_to_display <- data_to_display |>
    mutate(
      evolution_label = evolution + ifelse(evolution >= 0, 0.03 * y_range, -0.04 * y_range)
    )|>
    arrange(year) |>
    mutate(year_label = paste0(year, "<span style='font-size:80%'> /", year - 1, "</span>")) |>
    mutate(
      category = factor(
        category,
        levels = c(selected_territory, str_c(str_to_upper(str_sub(lib_category, 1, 1)), str_sub(lib_category, 2)))
      )
    )

  # Define color palette
  custom_colors <- setNames(
    c("#3b5461", "#C6CE41"),
    levels(data_to_display$category)
  )

  # ggplot chart
  p <- ggplot(data_to_display, aes(x = year, y = evolution, fill = category, text = tooltip)) +
    geom_hline(yintercept = 0, color = "#F5F5F5", size = 0.3) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(
      aes(y = evolution_label, label = paste0(ifelse(evolution >= 0, "+", ""), format(round(evolution, 1), decimal.mark = ","), "%")),
      position = position_dodge(width = 0.7), size = 3
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = " ")) +
    labs(
      title = "",
      x = "",
      y = "Taux d'évolution annuel (%)",
      fill = ""
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.margin = margin(t = 0, b = 0, l = 10, r = 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = "#294754"),
      axis.title.y = element_text(size = 12, margin = margin(r = 40)),
      axis.title.x = element_text(size = 12),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )

  # Return plotly chart
  g <- ggplotly(p, tooltip = "text") %>%
    layout(
      xaxis = list(
        tickmode = "array",
        tickvals = data_to_display$year,
        ticktext = data_to_display$year_label
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.085,
        traceorder = "normal",
        font = list(size = 15, color = "#294754")
      )
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png",  filename = "plot_export")
    )

  return(g)
}



#' @title Plot a Treemap of Cycling Infrastructure
#'
#' @description This function constructs a treemap visualization showing the total length of cycling infrastructure
#' in a given territory, broken down by type of infrastructure. The size of each rectangle corresponds to the total kilometers.
#' Small cells (<5% of the total) display only the type, while larger cells show type, km, and percentage.
#' Tooltips provide detailed information with formatted text.
#'
#' @param data_amenagement A `data.frame` containing the cycling infrastructure data, with columns
#'   for each type of infrastructure and year, following the naming convention "NB_<type>_<year>".
#' @param current_year A numeric or character value indicating the year for which the data should be plotted.
#' @param amenagement_types A `data.frame` linking the short names of infrastructure types
#'   (in `short`) to their long descriptive names (in `long`).
#'
#' @return A `plotly` object representing the treemap of cycling infrastructure.
#'
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import plotly
#' @noRd
plot_treemap_amenagement <- function(data_amenagement,
                                     current_year,
                                     amenagement_types){

  # Select columns with cycling infrastructure km data for the year of interest
  cols_year <- names(data_amenagement) %>%
    keep(~ str_detect(.x, paste0("NB_.*_", current_year)) & !str_detect(.x, "NB_TOTAL"))

  # Custom color palette
  color_palette <- c("#294754", "#B1D6E4", "#C6CE41", "#E94F35", "#F28A6C", "#DCEB91", "#bfc8cc")
  names(color_palette) <- c("Piste cyclable", "Voie verte", "Bande cyclable", "Autres", "CVCB", "Double sens cyclable", "Couloir bus+vélo")


  # Prepare dataset for display
  df_plot <- data_amenagement %>%
    select(all_of(cols_year)) %>%
    tidyr::pivot_longer(everything(),
                        names_to = "type",
                        values_to = "km") %>%
    mutate(

      # Get clean label for aménagement type
      type = str_remove(type, paste0("_", current_year)),
      type = str_remove(type, "^NB_"),
      type = amenagement_types$long[match(type, amenagement_types$short)],

      # Format label for aménagement type for display
      type_label = case_when(
        type == "Double sens cyclable" ~ "<b>Double sens<br>cyclable</b>",
        type == "Couloir bus+vélo" ~ "<b>Couloir<br>bus+vélo</b>",
        TRUE ~ paste0("<b>", type, "</b>")
      ),

      # Compute percentage for each type
      km = km,
      pct = km / sum(km) * 100,

      # Format cell text depending on the size of the cell
      label_dynamic = ifelse(
        pct < 5,
        type_label,
        paste0(
          type_label, "<br>",
          format(round(km,0), big.mark = " "), " km<br>",
          format(round(pct,1), decimal.mark = ","), "%"
        )
      ),

      # Formatted detailled tooltip
      tooltip = paste0(
        "<b>", type, "</b> : ",
        format(round(km,0), big.mark = " "), " km, soit ",
        format(round(pct,1), decimal.mark = ","), "% du\nlinéaire total d'aménagement cyclable"
      ),

      # Color
      color = color_palette[type]
    )

  # Treemap
  g <- plot_ly(
    data = df_plot,
    type = "treemap",
    labels = ~label_dynamic,
    parents = NA,
    values = ~km,
    textinfo = "label",
    textposition = "middle center",
    text = ~tooltip,
    hovertemplate = "%{text}<extra></extra>",
    marker = list(colors = ~color),
    insidetextfont = list(
      size = 14
    )
  ) |>
    layout(
      font = list(family = "Poppins")
    ) |>
    config(displaylogo = FALSE)

  return(g)
}








