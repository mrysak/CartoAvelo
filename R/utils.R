#' @title List of available typology levels
#'
#' @description This function returns a character vector of internal typology levels
#' used throughout the application to represent territorial levels.
#'
#' @return Character vector of typology codes: "france", "region", "departement", "epci", "commune".
#'
#' @noRd
typologies <- function(){
  c("france", "region", "departement", "epci", "commune")
}



#' @title Get the infos of the territory selected by the user
#'
#' @description This function returns the INSEE code, the name and the category
#' of the territory selected by the user
#'
#' @param data_cat_terr Data frame containing the territorial data, including INSEE codes and names.
#' @param validated_selection A `reactiveValues` object containing the territory
#' selected by the user.
#'
#' @return List of infos : cog, names, cat and libcat
#' @import dplyr
#'
#' @noRd
get_territory_infos <- function(validated_selection, data_cat_terr){

  if (validated_selection$last_validated_typology == "france"){
    cog <- NULL
    name <- "France entière"
    cat <- NULL
    libcat <- NULL
  }

  else if (validated_selection$last_validated_typology == "region"){
    cog <- validated_selection$region
    name <- data_cat_terr |> filter(REG == cog) |> distinct(LIBREG) |> pull()
    cat <- NULL
    libcat <- ifelse(cog %in% c("01", "02", "03", "04", "06"), "Régions d'Outre-Mer", "Régions de métropole")
  }

  else if (validated_selection$last_validated_typology == "departement"){
    cog <- validated_selection$departement
    name <- data_cat_terr |> filter(DEP == cog) |> distinct(LIBDEP) |> pull()
    cat <- data_cat_terr |> filter(DEP == cog) |> distinct(CAT_DEP) |> pull()
    libcat <- data_cat_terr |> filter(DEP == cog) |> distinct(LIB_CAT_DEP) |> pull()
  }

  else if (validated_selection$last_validated_typology == "epci"){
    cog <- validated_selection$epci
    name <- data_cat_terr |> filter(EPCI == cog) |> distinct(LIBEPCI) |> pull()
    cat <- data_cat_terr |> filter(EPCI == cog) |> distinct(NATURE_EPCI) |> pull()
    libcat <- switch(cat,
                     CC = "Communauté de communes",
                     CA = "Communauté d'agglomération",
                     CU = "Communauté urbaine",
                     METRO = "Métropole",
                     "Etablissement public territorial" = "Établissements publics territoriaux")
  }

  else if (validated_selection$last_validated_typology == "commune"){
    cog <- validated_selection$commune
    name <- data_cat_terr |> filter(COM == cog) |> pull(LIBCOM)
    cat <- data_cat_terr |> filter(COM == cog) |> pull(CAT_COM)
    libcat <- data_cat_terr |> filter(COM == cog) |> pull(LIB_CAT_COM)
  }

  return(list(cog = cog, name = name, cat = cat, libcat = libcat))

}



#' @title Transform a category label for UI display
#'
#' @description This function transforms a given category label for a territory
#' into a formatted version for display in an introduction card in the UI.
#' It replaces specific administrative names as defined in `data_cat_terr`
#' with their corresponding display-friendly labels.
#'
#' @param text A single character string representing the category to transform.
#'
#' @return A character string with the transformed category name. If the input
#' does not match any known category, it is returned unchanged.
#'
#' @noRd
transform_category <- function(text) {
  dico <- c(
    "Rural à habitat dispersé" = "communes rurales à habitat dispersé",
    "Centres urbains intermédiaires" = "centres urbains intermédiaires",
    "Bourgs ruraux" = "bourgs ruraux",
    "Ceintures urbaines" = "ceintures urbaines",
    "Rural à habitat très dispersé" = "communes rurales à habitat très dispersé",
    "Petites villes" = "petites villes",
    "Grands centres urbains" = "grands centres urbains",
    "Département à prédominance rurale" = "départements à prédominance rurale",
    "Département à prédominance urbaine" = "départements à prédominance urbaine",
    "Département intermédiaire" = "départements intermédiaires",
    "CC" = "communautés de communes",
    "CA" = "communautés d'agglomération",
    "CU" = "communautés urbaines",
    "METRO" = "métropoles",
    "Etablissement public territorial" = "établissement publics territoriaux"
  )

  if (text %in% names(dico)) {
    return(dico[[text]])
  } else {
    return(text)
  }
}



#' Generate a heatmap color scaler function
#'
#' This function creates a color scaling function for heatmaps using a numeric palette
#' between a low and high color, based on the minimum and maximum values in the data.
#'
#' @param data A dataframe or tibble containing numeric (or character-coercible) columns.
#' @param heatmap_rows A vector specifying rows to include.
#' @param heatmap_clos A vector specifying columns to include.
#' @param color_low A string specifying the low color for the palette (e.g. "#F0F0F0").
#' @param color_high A string specifying the high color for the palette (e.g. "#FF0000").
#'
#' @return A function that maps numeric values within the data's range to colors in the palette.
#'
#' @import dplyr
#' @import scales
#' @noRd
heatmap_scaler <- function(data, color_low, color_high){
  # Return function for color_scaling
  scales::col_numeric(
    palette = c(color_low, color_high),
    domain = c(min(data), max(data))
  )
}



#' @title Calculate centroids for a given French administrative typology
#'
#' @description This function reads the corresponding GeoJSON file for the requested
#' administrative typology, computes centroids of the selected territory, and returns
#' a list with the centroid coordinates (longitude and latitude).
#'
#' @param selected_territory Character. INSEE code of the diplayed territory.
#' @param typology Character. The administrative typology to process.
#'   Must be one of: "region", "departement", "epci", "commune".
#' @param admin_cache `reactiveValues` — A shared reactive cache used across Shiny modules.
#' The loaded layers are stored here and reused if already present. Only useful when typology == "epci", else set to NULL.
#'
#' @return A named list with two elements :
#'   - `lon`: longitude of the centroid
#'   - `lat`: latitude of the centroid
#'
#' @import sf dplyr
#' @noRd
get_centroids <- function(selected_territory, typology, admin_cache = NULL) {

  code_col <- switch(typology,
                     region = "INSEE_REG",
                     departement = "INSEE_DEP",
                     epci = "INSEE_EPCI",
                     commune = "INSEE_COM")

  sf_obj <- switch(typology,
                     region = admin_region,
                     departement = admin_departement,
                     epci = load_admin_sf("epci", admin_cache))

  sf_obj <- sf_obj |>
    filter(!!sym(code_col) == selected_territory)

  return(list(lng = sf_obj |> pull(X_CENTROID),
              lat = sf_obj |> pull(Y_CENTROID)))
}



#' @title Format numeric values with European style and optional suffix
#'
#' @description This function formats numeric values using a comma as the decimal separator
#' (European style), with exactly one digit after the decimal point. It also appends
#' an optional suffix (e.g., units such as `"%"`, `" km"`, etc.).
#'
#' Commonly used to display values in user interfaces, particularly value boxes
#' or tooltip labels in Shiny applications.
#'
#' @param val A numeric scalar or vector. The value(s) to be formatted.
#' @param suffix A character string to append to the formatted number (e.g., `"%"`, `" km/hab"`).
#'   Defaults to an empty string.
#'
#' @return A character string (or vector) with the formatted value(s), rounded to one decimal place,
#'   using a comma as decimal separator, and including the specified suffix.
#'
#' @noRd
format_val <- function(val, suffix = "") {
  if (is.na(val)){
    return("-")
  } else if (val >= 1e3){
    formatted_string <- format(round(val, 0), big.mark = " ", decimal.mark = ",", nsmall = 0)
  } else if (val > 100){
    formatted_string <- format(round(val, 1), decimal.mark = ",", nsmall = 0)
  } else {
    formatted_string <- format(round(val, 1), decimal.mark = ",", nsmall = 1)
  }
  paste0(
    formatted_string,
    suffix
  )
}



#' @title Safely Add Polygon Layers to a Leaflet Map
#'
#' @description This helper function adds polygons to a Leaflet map only if the
#' provided `data` is not `NULL` and contains at least one row.
#' It prevents errors when attempting to display empty or missing spatial layers.
#'
#' @param map A `leaflet` map object.
#' @param data A `data.frame` or `sf` object containing spatial polygon features.
#'   If `NULL` or empty, no layer will be added.
#' @param ... Additional arguments passed to [leaflet::addPolygons()].
#'
#' @return A `leaflet` map object, unchanged if no data is provided, or
#' updated with the polygon layer otherwise.
#'
#' @importFrom leaflet addPolygons
#' @noRd
safe_addPolygons <- function(map, data, ...) {
  if (!is.null(data) && nrow(data) > 0) {
    map <- addPolygons(map, data = data, ...)
  }
  return(map)
}



#' @title Safely Add Circle Marker Layers to a Leaflet Map
#'
#' @description This helper function adds circle markers to a Leaflet map only if
#' the provided `data` is not `NULL` and contains at least one row.
#' It prevents errors when attempting to display empty or missing point layers.
#'
#' @param map A `leaflet` map object.
#' @param data A `data.frame` or `sf` object containing point features with
#'   coordinates. If `NULL` or empty, no layer will be added.
#' @param ... Additional arguments passed to [leaflet::addCircleMarkers()].
#'
#' @return A `leaflet` map object, unchanged if no data is provided, or
#' updated with the circle marker layer otherwise.
#'
#' @importFrom leaflet addCircleMarkers
#' @noRd
safe_addCircleMarkers <- function(map, data, ...) {
  if (!is.null(data) && nrow(data) > 0) {
    map <- addCircleMarkers(map, data = data, ...)
  }
  return(map)
}



#' @title Geographic zoom presets for France and overseas territories
#'
#' @description This helper function provides a predefined list of geographic centers
#' and zoom levels for mainland France ("Métropole") and French overseas
#' territories (Guadeloupe, Martinique, Guyane, Réunion, Mayotte).
#'
#' @return
#' A named list of lists. Each top-level element corresponds to one
#' territory and contains three numeric elements:
#' \code{lat}, \code{lng}, and \code{zoom}.
#'
#' @noRd
zones <- function(){
  list(
    "France métropolitaine" = list(lat = 46.5, lng = 2.3, zoom = 6),
    "Guadeloupe"            = list(lat = 16.25, lng = -61.55, zoom = 9),
    "Martinique"            = list(lat = 14.67, lng = -61.0, zoom = 9),
    "Guyane"                = list(lat = 4.0, lng = -53.2, zoom = 7),
    "Réunion"               = list(lat = -21.1, lng = 55.5, zoom = 9),
    "Mayotte"               = list(lat = -12.8, lng = 45.15, zoom = 10),
    "Nouvelle-Calédonie"    = list(lat = -21.1, lng = 165.4, zoom = 8)
  )
}



#' @title Plot a "No Data" Message Using ggplotly
#'
#' @description This function creates a placeholder plot using \code{ggplot2} and converts it to
#' an interactive \code{plotly} object, displaying a custom message when the dataset
#' is empty or has no relevant data. The message text is shown in a gray color, and
#' the plot title can be customized.
#'
#' @param label Character string. The message to display in the plot area.
#' @param label_size Numeric. Font size of the message label in the plot.
#' @param title Character string. Title of the plot.
#' @param x_title Numeric between 0 and 1. Horizontal position of the title (left = 0, center = 0.5, right = 1).
#' @param y_title Numeric between 0 and 1. Vertical position of the title (0 = bottom, 1 = top).
#' @param size_title Numeric. Font size of the plot title.
#' @param x_coord Numeric. x coordinate of the message label.
#' @param y_coord Numeric. y coordinate of the message label.
#'
#' @return A \code{plotly} object with a "no data" message displayed.
#'
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#' @importFrom grDevices rgb
#' @noRd
plot_message_no_data <- function(label, label_size, title, x_title, y_title, size_title,
                                 x_coord, y_coord, t = 10, b = 0, l = 20, r = 0){

  p <- ggplot() +
    geom_text(aes(x = x_coord, y = y_coord, label = label),
              size = label_size, family = "Poppins", color = "darkgrey") +
    ylim(0.5, 1) +
    xlim(0, 1) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = t, b = b, l = l, r = r)
    )

  g <- ggplotly(p) |>
    plotly::layout(
      title = list(
        text = title,
        x = x_title,
        y = y_title,
        font = list(size = size_title, color = rvm_colour('raisin_black'))
      )
    )

  return(g)
}



#' @title Create a color palette for a map showing positive and negative values
#'
#' @description This function generates a color mapping function (`pal`) and a corresponding
#' legend (colors and labels) to visualize positive and negative values.
#' It interpolates between specified minimum and maximum thresholds
#' using a progressive color ramp. Values below the negative threshold and
#' above the positive threshold are assigned fixed extreme colors. Missing or
#' non-finite values are rendered in grey.
#'
#' @param threshold_min Numeric. Lower threshold for negative values. Values
#'   smaller than this threshold are assigned the extreme negative color.
#' @param threshold_max Numeric. Upper threshold for positive values. Values
#'   greater than this threshold are assigned the extreme positive color.
#' @param colors Character vector. Colors to be used in the legend. Must have
#'   the same length as \code{labels}.
#' @param labels Character vector. Labels to be displayed in the legend.
#'   Must have the same length as \code{colors}.
#' @param color_pos_min Character. Hex code of the color representing small
#'   positive values (close to 0).
#' @param color_pos_max Character. Hex code of the color representing large
#'   positive values (close to \code{threshold_max}).
#' @param color_neg_min Character. Hex code of the color representing large
#'   negative values (close to \code{threshold_min}).
#' @param color_neg_max Character. Hex code of the color representing small
#'   negative values (close to 0).
#' @param admin_sf A \code{sf} object (or data frame) containing a column
#'   \code{value}. Used to determine whether finite values exist for building
#'   the legend.
#'
#' @return A list with three elements:
#'   \item{pal}{A function that maps numeric input values to hex color codes.}
#'   \item{legend_colors}{Character vector of colors to be displayed in the
#'     legend. If no finite values are present, returns grey (\code{"#d3d3d3"}).}
#'   \item{legend_labels}{Character vector of labels to be displayed in the
#'     legend. If no finite values are present, returns \code{"NA"}.}
#'
#' @noRd
color_palette_map_evol <- function(threshold_min, threshold_max,
                                   colors, labels,
                                   color_pos_min, color_pos_max,
                                   color_neg_min, color_neg_max,
                                   admin_sf){

  # Progressive palette with minimal and maximal threshold
  pal <- function(x) {
    sapply(x, function(val) {
      if (is.na(val)) return("#d3d3d3")
      else if (val < threshold_min) return(color_neg_min)
      else if (val < 0) {
        prop <- (val - 0)/(threshold_min - 0)
        colorRampPalette(c(color_neg_max, color_neg_min))(100)[round(prop*99)+1]
      }
      else if (val > threshold_max) return(color_pos_max)
      else if (val > 0) {
        prop <- val/threshold_max
        colorRampPalette(c(color_pos_min, color_pos_max))(100)[round(prop*99)+1]
      }
      else return("#ffffff")
    })
  }

  # Legend labels
  finite_values <- admin_sf$value[is.finite(admin_sf$value)]
  if(length(finite_values) == 0){
    legend_colors <- "#d3d3d3"
    legend_labels <- "NA"
  } else {
    legend_colors <- colors
    legend_labels <- labels
  }

  return(list(pal = pal, legend_colors = legend_colors, legend_labels = legend_labels))

}



#' @title Create a sequential color palette for a map showing only positive values
#'
#' @description This function generates a color mapping function (`pal`) and a corresponding
#' legend (colors and labels) to visualize strictly positive values. Values
#' greater than the maximum threshold are assigned a fixed extreme color.
#' Missing, non-finite, or negative values are rendered in grey.
#'
#' @param threshold_max Numeric. Upper threshold for positive values. Values
#'   greater than this threshold are assigned the extreme positive color.
#' @param colors Character vector. Colors to be used in the legend. Must have
#'   the same length as \code{labels}.
#' @param labels Character vector. Labels to be displayed in the legend.
#'   Must have the same length as \code{colors}.
#' @param color_pos_min Character. Hex code of the color representing small
#'   positive values (close to 0).
#' @param color_pos_max Character. Hex code of the color representing large
#'   positive values (close to \code{threshold_max}).
#' @param admin_sf A \code{sf} object (or data frame) containing a column
#'   \code{value}. Used to determine whether finite values exist for building
#'   the legend.
#' @param threshold_in Numeric. Lower threshold for positive values. Values
#'   lower than this threshold are assigned the lowest positive color. Defaults to 0.
#'
#' @return A list with three elements:
#'   \item{pal}{A function that maps numeric input values to hex color codes.}
#'   \item{legend_colors}{Character vector of colors to be displayed in the
#'     legend. If no finite values are present, returns grey (\code{"#d3d3d3"}).}
#'   \item{legend_labels}{Character vector of labels to be displayed in the
#'     legend. If no finite values are present, returns \code{"NA"}.}
#'
#' @noRd
color_palette_map_value <- function(threshold_max,
                                    colors, labels,
                                    color_pos_min, color_pos_max,
                                    admin_sf, threshold_min = 0){

  # Progressive palette with minimal and maximal threshold
  pal <- function(x) {
    sapply(x, function(val) {
      if (is.na(val)) return("#d3d3d3")
      else if (threshold_min !=0 & val <= threshold_min) return(color_pos_min)
      else if (threshold_min ==0 & val < threshold_min) return("#d3d3d3")
      else if (val > threshold_max) return(color_pos_max)
      else if (val > threshold_min) {
        prop <- (val - threshold_min) / (threshold_max - threshold_min)
        colorRampPalette(c(color_pos_min, color_pos_max))(100)[round(prop*99)+1]
      }
      else return("#ffffff")
    })
  }

  # Legend labels
  finite_values <- admin_sf$value[is.finite(admin_sf$value)]
  if(length(finite_values) == 0){
    legend_colors <- "#d3d3d3"
    legend_labels <- "NA"
  } else {
    legend_colors <- colors
    legend_labels <- labels
  }

  return(list(pal = pal, legend_colors = legend_colors, legend_labels = legend_labels))

}



#' @title Draw the Outline of a Selected Territory on a Leaflet Map
#'
#' @description
#' This function adds the geographic outline of a selected administrative
#' territory (region, department, EPCI, or commune) to an existing
#' [leaflet::leaflet()] map. It is typically used to highlight the boundary of
#' the user's selected territory on top of other map layers.
#'
#' @param map A [leaflet::leaflet()] map widget object to which the outline will
#'   be added.
#' @param selected_territory A character string giving the code of the selected
#'   territory (e.g., INSEE code of a region, department, EPCI, or commune).
#' @param typology A character string specifying the administrative level of the
#'   selected territory. Must be one of `"france"`, `"region"`, `"departement"`,
#'   `"epci"`, or `"commune"`. If `"france"`, the function returns the map
#'   unchanged.
#' @param admin_cache `reactiveValues` — A shared reactive cache used across Shiny modules.
#' The loaded layers are stored here and reused if already present. Only useful when typology == "epci", else set to NULL.
#'
#' @return
#' A [leaflet::leaflet()] map widget with the selected territory outline added.
#'
#' @import leaflet
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom sf st_as_sf
#' @noRd
draw_territory_outline <- function(map, selected_territory, typology, admin_cache = NULL) {

  if (typology == "france"){
    return(map)
  }

  code_col <- switch(typology,
                     region = "INSEE_REG",
                     departement = "INSEE_DEP",
                     epci = "INSEE_EPCI",
                     commune = "INSEE_COM")

  sf_obj <- switch(typology,
                   region = admin_region,
                   departement = admin_departement,
                   epci = load_admin_sf("epci", admin_cache))

  sf_obj <- sf_obj |>
    filter(!!sym(code_col) == selected_territory)

  map <- map |>
    addPolygons(
      data = sf_obj,
      fill = FALSE,
      color = "#444444",
      weight = 4,
      opacity = 1
    )

  return(map)
}



#' @title Insert a line break near the middle of a long text string
#'
#' @description Splits a long character string into two lines at the nearest word boundary
#' to the middle of the text. If the string is shorter than a given maximum length or contains
#' no spaces, the function returns it unchanged.
#' Optionally, the function can insert an HTML line break (`<br>`) instead of a newline character.
#'
#' @param text Character string. The input text to be split.
#' @param max_length Integer. The maximum number of characters before attempting to insert a line break.
#' Default is `25`.
#' @param html Logical. If `TRUE`, inserts `<br>` as the line break (useful for HTML labels or tooltips);
#' if `FALSE`, inserts a standard newline (`\n`). Default is `FALSE`.
#'
#' @return A character string with a line break inserted between words near the middle of the text,
#' or the original string if no break is needed.
#'
#' @noRd
insert_linebreak_middle <- function(text, max_length = 25, html = FALSE) {
  # If the string is short or contains no spaces, return it unchanged
  if (nchar(text) <= max_length || !grepl(" ", text)) return(text)

  # Split the string into words
  words <- strsplit(text, " ")[[1]]

  # If there is only one word, return unchanged
  if (length(words) == 1) return(text)

  # Compute cumulative word lengths (including spaces)
  cum_lengths <- cumsum(nchar(words) + 1)  # +1 accounts for space between words

  # Find the index closest to the middle of the string
  split_index <- which.min(abs(cum_lengths - (nchar(text) / 2)))

  # Rebuild the string with the line break inserted
  separator <- ifelse(html, "<br>", "\n")
  paste(
    paste(words[1:split_index], collapse = " "),
    paste(words[(split_index + 1):length(words)], collapse = " "),
    sep = separator
  )
}
