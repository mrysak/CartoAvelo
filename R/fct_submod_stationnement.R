#' @title Generate Local Comparison Data for Bicycle Parking Indicators
#'
#' @description This function computes comparison statistics for a selected bicycle parking indicator across
#' all territories belonging to the same category as a selected territory (e.g., same type of EPCI, département, or region).
#' It enables localized benchmarking by calculating the **minimum**, **maximum**, and **weighted average** of the indicator
#' for comparable territories.
#'
#' The function also returns the formatted label of the comparison group (e.g., "communautés d'agglomération",
#' or "régions d'outre-mer") for use in visualization.
#'
#' The indicators supported are:
#' - `"stat_hab"`: Bicycle parking for every 1,000 residents (/1000hab)
#' - `"stat_km"`: Bicycle parking per kilometer of bike path (/km)
#'
#' @param data_cat_terr A data frame describing all territories and their categories (e.g., `COM`, `EPCI`, `DEP`, `REG`, and associated category labels).
#' @param stationnement_data A data frame containing bicycle barking infrastructure data by territory and year.
#' @param typology A string defining the level of analysis. One of `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param indicator A string indicating the indicator to calculate. Must be one of: `"stat_hab"` or `"stat_km"`.
#' @param selected_cog A string or code identifying the selected territory (e.g., INSEE code).
#' @param current_year Latest year with data.
#' @param start_year First year to include.
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
generate_stationnement_comparison_data_local <- function(data_cat_terr, stationnement_data,
                                                         indicator, typology, selected_cog,
                                                         start_year, current_year){
  time_period <- seq(start_year, current_year)

  # Columns containign the selected indicator
  cols_comparison <- switch(indicator,
                            stat_hab = paste0("NB_STAT_1000HAB_", time_period),
                            stat_km = paste0("NB_STAT_1KM-AC_", time_period))

  cols_to_select <- switch(indicator,
                           stat_hab = c(cols_comparison, "POP"),
                           stat_km = c(cols_comparison, paste0("KM_AMGT_", time_period)))

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

  if (typology == "epci"){
    data_cat_terr <- data_cat_terr |>
      mutate(EPCI = as.character(EPCI))
  }

  data_comparison <- data_cat_terr |>
    filter(.data[[colname[2]]] == lib_category) |>
    select(all_of(colname[1])) |>
    unique() |>
    left_join(stationnement_data |> select(all_of(c(colname[3], cols_to_select))),
              by = setNames(colname[3], colname[1])) |>
    filter(!if_all(-colname[1], is.na))

  # Rule out communes where the indicator is null over the whole period
  data_comparison <- data_comparison |>
    filter(!if_all(all_of(cols_comparison), ~ .x == 0))

  # Compute indicator
  if (indicator == "stat_hab"){

    # Reformat to ease computation
    data_comparison_long <- data_comparison |>
      pivot_longer(
        cols = all_of(cols_comparison),
        names_to = "year",
        values_to = "indicator"
      ) |>
      mutate(
        year = str_extract(year, "\\d{4}$"),
        year = as.integer(year)
      )

    # Compute chosen indicator by category
    data_comparison_cat <- data_comparison_long |>
      mutate(
        stat = indicator * POP
      ) |>
      group_by(year) |>
      summarise(
        Moy = sum(stat, na.rm = TRUE) / sum(POP, na.rm = TRUE),
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
        year = as.character(year)
      )

  } else if (indicator == "stat_km"){

    # Reformat to ease computation
    data_comparison_long <- data_comparison |>
      pivot_longer(
        cols = matches("NB_STAT_1KM-AC_|KM_AMGT_"),
        names_to = c(".value", "year"),
        names_pattern = "(NB_STAT_1KM-AC_|KM_AMGT_)(\\d{4})"
      ) |>
      rename(
        indicator = `NB_STAT_1KM-AC_`,
        km_ac = `KM_AMGT_`
      ) |>
      mutate(
        year = as.integer(year)
      )

    # Compute chosen indicator by category
    data_comparison_cat <- data_comparison_long |>
      mutate(
        stat = indicator * km_ac
      ) |>
      group_by(year) |>
      summarise(
        Moy = sum(stat, na.rm = TRUE) / sum(km_ac, na.rm = TRUE),
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
        year = as.character(year)
      )
  }

  lib_category <- transform_category(lib_category)

  return(list(data_comparison_cat = data_comparison_cat, lib_category = lib_category))

}



#' @title Generate National Comparison Data for Bicycle Parking Indicators
#'
#' @description This function computes aggregated comparison statistics for a selected bicycle parking indicator
#' across three categories of French communes: *Grands centres urbains*, *Communes intermédiaires*,
#' and *Communes rurales*. The aggregation is done at the national level (i.e., all of France),
#' and returns a time series per category suitable for benchmarking visualizations.
#'
#' The commune typology is based on `CAT_COM` codes:
#' - `1` for *Grands centres urbains*
#' - `2, 3, 4` for *Communes intermédiaires*
#' - `5, 6, 7` for *Communes rurales*
#'
#' The indicators supported are:
#' - `"stat_hab"`: Bicycle parking for every 1,000 residents (/1000hab)
#' - `"stat_km"`: Bicycle parking per kilometer of bike path (/km)
#'
#' @param data_cat_terr Dataframe containing territorial categorisation, including department codes, labels, region codes and labels.
#' @param stationnement_communal A data frame with bicycle parking data for each commune.
#' @param stationnement_data A data frame containing bicycle barking infrastructure data by territory and year.
#' @param indicator A string indicating the indicator to calculate. Must be one of: `"stat_hab"` or `"stat_km"`.
#' @param selected_cog A string or code identifying the selected territory (e.g., INSEE code).
#' @param current_year Latest year with data.
#' @param start_year First year to include.
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
generate_stationnement_comparison_data_national <- function(data_cat_terr, stationnement_communal,
                                                            indicator, start_year, current_year){

  time_period <- seq(start_year, current_year)

  # Columns containign the selected indicator
  cols_comparison <- switch(indicator,
                            stat_hab = paste0("NB_STAT_1000HAB_", time_period),
                            stat_km = paste0("NB_STAT_1KM-AC_", time_period))

  cols_to_select <- switch(indicator,
                           stat_hab = c(cols_comparison, "POP"),
                           stat_km = c(cols_comparison, paste0("KM_AMGT_", time_period)))

  # Retrieve relevant data and label communes categories
  data_comparison <- data_cat_terr |>
    mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                           CAT_COM %in% c(2, 3, 4) ~ 2,
                           CAT_COM %in% c(5, 6, 7) ~ 3),
           LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                              CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                              CAT_COM %in% c(5, 6, 7) ~ "Communes rurales")) |>
    select(COM, CAT, LIBCAT) |>
    left_join(stationnement_communal |> select(all_of(c("INSEE_COM", cols_to_select))), by = c("COM" = "INSEE_COM")) |>
    filter(!if_all(-all_of(c("COM", "CAT", "LIBCAT")), is.na))

  # Rule out communes where the indicator is null over the whole period
  data_comparison <- data_comparison |>
    filter(!if_all(all_of(cols_comparison), ~ .x == 0))

  # Reformat to ease computation
  if (indicator == "stat_hab"){
    data_comparison_long <- data_comparison |>
      pivot_longer(
        cols = all_of(cols_comparison),
        names_to = "year",
        values_to = "indicator"
      ) |>
      mutate(
        year = str_extract(year, "\\d{4}$"),
        year = as.character(year)
      ) |>
      rename(denom = POP)
  } else if (indicator == "stat_km"){
    data_comparison_long <- data_comparison |>
      pivot_longer(
        cols = matches("NB_STAT_1KM-AC_|KM_AMGT_"),
        names_to = c(".value", "year"),
        names_pattern = "(NB_STAT_1KM-AC_|KM_AMGT_)(\\d{4})"
      ) |>
      rename(
        indicator = `NB_STAT_1KM-AC_`,
        denom = `KM_AMGT_`
      ) |>
      mutate(
        year = as.character(year)
      )
  }

  # Compute chosen indicator by category
  data_comparison_cat <- data_comparison_long |>
    group_by(CAT, LIBCAT, year) |>
    summarise(
      indicator = sum(indicator * denom, na.rm = TRUE) / sum(denom, na.rm = TRUE),
      .groups = "drop"
    )

  return(data_comparison_cat)

}



#' @title Plot the Evolution of a Bicycle Parking Indicator with Comparison
#'
#' @description This function generates an interactive `plotly` bar and line chart showing the temporal evolution
#' of a bicycle parking indicator for a selected territory. It also overlays comparative reference lines from other territories in
#' the same category (e.g., similar communes, EPCI, or regions).
#'
#' @param data A data frame containing annual values of the indicator for the selected territory.
#' @param data_comparison_cat A data frame containing indicator values for the comparison categories (e.g., min, max, average of similar territories).
#' @param indicator A string specifying the indicator to be plotted. Must be one of: `"stat_hab"`, or `"stat_km"`.
#' @param selected_territory A character string naming the territory of interest (e.g., a commune, EPCI, département, or région).
#' @param current_year Latest year with data.
#' @param start_year First year to include.
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
plot_evolution_stationnement <- function(data, data_comparison_cat,
                                         indicator, selected_territory,
                                         start_year, current_year,
                                         typology, lib_category){

  root <- switch(indicator,
                 stat_hab = c("NB_STAT_1000HAB_"),
                 stat_km = c("NB_STAT_1KM-AC_"))

  indicator_title <- switch(indicator,
                            stat_hab = "Stationnements par habitant",
                            stat_km = "Stationnements par km d'aménagement")

  time_period <- seq(start_year, current_year)

  n_decimals <- 2

  cols_to_display <- paste0(root, time_period)


  ## Compute indicator
  data_to_display <- data |>
    select(all_of(cols_to_display)) |>
    pivot_longer(
      cols = all_of(cols_to_display),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = str_remove(year, root)
    )


  ## If territory has no data, print message
  if (nrow(data_to_display) == 0){
    return(plot_message_no_data("Pas de données disponibles sur le territoire\npour cet indicateur", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.5, y_coord = 0.85, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  }


  ## Custom tooltip for main data, with evolution

  # Units to display
  unit <- switch(indicator,
                 stat_hab = "/1 000 hab",
                 stat_km = "/km")

  # Compute evolution
  data_to_display <- data_to_display |>
    #mutate(year = as.integer(year)) |>
    arrange(year) |>
    mutate(evolution_rate = ((value / dplyr::lag(value)) - 1) * 100)

  # Join over the previous year
  data_to_display <- data_to_display  |>
    mutate(
      tooltip = ifelse(year == start_year,
                       paste0(indicator_title,
                              " en ", year, " : <span style='font-weight:bold'>",
                              format(round(value, n_decimals), decimal.mark = ",", big.mark = " "), " ", unit, "</span>"),
                       paste0(indicator_title,
                              " en ", year, " : <span style='font-weight:bold'>",
                              format(round(value, n_decimals), decimal.mark = ",", big.mark = " "), " ", unit, "</span>\n",
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


  ## Custom tooltip and colors for comparison data
  if (typology == "france"){
    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(indicator_title,
                         " dans les\n", str_to_lower(LIBCAT),
                         " en ", year, " : <span style='font-weight:bold'>",
                         format(round(indicator, n_decimals), decimal.mark = ",", big.mark = " "), " ", unit, "</span>")
      )

    custom_colors <- c(
      "Grands centres urbains" = "#E94F35",
      "Communes intermédiaires" = "#9185BE",
      "Communes rurales" = "#C6CE41"
    )
  } else {

    if (typology %in% c("commune", "epci")){
      data_comparison_cat <- data_comparison_cat |>
        filter(LIBCAT == "Moy")
    }

    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(case_when(LIBCAT == "Moy" ~ "Moyenne pondérée des ",
                                   LIBCAT == "Min" ~ "Minimum des ",
                                   LIBCAT == "Max" ~ "Maximum des ",
                                   TRUE ~ ""),
                         str_to_lower(indicator_title), "\ndans les ", str_to_lower(lib_category),
                         " en ", year, " : <span style='font-weight:bold'>",
                         format(round(indicator, n_decimals), decimal.mark = ",", big.mark = " "), " ", unit, "</span>")
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
          switch(indicator,
                 stat_hab = "Stationnements vélo pour 1 000 habitants",
                 stat_km = "Stationnements vélo par km d'aménagement cyclable"),
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
  } else if (typology %in% c("commune", "epci")) {
    g <- g %>%
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 2, showlegend = TRUE)
  } else {
    g <- g %>%
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[3], traces = 2, showlegend = TRUE) %>%
      style(name = line_categories[2], traces = 3, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 4, showlegend = TRUE)
  }

  return(g)
}



#' @title Generate a tooltip label for an interactive bicycle parking map
#'
#' @description Creates a formatted HTML tooltip label string to display on a leaflet
#' map, showing the indicator value and its evolution from the previous year.
#'
#' @param value Numeric. The current value of the selected indicator, or its evolution.
#' @param label Numeric. Additional information to display in the label.
#' @param info_to_display Character. Either `"value"` or `"evol"` to determine which value to display as the main value.
#' @param indicator_choice Character. One of `"stat_hab"`, or `"stat_km"`.
#' Determines which indicator is displayed and the corresponding units and label.
#' @param current_year Character. The latest year with data.
#'
#' @return A character string with HTML formatting, ready to be used in a leaflet tooltip.
#' The string includes the main value, unit, and, if applicable, the evolution from the previous year.
#'
#' @noRd
tooltip_label_stationnement <- function(value, label, info_to_display, indicator_choice, current_year){

  general_label <- switch(indicator_choice,
                          stat_hab = "Stationnements vélo pour 1 000 habitants : <span style='font-weight:bold'>",
                          stat_km = "Stationnements vélo par km d'aménagement cyclable : <span style='font-weight:bold'>")

  unit <- switch(indicator_choice,
                 stat_hab = c(" /1 000 hab", "%"),
                 stat_km = c(" /km", "%"))

  val <- ifelse(info_to_display == "value", value, label)

  evol <- ifelse(info_to_display == "value", label, value)

  value <- ifelse(is.na(val),
                  "-",
                  format(round(val, 2), decimal.mark = ","))

  prefix <- ifelse(!is.na(evol) && evol >= 0, "+", "")
  evolution <- ifelse(is.na(evol),
                      paste0("Évolution /", current_year - 1, " : -"),
                      paste0("Évolution /", current_year - 1, " : <span style='font-weight:bold'>",
                             prefix, format(round(evol, 2), decimal.mark = ","), unit[2], "</span>"))

  legend <- paste0(general_label, value, unit[1], "</span></br>", evolution)

  return(legend)
}



#' @title Interactive map of biycle parking indicators using Leaflet
#'
#' @description Generates an interactive choropleth leaflet map of bicycle parking indicators
#' (parking spots per resident or parking sport per km of cycling infrastructure),
#' including tooltips, legends, and export options.
#'
#' @param data_cat_terr A dataframe containing territorial categorisation information for French administrative units.
#' @param stationnement_data A dataframe containing bicycle parking data.
#' @param admin_sf sf object. Spatial administrative boundaries (communes, EPCI, départements, régions).
#' @param tab_typology Character. Typology of the displayed territory. One of `"france"`, `"region"`, `"departement"`, or `"epci"`.
#' @param display_typology Character. Administrative scale used for display. One of `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param selected_territory Character. INSEE code of the territory selected by the user.
#' @param indicator_choice Character. The cycling indicator to display. One of:
#' \itemize{
#'   \item `"stat_hab"` — Bicycle parking for every 1,000 residents (/1000hab).
#'   \item `"stat_km"` — Bicycle parking per kilometer of bike path (/km).
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
map_stationnement_indicators <- function(data_cat_terr, stationnement_data, admin_sf,
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

  # Join administrative data with stationnement data
  admin_sf <- admin_sf |>
    left_join(stationnement_data |>  mutate(across(all_of(insee_col_admin), as.character)),
              by = insee_col_admin)

  # Extract data corresponding to the territory selected by the user, according to the selected display scale
  if (tab_typology != "france"){
    if(tab_typology == "region"){
      list <- data_cat_terr |>
        filter(REG == selected_territory)
    } else if(tab_typology == "departement"){
      list <- data_cat_terr |>
        filter(DEP == selected_territory)
    } else if(tab_typology == "epci"){
      list <- data_cat_terr |>
        filter(EPCI == selected_territory)
    }
    list <- list |>  pull(.data[[insee_col_cat_terr]]) |>  unique()
    admin_sf <- admin_sf |>  filter(.data[[insee_col_admin]] %in% list)
  }

  # Get territory name depending on the selected display scale
  admin_sf <- admin_sf |>
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
    info_to_display == "value" & indicator_choice == "stat_hab" ~
      "Stationnements vélo pour 1 000 habitants",
    info_to_display == "evol" & indicator_choice == "stat_hab" ~
      paste0("Évolution du nombre de stationnements vélo<br/>pour 1 000 habitants par rapport à ", current_year - 1, " (%)"),
    info_to_display == "value" & indicator_choice == "stat_km" ~
      "Stationnements vélo par km<br/>d'aménagement cyclable",
    info_to_display == "evol" & indicator_choice == "stat_km" ~
      paste0("Évolution du nombre de stationnements<br/>vélo par km d'aménagement cyclable<br/>par rapport à ", current_year - 1, " (%)"),
    TRUE ~ "Légende"
  )


  # ----- ADDITIONAL DATA PREPARATION -----

  # Compute evolution
  prefix <- switch(indicator_choice,
                   stat_hab = "NB_STAT_1000HAB_",
                   stat_km = "NB_STAT_1KM-AC_")

  selected_col <- paste0(prefix, current_year)
  previous_col <- paste0(prefix, current_year - 1)

  admin_sf <- admin_sf |>
    mutate(!!selected_col := .data[[selected_col]]) |>
    mutate(!!previous_col := .data[[previous_col]]) |>
    mutate(EVOL = if_else(
      get(previous_col) != 0,
      100 * ((get(selected_col) / get(previous_col)) - 1),
      NA_real_)
    )

  # Rename columns
  admin_sf <- admin_sf  |>
    rename(!!ifelse(info_to_display == "value", "value", "label") := all_of(selected_col),
           !!ifelse(info_to_display == "value", "label", "value") := EVOL)


  # ---- COLOR PALETTE AND LEGEND ----

  if (info_to_display == "evol") {
    if (display_typology == "region"){
      color_palette_legend <- color_palette_map_value(30,
                                                      c("#F6F9ED", "#e3ebb3", "#cddd7a", "#b8ce41"),
                                                      c("0 à 10%", "10 à 20%", "20 à 30%", "> 30%"),
                                                      "#F6F9ED", "#b8ce41",
                                                      admin_sf)
    } else if (display_typology == "departement"){
      color_palette_legend <- color_palette_map_evol(-20, 40,
                                                     c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("< -20%", "-20 à -10%", "-10 à 0%", "0 à 20%", "20 à 40%", "> 40%"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    } else if (indicator_choice == "stat_hab"){
      color_palette_legend <- color_palette_map_evol(-30, 70,
                                                     c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("< -30%", "-30 à -15%", "-15 à 0%", "0 à 35%", "35 à 70%", "> 70%"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    } else if (indicator_choice == "stat_km"){
      color_palette_legend <- color_palette_map_evol(-70, 70,
                                                     c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("< -70%", "-70 à -35%", "-35 à 0%", "0 à 35%", "35 à 70%", "> 70%"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    }
  } else if (indicator_choice == "stat_hab" & info_to_display == "value"){
    if (display_typology == "commune"){
      color_palette_legend <- color_palette_map_value(50,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 à 15", "15 à 30", "30 à 50", "> 50"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf)
    } else {
      color_palette_legend <- color_palette_map_value(35,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 à 10", "10 à 20", "20 à 35", "> 35"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf)
    }
  } else if (indicator_choice == "stat_km" & info_to_display == "value"){
    if (display_typology %in% c("region", "departement")){
      color_palette_legend <- color_palette_map_value(25,
                                                      c("#e8e6f4", "#c8c2df", "#a79dcb", "#8378ab"),
                                                      c("0 à 5", "5 à 15", "15 à 25", "> 25"),
                                                      "#e8e6f4", "#9185be",
                                                      admin_sf)
    } else if (display_typology %in% c("epci", "commune")){
      color_palette_legend <- color_palette_map_value(40,
                                                      c("#e8e6f4", "#c8c2df", "#a79dcb", "#8378ab"),
                                                      c("0 à 15", "15 à 30", "30 à 40", "> 40"),
                                                      "#e8e6f4", "#9185be",
                                                      admin_sf)
    }


  }

  pal <- color_palette_legend$pal
  legend_colors <- color_palette_legend$legend_colors
  legend_labels <- color_palette_legend$legend_labels


  # ---- LEAFLET MAP ----

  val <- admin_sf$value; lab <- admin_sf$label

  map <- leaflet(admin_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom) |>

    draw_territory_outline(
      selected_territory = selected_territory,
      typology = tab_typology,
      admin_cache = admin_cache) |>

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
          tooltip_label_stationnement(val, lab, info_to_display, indicator_choice, current_year), "</span>"
        )
      }, val, lab, admin_sf$territory_name) |>

        lapply(htmltools::HTML),

      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "14px",direction = "auto")
    ) |>

    addLegend(
      colors = legend_colors,
      labels = legend_labels,
      opacity = 0.7,
      title = legend_title,
      position = "bottomleft",
      labFormat = labelFormat(big.mark = "")) |>

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
    indicator_choice == "stat_hab" ~ 220,
    indicator_choice == "stat_km" & info_to_display == "value" ~ 197,
    indicator_choice == "stat_km" & info_to_display == "evol" ~ 220)

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



#' @title Generate value box content for bicycle parking indicators
#'
#' @description This function prepares formatted textual and numerical content
#' to be displayed inside a **Shiny value box**.
#'
#' @param stationnement_data A data frame containing annual values of the indicator for the selected territory (prefiltered).
#' @param current_year Integer. The most recent year of data available.
#' @param indicator A string specifying the indicator. Must be one of: `"stat_hab"`, or `"stat_km"`.
#'
#' @return
#' A **list** with three elements, suitable for populating a Shiny value box:
#' \describe{
#'   \item{`title`}{Character string. The title to display in the value box (with HTML line breaks).}
#'   \item{`value`}{Character string. The formatted indicator for `current_year`.}
#'   \item{`evol`}{Character string. The formatted evolution compared to the previous year, expressed in percentage.}
#' }
#'
#' @import dplyr
#' @noRd
value_box_content_stationnement <- function(stationnement_data, current_year, indicator){

  previous_year <- current_year - 1

  previous_year <- current_year - 1

  if (indicator == "nb_stat"){

    ## Extract value and compute evolution
    value_current <- stationnement_data |> pull(!!sym(paste0("NB_STAT_", current_year)))
    value_previous <- stationnement_data |> pull(!!sym(paste0("NB_STAT_", previous_year)))

    evol <- value_current - value_previous

    ## Format outputs
    title <- "Total stationnements"

    if (length(value_current) == 0){
      value <- "-"
    } else {
      value <- format(round(value_current, 0), big.mark = " ", trim = TRUE)
    }

    if (length(evol) != 0){
      evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 0), big.mark = " ", trim = TRUE),
                     " par rapport à ", previous_year)
    }
  } else if (indicator == "stat_hab"){

    ## Extract value and compute evolution
    value_current <- stationnement_data |> pull(!!sym(paste0("NB_STAT_1000HAB_", current_year)))
    value_previous <- stationnement_data |> pull(!!sym(paste0("NB_STAT_1000HAB_", previous_year)))

    evol <- 100 * ((value_current/value_previous) - 1)

    ## Format outputs
    title <- "Stationnements pour 1 000 habitants"

    if (length(value_current) == 0){
      value <- "-"
    } else {
      value <- format(round(value_current, 0), big.mark = " ", trim = TRUE)
    }

    if (length(evol) != 0){
      evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 1), decimal.mark = ",", trim = TRUE),
                     "% par rapport à ", previous_year)
    }
  } else if (indicator == "stat_km"){

    ## Extract value and compute evolution
    value_current <- stationnement_data |> pull(!!sym(paste0("NB_STAT_1KM-AC_", current_year)))
    value_previous <- stationnement_data |> pull(!!sym(paste0("NB_STAT_1KM-AC_", previous_year)))

    evol <- 100 * ((value_current/value_previous) - 1)

    ## Format outputs
    title <- "Stationnements par km d'aménagement"

    if (length(value_current) == 0){
      value <- "-"
    } else {
      value <- format(round(value_current, 0), big.mark = " ", trim = TRUE)
    }

    if (length(evol) != 0){
      evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 1), decimal.mark = ",", trim = TRUE),
                     "% par rapport à ", previous_year)
    }
  }

  return(list(title = title, value = value, evol = evol))
}



#' @title Plot the distribution of cycling parking density by commune type and territory
#'
#' @description This function generates a combined interactive visualization (with Plotly)
#' showing the distribution of communes by classes of cycling parking density
#' (in number of parking spots per 1,000 inhabitants) for different commune types
#' ("Grands centres urbains", "Communes intermédiaires", and "Communes rurales"),
#' as well as for the selected geographical territory.
#' The first chart displays, for each commune type, the percentage of communes
#' falling within each density class (stacked horizontal bar chart).
#' The second chart, aligned with the first, shows the total number of parking
#' spots in each commune type. Both charts are interactive, with custom tooltips
#' and a consistent visual theme.
#'
#' @param stationnement_communal A data frame containing cycling parking data
#'   at the commune level, with one row per commune. Must include columns named
#'   `NB_STAT_<year>` and `NB_STAT_1000HAB_<year>` for the selected year.
#' @param data_cat_terr A data frame mapping each commune (identified by its INSEE code)
#'   to its higher territorial units (EPCI, department, region) and its INSEE
#'   density category (\code{CAT_COM}). Used to group communes into the three
#'   aggregate categories:
#'   \itemize{
#'     \item 1 → \strong{Grands centres urbains}
#'     \item 2–4 → \strong{Communes intermédiaires}
#'     \item 5–7 → \strong{Communes rurales}
#'   }
#' @param typology A character string specifying the territorial scale of analysis.
#'   Must be one of \code{"epci"}, \code{"departement"}, \code{"region"}, or \code{"france"}.
#' @param selected_cog The INSEE code identifying the selected territory.
#'   Ignored if \code{typology = "france"}.
#' @param selected_territory A character string giving the display name of the
#'   selected territory (e.g. \emph{"France entière"}, \emph{"Bretagne"}, etc.).
#' @param current_year An integer indicating the most recent year for which
#'   cycling parking data are available (used to select the appropriate columns).
#'
#' @return A Plotly object combining two horizontal bar charts:
#'   \itemize{
#'     \item The left panel shows the percentage of communes in each parking
#'           density class by commune type.
#'     \item The right panel shows the total number of parking spots per commune type.
#'   }
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import scales
#' @importFrom stringr str_to_lower
#' @importFrom tidyr replace_na
#' @noRd
plot_stationnement_density <- function(stationnement_communal, data_cat_terr,
                                       typology, selected_cog, selected_territory, current_year){

  ## Quick formatting
  stationnement_communal <- stationnement_communal |>
    rename(nb_stat = !!sym(paste0("NB_STAT_", current_year)),
           nb_stat_1000hab = !!sym(paste0("NB_STAT_1000HAB_", current_year))) |>
    select(INSEE_COM, nb_stat, nb_stat_1000hab)


  # --- BY MILIEU ---

  ## Retrieve territorial data and label communes categories
  data_cat_terr <- data_cat_terr |>
    mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                           CAT_COM %in% c(2, 3, 4) ~ 2,
                           CAT_COM %in% c(5, 6, 7) ~ 3),
           LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                              CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                              CAT_COM %in% c(5, 6, 7) ~ "Communes rurales"))

  stationnement_com <- data_cat_terr |>
    select(COM, EPCI, DEP, REG, LIBCAT) |>
    rename(category = LIBCAT) |>
    left_join(
      stationnement_communal |>
        rename(COM = INSEE_COM),
      by = "COM"
    ) |>
    mutate(across(where(is.numeric), ~replace_na(., 0)))


  ## Filter selected_territory if needed
  col_insee <- switch(typology,
                      region = "REG",
                      departement = "DEP",
                      epci = "EPCI")

  if (typology != "france"){
    stationnement_com <- stationnement_com |>
      filter(!!sym(col_insee) == selected_cog)
  }


  ## Compute indicators by milieu
  df_summary <- stationnement_com |>
    mutate(
      densite_classe = case_when(
        nb_stat_1000hab >= 0 & nb_stat_1000hab < 1   ~ "0 à 1",
        nb_stat_1000hab >= 1 & nb_stat_1000hab < 3   ~ "1 à 3",
        nb_stat_1000hab >= 3 & nb_stat_1000hab < 5   ~ "3 à 5",
        nb_stat_1000hab >= 5 & nb_stat_1000hab < 15  ~ "5 à 15",
        nb_stat_1000hab >= 15 & nb_stat_1000hab < 50 ~ "15 à 50",
        nb_stat_1000hab >= 50                        ~ "> 50",
        TRUE ~ NA_character_
      )
    ) |>
    # Communes count in each density class
    group_by(category, densite_classe) |>
    summarise(
      nb_communes = n(),
      .groups = "drop_last"
    ) |>
    # Convert into percentage
    mutate(
      total_cat = sum(nb_communes),
      pct_communes = 100 * nb_communes / total_cat
    ) |>
    ungroup() |>
    # Compute count of parking spots in each category
    left_join(
      stationnement_com |>
        group_by(category) |>
        summarise(nb_stat = sum(nb_stat, na.rm = TRUE)),
      by = "category"
    ) |>
    mutate(densite_classe = factor(densite_classe,
                                   levels = c("0 à 1", "1 à 3", "3 à 5", "5 à 15", "15 à 50", "> 50"))) |>
    arrange(category, densite_classe)


  ## Tooltip
  data_plot <- df_summary |>
    mutate(tooltip_density = paste0("Parmi les ", str_to_lower(category),
                                    " du territoire, <span style='font-weight:bold'>",
                                    format(round(nb_communes, 0), big.mark = " "), "</span> communes\ncomptent <span style='font-weight:bold'>",
                                    densite_classe, "</span> places de stationnement pour 1 000 habitants"),
           tooltip_nb_stat = paste0("Les ", str_to_lower(category),
                                    " du territoire comptent <span style='font-weight:bold'>",
                                    format(round(nb_stat, 0), big.mark = " "), "</span> places de stationnement")
    )


  # --- SELECTED TERRITORY ---

  ## Compute indicators
  df_territory <- stationnement_com |>
    mutate(category = paste0("<b>", selected_territory, "</b>")) |>
    mutate(
      densite_classe = case_when(
        nb_stat_1000hab >= 0 & nb_stat_1000hab < 1   ~ "0 à 1",
        nb_stat_1000hab >= 1 & nb_stat_1000hab < 3   ~ "1 à 3",
        nb_stat_1000hab >= 3 & nb_stat_1000hab < 5   ~ "3 à 5",
        nb_stat_1000hab >= 5 & nb_stat_1000hab < 15  ~ "5 à 15",
        nb_stat_1000hab >= 15 & nb_stat_1000hab < 50 ~ "15 à 50",
        nb_stat_1000hab >= 50                        ~ "> 50",
        TRUE ~ NA_character_
      )
    ) |>
    # Communes count in each density class
    group_by(category, densite_classe) |>
    summarise(
      nb_communes = n(),
      .groups = "drop_last"
    ) |>
    # Convert into percentage
    mutate(
      total_cat = sum(nb_communes),
      pct_communes = 100 * nb_communes / total_cat
    ) |>
    ungroup() |>
    mutate(nb_stat = NA_real_,
           densite_classe = factor(densite_classe,
                                   levels = c("0 à 1", "1 à 3", "3 à 5", "5 à 15", "15 à 50", "> 50"))) |>
    arrange(densite_classe)


  ## Tooltip
  df_territory <- df_territory |>
    mutate(tooltip_density = paste0("Parmi les communes du territoire, <span style='font-weight:bold'>",
                                    format(round(nb_communes, 0), big.mark = " "), "</span> communes\ncomptent <span style='font-weight:bold'>",
                                    densite_classe, "</span> places de stationnement pour 1 000 habitants"),
           tooltip_nb_stat = paste0("Les communes du territoire comptent <span style='font-weight:bold'>",
                                    format(round(nb_stat, 0), big.mark = " "), "</span> places de stationnement")
    )


  ## Merge
  data_plot <- rbind(data_plot, df_territory) |>
    mutate(category = factor(category,
                             levels = c(paste0("<b>", selected_territory, "</b>"), "Communes rurales",
                                        "Communes intermédiaires", "Grands centres urbains"))) |>
    arrange(category)


  # --- PLOT ---

  ## Custom palette
  custom_colors <- c(
    "0 à 1"   = "#E3E5F4",
    "1 à 3"   = "#B6B9E4",
    "3 à 5"   = "#898ED3",
    "5 à 15"  = "#5B64C3",
    "15 à 50" = "#303876",
    "> 50"    = "#1D2354"
  )


  ## Work on labels : width threshold and color
  data_plot <- data_plot |>
    mutate(
      n_digits = nchar(as.character(nb_communes)),
      seuil_adapt = case_when(
        n_digits <= 2 ~ 5,
        n_digits == 3 ~ 8.5,
        n_digits == 4 ~ 12,
        TRUE ~ 12
      ),
      label = case_when(
        pct_communes == 0 ~ "",
        pct_communes < seuil_adapt ~ "  ",
        TRUE ~ format(nb_communes, big.mark = " ", trim = TRUE)
      ),
      color_label = ifelse(densite_classe %in% c("0 à 1"), "grey60", "white")
    )


  ## STacked bar chart : % of communes in each density class
  g1 <- ggplot(data_plot, aes(x = category, y = pct_communes, fill = densite_classe, text = tooltip_density)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(
      aes(label = label, color = color_label),
      position = position_stack(vjust = 0.5),
      size = 3, show.legend = FALSE
    ) +
    coord_flip() +
    scale_y_continuous(labels = label_percent(scale = 1)) +
    scale_fill_manual(values = custom_colors) +
    scale_color_identity() +
    labs(x = NULL, y = NULL, fill = "Densité du stationnement<br>(places/1 000 hab)"
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(size = 10, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )


  ## Bar chart : Parking spots count
  df_nb_stat <- data_plot |>
    group_by(category) |>
    summarise(nb_stat = unique(nb_stat),
              tooltip_nb_stat = unique(tooltip_nb_stat)) |>
    mutate(label_x = nb_stat + 0.15 * max(data_plot$nb_stat, na.rm = TRUE))

  g2 <- ggplot(df_nb_stat, aes(x = category, y = nb_stat, text = tooltip_nb_stat)) +
    geom_col(fill = "#294754") +
    geom_text(
      aes(x = category, y = label_x, label = scales::comma(nb_stat, big.mark = " ")),
      size = 3, color = "#294754", hjust = 0
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = cut_short_scale()),
      expand = expansion(mult = c(0, 0.1))
    ) +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(size = 10, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )


  ## Combine the two plots side by side
  p1 <- ggplotly(g1, tooltip = "text")
  p2 <- ggplotly(g2, tooltip = "text")

  p_combined <- subplot(
    p1, p2,
    widths = c(0.6, 0.4),
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE
  ) |>
    layout(showlegend = FALSE,
           legend = list(
             orientation = "v",
             x = 1.1,
             xanchor = "left")
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png", filename = "plot_export")
    )
  # |>
  #   style(name = "0 à 1", traces = 1, showlegend = TRUE) |>
  #   style(name = "1 à 3", traces = 2, showlegend = TRUE) |>
  #   style(name = "3 à 5", traces = 3, showlegend = TRUE) |>
  #   style(name = "5 à 15", traces = 4, showlegend = TRUE) |>
  #   style(name = "15 à 50", traces = 5, showlegend = TRUE) |>
  #   style(name = "> 50", traces = 6, showlegend = TRUE) |>
  #   style(traces = c(7, 8, 9, 10, 11, 12), showlegend = FALSE)

  return(p_combined)
}


