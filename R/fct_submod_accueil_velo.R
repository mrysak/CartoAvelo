#' @title Generate a correspondence table for Accueil Vélo service types
#'
#' @description This function retrieves the full name of the different service types
#' from the abbreviations used in the columns of the source files.
#'
#' @return Dataframe with abbreviations as first column and full names as second column
#'
#' @noRd
presta_types_table <- function(){
  data.frame(
    short = c("TOTAL", "CAMP", "CHOT", "GITE", "HOTL", "AHEB", "REST", "LOIS", "OFTO", "LOUR"),
    long = c("Tout", "Campings", "Chambres d'hôte", "Gîtes et locations de meublés", "Hôtels",
             "Autres hébergements", "Restaurants et bars", "Loisirs et activités",
             "Offices de tourisme", "Loueurs et réparateurs de vélo")
  )
}




#' @title Plot the evolution of Accueil Vélo services over time
#'
#' @description This function generates a bar chart showing the evolution of the number of
#' Accueil Vélo services (total or by type depending on the user selection) over a given
#' time period. It includes a custom tooltip displaying the value and the year-on-year
#' evolution rate.
#'
#' @param data A data frame containing the columns of number of services per year,
#'   named as "NB_[selected_type]_[year]", and a unique row corresponding to the
#'   territory selected by the user.
#' @param selected_type A character string indicating the type of services to display.
#'   Must match the suffix used in column names (e.g., "TOTAL" or short codes present
#'   in `presta_types_table()`).
#' @param time_period A character vector specifying the years to include.
#' @param selected_territory A character string specifying the name of the selected territory
#'
#' @return A plotly interactive bar chart showing the evolution of the number of services,
#'   with tooltips displaying the value per year and the year-on-year evolution rate in percentage.
#'
#' @details Requires the helper function `presta_types_table()` to translate short codes to full type names.
#'
#' @import ggplot2
#' @import plotly
#' @importFrom dplyr select mutate filter left_join pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove str_to_lower
#'
#' @noRd
plot_evolution_nb_services <- function(data, selected_type, time_period, selected_territory){

  ## Display elements

  # Clean territory_name for title
  selected_territory_clean <- selected_territory

  if (grepl("Communauté de communes", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté de communes", "CC", selected_territory_clean)
  } else if (grepl("Communauté urbaine", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté urbaine", "CU", selected_territory_clean)
  } else if (grepl("Communauté d'agglomération", selected_territory_clean)) {
    selected_territory_clean <- sub("Communauté d'agglomération", "CA", selected_territory_clean)
  }

  # Custom title
  plot_title <- paste0(
    selected_territory_clean,
    "<span style='font-size:80%; font-weight:normal;'>",
    " - ",
    ifelse(selected_type == "TOTAL",
           "Prestations Accueil Vélo",
           presta_types_table() |> filter(short == selected_type) |> pull(long)),
    "</span>"
  )

  # Custom y label
  y_label <- paste0("Nombre de ",
                    ifelse(selected_type == "TOTAL",
                           "prestations Accueil Vélo",
                           str_to_lower(presta_types_table() |> filter(short == selected_type) |> pull(long))))


  ## Case when selected territory has no Accueil Vélo services : display message
  if(nrow(data) == 0){
    return(plot_message_no_data("Ce territoire ne dispose pas de prestations Accueil Vélo", 4.5,
                                plot_title, x_title = 0.06, y_title = 1.1, size_title = 20,
                                x_coord = 0.4, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0))
  }


  ## Select data to display and prepare it for plotting
  cols_to_display <- paste0("NB_", selected_type, "_", time_period)
  data_to_display <- data |>
    select(all_of(cols_to_display)) |>
    pivot_longer(
      cols = everything(),
      names_to = "year",
      values_to = "value"
    ) |>
    mutate(
      year = str_remove(year, paste0("NB_", selected_type, "_"))
    )


  ## Custom tooltip

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
      evolution_rate = (value - value_previous) / value_previous * 100,
      tooltip = ifelse(year == "2022",
                       paste0(ifelse(selected_type == "TOTAL",
                                     "Prestations Accueil Vélo",
                                     presta_types_table() |> filter(short == selected_type) |> pull(long)),
                              " en ", year, " : <span style='font-weight:bold'>", value, "</span>"),
                       paste0(ifelse(selected_type == "TOTAL",
                                     "Prestations Accueil Vélo",
                                     presta_types_table() |> filter(short == selected_type) |> pull(long)),
                              " en ", year, " : <span style='font-weight:bold'>", value, "</span>\n",
                              "Évolution par rapport à ", as.character(as.numeric(year) - 1), " : <span style='font-weight:bold'>",
                              ifelse(evolution_rate >= 0, "+", ""), format(round(evolution_rate, 1), decimal.mark = ","), "%</span>")
      )
    )


  ## ggplot chart
  p <- ggplot(data_to_display, aes(x = year, y = value, text = tooltip)) +
    geom_bar(stat = "identity", width = 0.4, fill = rvm_colour('raisin_black')) +
    labs(
      title = "",
      x = "",
      y = y_label
    ) +
    scale_y_continuous(labels = label_number(decimal.mark = ",", big.mark = " ")) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 12, b = 0, l = 20, r = 0),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = rvm_colour('raisin_black')),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(color = rvm_colour('raisin_black')),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 9)
    )


  ## Return plotly chart

  # Plotly chart with custom title
  g <- ggplotly(p, tooltip = "text") |>
    plotly::layout(
      title = list(
        text = plot_title,
        x = 0.06,
        y = 1.1,
        font = list(size = 20, color = rvm_colour('raisin_black'))
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




#' @title Plot Services per Itinerary as a stacked bar chart
#'
#' @description This function creates a horizontal stacked bar chart using ggplot2 and converts it
#' to an interactive plotly graph. It visualizes the counts of different categories of
#' services (Hébergements, Loisirs et activités, Restaurants et bars, Offices de tourisme,
#' Loueurs et réparateurs) per selected itinerary, with totals aligned vertically to the right.
#'
#' @param data_iti A data frame containing service counts per itinerary.
#' @param selected_iti A character or factor vector of itinerary identifiers (\code{NUM}) to include in the plot.
#' @param title A character string specifying the title of the plot.
#' @param year A numeric or character string specifying the most recent year with data.
#'
#' @return An interactive plotly object displaying a horizontal stacked bar chart with:
#' \itemize{
#'   \item Bars representing counts for each category by itinerary.
#'   \item Numeric labels inside each bar segment.
#'   \item Total counts displayed at the right end of each bar.
#'   \item Legend for service categories with custom colors.
#' }
#'
#' @details
#' The function first groups several accommodation-related columns into a single "Hébergements" category,
#' reshapes the data to a long format, and then generates a ggplot2 bar chart flipped horizontally.
#' The plot is converted to an interactive plotly plot with tooltips displaying category counts.
#'
#' @importFrom dplyr mutate filter select left_join group_by summarise case_when
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @import plotly
#'
#' @noRd
plot_services_per_itinerary <- function(data_iti, title, year){

  ## Display elements
  year <- as.character(year)
  plot_title <- ifelse(title == "",
                       "",
                       paste0(title,
                              "<span style='font-size:80%; font-weight:normal;'>",
                              " (si au moins 10)",
                              "</span>")
  )

  ## Case when selected territory has no Accueil Vélo services : display message
  if(nrow(data_iti) == 0){
    if(grepl("national", title)){
      label <- "Ce territoire ne dispose pas de prestations Accueil Vélo\nà proximité des véloroutes nationales"
      return(plot_message_no_data(label, 4, plot_title,
                                  x_title = 0.07, y_title = 0.978, size_title = 16,
                                  x_coord = 0.5, y_coord = 0.91, t = 0, b = 0, l = 0, r = 0))
    } else {
      label <- "Ce territoire ne dispose pas de prestations Accueil Vélo\nà proximité des véloroutes régionales"
      return(plot_message_no_data(label, 4, plot_title,
                                  x_title = 0.1, y_title = 0.978, size_title = 16,
                                  x_coord = 0.39, y_coord = 0.91, t = 0, b = 0, l = 20, r = 0))
    }
  }

  ## Build dynamic column names
  cols_heb <- paste0(c("NB_GITE_", "NB_HTL_", "NB_CHOT_", "NB_AHEB_", "NB_CAMP_"), year)
  col_lois <- paste0("NB_LOIS_", year)
  col_rest <- paste0("NB_REST_", year)
  col_ofto <- paste0("NB_OFTO_", year)
  col_lour <- paste0("NB_LOUR_", year)

  ## Group "Hébergements" columns
  data_iti <- data_iti %>%
    mutate(NB_HEB_YEAR = rowSums(across(all_of(cols_heb)), na.rm = TRUE))

  ## Prepare long dataframe with renamed categories
  df_long <- data_iti %>%
    select(ITI_NUM, NB_HEB_YEAR, !!col_lois, !!col_rest, !!col_ofto, !!col_lour) %>%
    rename(NB_LOIS_YEAR = !!col_lois,
           NB_REST_YEAR = !!col_rest,
           NB_OFTO_YEAR = !!col_ofto,
           NB_LOUR_YEAR = !!col_lour) %>%
    pivot_longer(cols = starts_with("NB_"),
                 names_to = "Categorie", values_to = "Count") %>%
    mutate(Categorie = case_when(
      Categorie == "NB_HEB_YEAR" ~ "Hébergements",
      Categorie == "NB_LOIS_YEAR" ~ "Loisirs et activités",
      Categorie == "NB_REST_YEAR" ~ "Restaurants et bars",
      Categorie == "NB_OFTO_YEAR" ~ "Offices de tourisme",
      Categorie == "NB_LOUR_YEAR" ~ "Loueurs et réparateurs",
      TRUE ~ Categorie
    ))

  ## Compute total for each itinerary and define their position
  totaux <- df_long %>%
    group_by(ITI_NUM) %>%
    summarise(Total = sum(Count), .groups = "drop")

  # Merge
  df_plot <- df_long %>%
    left_join(totaux, by = "ITI_NUM")

  # Order itineraries by decreasing total
  df_plot$ITI_NUM <- factor(df_plot$ITI_NUM, levels = totaux$ITI_NUM[order(totaux$Total, decreasing = FALSE)])

  # Compute maximul of totals (to find the right position)
  max_total <- max(totaux$Total)

  # Define a fixed position for totals so that they're vertically aligned
  totaux <- totaux %>%
    mutate(Total_position = max_total * 1.08)  # 8% further right than maximum

  # Define colours for plot
  palette <- c(
    'Hébergements' = "#303876",
    'Loisirs et activités' = "#979BBA",
    'Restaurants et bars' = "#C6CE41",
    'Offices de tourisme' = "#F3C460",
    'Loueurs et réparateurs' = "#E94F35"
  )

  # Height threshold for bar labels display
  df_threshold <- df_plot %>%
    group_by(ITI_NUM) %>%
    mutate(sum_presta = sum(Count, na.rm = TRUE))

  seuil <- max(df_threshold$sum_presta) * 0.029

  df_plot <- df_plot |>
    mutate(label = case_when(
      Count == 0 ~ "",
      Count < seuil ~ "  ",
      TRUE ~ as.character(Count)
    ))

  ## Filter if more than 8 itineraries
  top_iti <- df_plot |>
    distinct(ITI_NUM, Total) |>
    arrange(desc(Total)) |>
    slice_head(n = 8)

  df_plot <- df_plot |>
    filter(ITI_NUM %in% top_iti$ITI_NUM)

  totaux <- totaux |>
    filter(ITI_NUM %in% top_iti$ITI_NUM)

  ## ggplot chart
  p <- ggplot(df_plot, aes(y = Count, x = ITI_NUM, fill = Categorie, text = paste0(Categorie, ": <span style='font-weight:bold'>", Count, "</span>"))) +
    geom_bar(stat = "identity", position = "stack", width = 0.4) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              color = "white", size = 2.8) +
    geom_text(data = totaux, aes(y = Total_position, x = ITI_NUM, label = paste0("<b>", Total, "</b>")),
              inherit.aes = FALSE, hjust = 0, fontface = "bold", size = 4) +
    scale_fill_manual(values = palette) +
    labs(title = title, y = NULL, x = NULL, fill = "Catégorie") +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(legend.position = ifelse(title == "Prestations des véloroutes nationales",
                                   "none",
                                   "right"),
          legend.title = element_text(size = 12),
          legend.spacing.y = unit(100, "pt"),
          legend.text = element_text(size = 10),
          plot.margin = margin(t = 0, b = 0, l = 20, r = 0),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 12, color = rvm_colour('raisin_black'))) +
    coord_flip(clip = "off")


  ## Return plotly chart
  ggplotly(p, tooltip = "text") |>
    layout(title = list(text = plot_title,
                        x = 0,
                        y = 1.1,
                        font = list(size = 16, color = rvm_colour('raisin_black')))
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

}




#' @title Extract and rename prestation columns for a given year
#'
#' @description This function selects prestation columns for a specific year from a given dataframe,
#' renames them using their long labels from the `presta_types` mapping table,
#' and renames the "Tout" column to "Total" if present.
#'
#' @param df A dataframe containing prestation columns with names formatted as "NB_[short]_[year]".
#' @param year An integer specifying the target year of data to extract.
#' @param presta_types A dataframe mapping short to long prestation names.
#' @param libdep_col The name of the column containing department labels. Default is \code{"LIBDEP"}.
#'
#' @return A dataframe with renamed columns for the given year, and \"Tout\" renamed to \"Total\"
#'
#' @import dplyr
#' @noRd
extract_presta_cols <- function(df, year, presta_types, lib_col = "LIBDEP") {
  out <- df %>%
    select(all_of(lib_col), matches(paste0(year))) %>%
    rename_with(
      ~ presta_types$long[match(gsub(paste0("NB_|_", year), "", .x), presta_types$short)],
      matches(paste0(year))
    )

  # Rename "Tout" to "Total" if it exists
  names(out) <- sub("^Tout$", "Total", names(out))

  return(out)
}




#' @title Prepare national aggregated Accueil Vélo services data
#'
#' @description This function prepares a national-level table of Accueil Vélo services for a given year and computes
#' the evolution compared to the previous year. It selects only relevant columns, renames
#' them according to their long labels, aggregates the current and previous year data,
#' and calculates the percentage evolution.
#'
#' @param accueil_velo_national A dataframe containing national Accueil Vélo data, with columns
#' for each type of service and year (e.g. NB_HOTELS_2024, NB_RESTAURANTS_2024).
#' @param current_year An integer specifying the current year for which to extract data.
#' @param presta_types A dataframe containing at least two columns:
#' \describe{
#'   \item{short}{Short codes matching variable names in `accueil_velo_national` (e.g. "HOTELS").}
#'   \item{long}{Full descriptive names for display (e.g. "Hôtels").}
#' }
#'
#' @return A list with two elements:
#' \describe{
#'   \item{table_national}{A dataframe containing the total for the current year and
#'   the evolution (\%) compared to the previous year.}
#'   \item{national_evolution}{A dataframe containing only the evolution row.}
#' }
#'
#' @noRd
prepare_national_data <- function(accueil_velo_national, current_year, presta_types){

  current_year <- as.numeric(current_year)

  # Only keep relevant rows
  data_table_services_national <- accueil_velo_national %>%
    select(-matches("KMSNV|KMSRV")) %>%
    select(matches(paste0("NB_.*_", current_year - 1, "|NB_.*_", current_year)))

  # Rename presta types for current year
  national_current <- data_table_services_national %>%
    select(contains(as.character(current_year))) %>%
    rename_with(
      ~ presta_types$long[match(gsub(paste0("NB_|_", current_year), "", .x), presta_types$short)]
    ) %>%
    rename(Total = Tout)

  # Rename presta types for previous year
  national_previous <- data_table_services_national %>%
    select(contains(as.character(current_year - 1))) %>%
    rename_with(
      ~ presta_types$long[match(gsub(paste0("NB_|_", current_year - 1), "", .x), presta_types$short)]
    ) %>%
    rename(Total = Tout)

  # Compute evolution rate
  national_evolution <- (national_current / national_previous - 1) * 100
  national_evolution <- cbind(
    LIB = paste0("Evolution France entière /", current_year - 1),
    national_evolution
  )

  national_current <- cbind(
    LIB = "Total France entière",
    national_current
  )

  table_national <- rbind(national_current, national_evolution)

  return(list(table_national = table_national, national_evolution = national_evolution))

}




#' @title Prepare a territorial Accueil Vélo services table with current and evolution data
#'
#' @description This function prepares a table of Accueil Vélo services for a given year and computes
#' the evolution compared to the previous year. It selects only relevant columns, renames
#' them according to their long labels, aggregates the current and previous year data,
#' and calculates the percentage evolution.
#'
#' @param accueil_velo_data A dataframe containing Accueil Vélo data of the selected territory, with columns
#' for each type of service and year (e.g. NB_HOTELS_2024, NB_RESTAURANTS_2024).
#' @param accueil_velo_national A dataframe containing national Accueil Vélo data, with columns
#' for each type of service and year (e.g. NB_HOTELS_2024, NB_RESTAURANTS_2024).
#' @param data_cat_terr Dataframe containing territorial categorisation, including department codes, labels, region codes and labels.
#' @param selected_col Character. The name of the column used to filter the territory code (e.g., "INSEE_DEP" or "INSEE_EPCI").
#' @param join_col_terr Character. The name of the column in \code{data_cat_terr} used for the join (e.g., "DEP" or "EPCI").
#' @param lib_col Character. The name of the column containing the territorial label (e.g., "LIBDEP" or "LIBEPCI").
#' @param territory_code Character or numeric. The territorial code to filter (e.g., a department code or EPCI SIREN).
#' @param current_year Numeric. The reference year for extracting current and previous data.
#' @param presta_types A dataframe containing at least two columns:
#' \describe{
#'   \item{short}{Short codes matching variable names in `accueil_velo_national` (e.g. "HOTELS").}
#'   \item{long}{Full descriptive names for display (e.g. "Hôtels").}
#' }
#'
#' @return A data.frame containing three stacked rows:
#' \itemize{
#'   \item Total services for the territory in the current year.
#'   \item Evolution (%) compared to the previous year.
#'   \item National evolution data for comparison.
#' }
#' Empty dataframe if the selected territory has no Accueil Vélo services.
#'
#' @import dplyr
#' @importFrom rlang sym
#' @noRd
prepare_dep_epci_com_data <- function(accueil_velo_data, accueil_velo_national, data_cat_terr, selected_col, join_col_terr, lib_col, territory_code, current_year, presta_types) {

  current_year <- as.numeric(current_year)
  data_cat_terr <- data_cat_terr %>% mutate(across(all_of(join_col_terr), as.character))

  # Prepare territory data
  table_services <- accueil_velo_data %>%
    left_join(
      data_cat_terr %>% distinct(!!sym(join_col_terr), !!sym(lib_col)),
      by = setNames(join_col_terr, selected_col)
    ) %>%
    filter(.data[[selected_col]] == territory_code) %>%
    select(-matches("KMSNV|KMSRV")) %>%
    select(all_of(c(selected_col, lib_col)),
           matches(paste0("NB_.*_", current_year - 1, "|NB_.*_", current_year)))

  # Return empty dataframe if territory has no Accueil Vélo data
  if (nrow(table_services) == 0 || all(select(table_services, starts_with("NB_")) == 0, na.rm = TRUE)) {
    return(data.frame())
  }

  # Extract data for current and previous year
  current <- extract_presta_cols(table_services, current_year, presta_types, lib_col) %>%
    select(-all_of(lib_col))

  previous <- extract_presta_cols(table_services, current_year - 1, presta_types, lib_col) %>%
    select(-all_of(lib_col))

  # Compute evolution rate
  evolution <- (current / previous - 1) * 100
  evolution <- cbind(
    LIB = paste0("Evolution ", unique(table_services[[lib_col]]), " /", current_year - 1),
    evolution
  )

  # Merge
  current <- cbind(
    LIB = paste0("Total ", unique(table_services[[lib_col]])),
    current
  )

  # Clean territory_name for EPCI
  if (selected_col == "INSEE_EPCI"){
    if (grepl("Communauté de communes", current$LIB)) {
      current$LIB <- sub("Communauté de communes", "CC", current$LIB)
      evolution$LIB <- sub("Communauté de communes", "CC", evolution$LIB)
    } else if (grepl("Communauté urbaine", current$LIB)) {
      current$LIB <- sub("Communauté urbaine", "CU", current$LIB)
      evolution$LIB <- sub("Communauté urbaine", "CU", evolution$LIB)
    } else if (grepl("Communauté d'agglomération", current$LIB)) {
      current$LIB <- sub("Communauté d'agglomération", "CA", current$LIB)
      evolution$LIB <- sub("Communauté d'agglomération", "CA", evolution$LIB)
    }
  }

  # Prepare national data
  national_evolution <- prepare_national_data(accueil_velo_national, current_year, presta_types)$national_evolution

  # Merge
  final_table <- rbind(
    current,
    evolution,
    national_evolution
  )

  return(final_table)
}




#' @title Generate a table with the counts of Accueil Vélo services for a regional datavizualisation
#'
#' @description This function creates a comprehensive dataframe that includes:
#' \itemize{
#'   \item Department-level data for a selected region and year
#'   \item A regional total row (sum of all departments)
#'   \item An evolution row showing % change for the region (current year vs previous year)
#'   \item An evolution row showing % change for France as a whole (current year vs previous year)
#' }
#' The evolution rows are formatted as "+x%" or "-x%" based on their sign.
#' This dataframe serves as the basis for generating a dataviz at regional level
#'
#' @param accueil_velo_departemental Dataframe containing departmental level Accueil Vélo data with columns like "NB_[short]_[year]".
#' @param accueil_velo_national Dataframe containing national level Accueil Vélo data with similar columns.
#' @param data_cat_terr Dataframe containing territorial categorisation, including department codes, labels, region codes and labels.
#' @param presta_types A dataframe mapping prestation short codes to long labels, as in \code{presta_types_table()}.
#' @param current_year An integer indicating the current year of analysis.
#' @param selected_region An integer indicating the code of the selected region.
#'
#' @return A dataframe with:
#' \itemize{
#'   \item First column: department or aggregation labels
#'   \item Other columns: number of services by prestation type for departments and region total, and formatted % evolutions for region and national rows.
#' }
#'
#' @import dplyr
#' @import stringr
#' @noRd
prepare_regional_data <- function(accueil_velo_departemental,
                              accueil_velo_national,
                              data_cat_terr,
                              presta_types,
                              current_year,
                              selected_region) {

  current_year <- as.numeric(current_year)

  # Prepare departmental data
  data_table_services_departemental <- accueil_velo_departemental %>%
    left_join(data_cat_terr %>% distinct(DEP, LIBDEP, REG, LIBREG), by = c("INSEE_DEP" = "DEP")) %>%
    filter(REG == selected_region) %>%
    select(-matches("KMSNV|KMSRV")) %>%
    select(INSEE_DEP, LIBDEP, REG, LIBREG, matches(paste0("NB_.*_", current_year - 1, "|NB_.*_", current_year)))

  # Department data for current_year
  dept_data <- extract_presta_cols(data_table_services_departemental, current_year, presta_types)

  # Row for region total
  region_total <- dept_data %>%
    select(-LIBDEP) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
    mutate(LIB = paste0("Total ", unique(data_table_services_departemental$LIBREG))) %>%
    select(LIB, everything())

  # Row for regional evolution
  dept_previous <- extract_presta_cols(data_table_services_departemental, current_year - 1, presta_types)

  region_previous_total <- dept_previous %>%
    select(-LIBDEP) %>%
    summarise(across(everything(), sum, na.rm = TRUE))

  region_evolution <- (region_total[,-1] / region_previous_total - 1) * 100
  region_evolution <- cbind(
    LIB = paste0("Evolution ", unique(data_table_services_departemental$LIBREG), " /", current_year - 1),
    region_evolution
  )

  # Prepare national data
  national_evolution <- prepare_national_data(accueil_velo_national, current_year, presta_types)$national_evolution

  # Final table construction
  final_df <- bind_rows(
    dept_data %>% rename(LIB = LIBDEP) %>% arrange(desc(Total)),
    region_total,
    region_evolution,
    national_evolution
  )

  return(final_df)
}




#' @title Generate a formatted gt table for Accueil Vélo services with heatmap coloring
#'
#' @description This function generates a gt table with custom heatmap color scaling,
#' formatting, and styling, specifically designed to display Accueil Vélo services counts.
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
generate_services_count_table <- function(data, typology, color_low = "#F6F9ED", color_high = "#C6CE41", font_size = 15, font_family = "Poppins") {

  # Identify useful rows
  total_row <- which(stringr::str_detect(data$LIB, "^Total"))
  evolution_rows <- which(stringr::str_detect(data$LIB, "^Evolution"))

  # If territory has no data, print message instead of table
  no_data <- FALSE

  if (nrow(data) == 0) {
    no_data <- TRUE
  } else {
    if (length(total_row) == 1) {
      vals <- unlist(data[total_row, -1], use.names = FALSE)
      if (all(is.na(vals) | vals == 0)) {
        no_data <- TRUE
      }
    }
  }

  if (no_data) {
    return(
      gt::gt(data.frame(txt = "Ce territoire ne dispose pas de prestations Accueil Vélo")) %>%
        gt::cols_label(txt = "") %>%
        gt::cols_align(align = "left", columns = everything()) %>%
        gt::tab_options(
          column_labels.hidden = TRUE,
          table.border.top.color = "transparent",
          table.border.bottom.color = "transparent",
          heading.border.bottom.color = "transparent",
          table_body.border.top.color = "transparent",
          table_body.border.bottom.color = "transparent",
          table_body.hlines.color = "transparent",
          table_body.vlines.color = "transparent",
          summary_row.border.color = "transparent",
          grand_summary_row.border.color = "transparent",
          data_row.padding = gt::px(0),
          table.font.size = gt::px(17),
          table.font.names = "Poppins",
          table.font.color = "darkgrey"
        )
    )
  }

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
      rows = heatmap_rows,
      fn = scaler
    ) %>%

    # Alignment and widths
    cols_align(align = "center") %>%
    cols_width(
      LIB ~ px(170),
      `Autres hébergements` ~ px(100),
      everything() ~ px(93)
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

    # Format general rows as integers
    fmt_number(
      columns = cols_names,
      rows = setdiff(1:total_row, evolution_rows),
      decimals = 0,
      use_seps = TRUE,
      sep_mark = " "
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




#' @title Generate HTML labels for Accueil Vélo value indicators
#'
#' @description This function generates an HTML-formatted label displaying the value and its evolution for a given territory,
#' to be used in the leaflet map of Accueil Vélo services.
#'
#' @param value Numeric. The value to display (e.g. number of services).
#' @param label Numeric. The evolution value to display (e.g. % change from previous year).
#' @param total Numeric. The total value used to determine NA cases (e.g. total number of services or km of routes).
#'
#' @return A character string containing HTML code to display in leaflet labels.
#'
#' @details This function distinguishes between NA territories (no services) and valid data. It uses external variables:
#' - `info_to_display`: value or evolution display mode
#' - `selected_type`: type of prestation selected
#' - `presta_types`: dataframe linking short and long labels of prestations
#' - `current_year`: current year for display
#'
#' @seealso [label_function_ratio()] for ratio indicators.
#'
#' @noRd
label_function_value <- function(value, label, total,
                                 info_to_display, selected_type, presta_types, current_year){
  is_na <- is.na(total)

  legend <- ifelse(
    is_na,
    "Ce territoire ne dispose pas<br/>de prestations Accueil Vélo",
    if (info_to_display == "value") {
      paste0(
        ifelse(selected_type == "TOTAL",
               "Prestations Accueil Vélo",
               presta_types |> filter(short == selected_type) |> pull(long)),
        " :<span style='font-weight:bold'> ", value, "</span><br/>",
        ifelse(is.na(label) | is.infinite(label),
               "Evolution : -",
               ifelse(label >= 0,
                      paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> +",  format(round(label, 1), decimal.mark = ","), "%</span>"),
                      paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> ",  format(round(label, 1), decimal.mark = ","), "%</span>")))

      )
    } else if (info_to_display == "evol") {
      paste0(
        ifelse(selected_type == "TOTAL",
               "Prestations Accueil Vélo",
               presta_types |> filter(short == selected_type) |> pull(long)),
        " :<span style='font-weight:bold'> ", label, "</span><br/>",
        ifelse(is.na(value) | is.infinite(value),
               "Evolution : -",
               ifelse(value >= 0,
                      paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> +",  format(round(value, 1), decimal.mark = ","), "%</span>"),
                      paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> ",  format(round(value, 1), decimal.mark = ","), "%</span>")))
      )
    }
  )

  return(legend)
}




#' @title Generate HTML labels for Accueil Vélo ratio indicators
#'
#' @description This function generates an HTML-formatted label displaying the ratio (services per km of cycle route)
#' and/or its evolution for a given territory, to be used in the leaflet map of Accueil Vélo services.
#'
#' @param value Numeric. The ratio value to display (e.g. services per km).
#' @param label Numeric. The evolution value to display (e.g. % change from previous year).
#' @param total Numeric. The total value used to determine NA cases (e.g. total number of services or km of routes).
#'
#' @return A character string containing HTML code to display in leaflet labels.
#'
#' @details This function distinguishes between NA territories (no services or no routes) and valid data. It uses external variables:
#' - `info_to_display`: value or evolution display mode
#' - `selected_type`: type of prestation selected
#' - `presta_types`: dataframe linking short and long labels of prestations
#' - `cycle_route_type`: type of cycle route considered
#' - `current_year`: current year for display
#'
#' @seealso [label_function_value()] for value indicators.
#'
#' @noRd
label_function_ratio <- function(value, label, total,
                                 info_to_display, selected_type, presta_types, current_year, cycle_route_type){
  is_na <- is.na(total)

  legend <- ifelse(
    is_na,
    "Ce territoire ne dispose pas<br/>de prestations Accueil Vélo",
    if (info_to_display == "value") {
      ifelse(is.na(value),
             "Ce territoire ne possède pas de véloroute <br/>inscrite à un schéma régional",
             paste0(ifelse(selected_type == "TOTAL",
                           paste0("Prestations Accueil Vélo<br/>par km de véloroute (", cycle_route_type, ")"),
                           paste0(presta_types |> filter(short == selected_type) |> pull(long), "<br/>par km de véloroute (", cycle_route_type, ")")),
                    " :<span style='font-weight:bold'> ", format(round(value, 2), decimal.mark = ","), "</span><br/>",
                    ifelse(is.na(label) | is.infinite(label),
                           "Evolution : -",
                           ifelse(label >= 0,
                                  paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> +",  format(round(label, 1), decimal.mark = ","), "%</span>"),
                                  paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> ",  format(round(label, 1), decimal.mark = ","), "%</span>")))
             )
      )
    } else if (info_to_display == "evol") {
      ifelse(is.na(label),
             "Ce territoire ne possède pas de véloroute <br/>inscrite à un schéma régional",
             paste0(ifelse(selected_type == "TOTAL",
                           paste0("Prestations Accueil Vélo<br/>par km de véloroute (", cycle_route_type, ")"),
                           paste0(presta_types |> filter(short == selected_type) |> pull(long), "<br/>par km de véloroute (", cycle_route_type, ")")),
                    " :<span style='font-weight:bold'> ", format(round(label, 2), decimal.mark = ","), "</span><br/>",
                    ifelse(is.na(value) | is.infinite(value),
                           "Evolution : -",
                           ifelse(value >= 0,
                                  paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> +", format(round(value, 1), decimal.mark = ","), "%</span>"),
                                  paste0("Evolution /", as.numeric(current_year)-1, " :<span style='font-weight:bold'> ",  format(round(value, 1), decimal.mark = ","), "%</span>")))
             )
      )
    }
  )

  return(legend)
}




#' @title Generate an interactive map of Accueil Vélo services
#'
#' @description This function returns an interactive leaflet map displaying either
#' the national or regional distribution of Accueil Vélo services. It can display
#' both absolute counts and ratios per km of cycle routes, along with their evolution
#' from the previous year.
#'
#' @param data_cat_terr A dataframe containing territorial categorisation information for French administrative units.
#' @param accueil_velo_data A dataframe containing Accueil Vélo services data.
#' @param admin_sf An sf object containing the administrative boundaries to display on the map.
#' @param insee_col Character. The name of the column containing INSEE codes to join data.
#' @param current_year Numeric. The latest year with data.
#' @param presta_types A dataframe containing the types of Accueil Vélo services with `short` and `long` labels.
#' @param tab_typology Character. Typology of the displayed territory ("france" for national or "region" for regional maps).
#' @param selected_region Character. INSEE code of the selected region when `tab_typology` is "region".
#' @param selected_type Character. Type of Acceuil Vélo services selected by the user.
#' @param indicator_choice Character. User choice of indicator to display ("count" for absolute number of services, "ratio" for services per km of cycle route).
#' @param info_to_display Character. Whether to display the value itself ("value") or its evolution from the previous year ("evol").
#' @param cycle_route_type Character. When `indicator_choice` is "ratio", specifies the type of cycle route included ("SNV" or "SRV").
#' @param display_typology Character. Administrative scale used for display. One of `"departement"` or `"region"`.
#'
#' @return A leaflet map displaying Accueil Vélo services with polygons coloured according to the selected indicator and value type. The map includes interactive labels and a legend.
#'
#' @import sf
#' @import leaflet
#' @import dplyr
#' @import readr
#' @import stringr
#' @import leaflet.extras2
#' @importFrom htmltools HTML
#' @noRd
map_services_count <- function(data_cat_terr, accueil_velo_data, admin_sf, insee_col,
                               current_year, presta_types, tab_typology, selected_region = "", selected_type,
                               indicator_choice, info_to_display, cycle_route_type = "",
                               display_typology, ns){

  # ---- JOIN DATA ----

  admin_sf <- admin_sf %>%
    left_join(accueil_velo_data %>% mutate(across(all_of(insee_col), as.character)),
              by = insee_col)

  # Filter by region if needed
  if (tab_typology == "region") {
    list_dep <- data_cat_terr %>%
      filter(REG == selected_region) %>%
      pull(DEP) %>%
      unique()
    admin_sf <- admin_sf %>% filter(INSEE_DEP %in% list_dep)
  }

  # ---- COMPUTE EVOLUTION ----

  selected_col <- paste0("NB_", selected_type,
                         ifelse(indicator_choice == "ratio", "_KM", ""),
                         ifelse(indicator_choice == "ratio", cycle_route_type, ""),
                         "_", current_year)

  previous_col <- paste0("NB_", selected_type,
                         ifelse(indicator_choice == "ratio", "_KM", ""),
                         ifelse(indicator_choice == "ratio", cycle_route_type, ""),
                         "_", current_year - 1)

  admin_sf <- admin_sf %>%
    mutate(EVOL = (get(selected_col) - get(previous_col)) / get(previous_col) * 100)

  # ---- RENAME COLUMNS ----

  admin_sf <- admin_sf %>%
    mutate(total = .[[paste0("NB_TOTAL_", current_year)]],
           territory_name = .[[grep("^NOM", names(.), value = TRUE)[1]]]) %>%
    rename(!!ifelse(info_to_display == "value", "value", "label") := all_of(selected_col),
           !!ifelse(info_to_display == "value", "label", "value") := EVOL)

  # ---- COLOR PALETTE AND LEGEND ----

  if (info_to_display == "evol" & display_typology == "region") {

    color_palette_legend <- color_palette_map_evol(-10, 30,
                                                   c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                   c("< -10%", "-10% – -5%", "-5% – 0%", "0% – 15%", "15% – 30%", "> 30%"),
                                                   "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                   admin_sf)

    pal <- color_palette_legend$pal
    colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (info_to_display == "evol" & display_typology == "departement"){

    color_palette_legend <- color_palette_map_evol(-30, 40,
                                                   c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                   c("< -30%", "-30% – -15%", "-15% – 0%", "0% – 20%", "10% – 40%", "> 40%"),
                                                   "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                   admin_sf)

    pal <- color_palette_legend$pal
    colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else if (indicator_choice == "ratio" & info_to_display == "value" & display_typology == "departement" & selected_type == "TOTAL"){

    color_palette_legend <- color_palette_map_value(0.6,
                                                    c("#e8f3f7", "#d0e6ef", "#b9dae7", "#9fc1cd"),
                                                    c("0 – 0,2", "0,2 – 0,4", "0,4 – 0,6%", "> 0,6"),
                                                    "#e8f3f7", "#9fc1cd",
                                                    admin_sf)

    pal <- color_palette_legend$pal
    colors <- color_palette_legend$legend_colors
    legend_labels <- color_palette_legend$legend_labels

  } else {

    colors_pal <- list(
      count = c(low = "#f4f5d9", high = "#9ea534"),
      ratio = c(low = "#e8f3f7", high = "#9fc1cd")
    )

    pal <- colorNumeric(
      palette = colorRampPalette(colors_pal[[indicator_choice]])(100),
      domain = admin_sf$value[is.finite(admin_sf$value)],
      na.color = "#d3d3d3"
    )

    # Legend labels
    finite_values <- admin_sf$value[is.finite(admin_sf$value)]

    if(length(finite_values) == 0){
      breaks <- NA
      midpoints <- NA
      colors <- "#d3d3d3" # To indicate missing data
      legend_labels <- "NA"
    } else {
      breaks <- pretty(finite_values, n = 5)

      if(length(breaks) < 2){
        breaks <- c(min(finite_values), max(finite_values))
      }

      midpoints <- pmin(pmax((breaks[-length(breaks)] + breaks[-1]) / 2, min(finite_values)), max(finite_values))
      colors <- pal(midpoints)

      legend_labels <- paste0(
        format(breaks[-length(breaks)], decimal.mark = ","),
        ifelse(info_to_display == "evol", "% – ", " – "),
        format(breaks[-1], decimal.mark = ","),
        ifelse(info_to_display == "evol", "%", "")
      )
    }
  }


  # ---- LEGEND TITLE ----

  label_value <- presta_types %>% filter(short == selected_type) %>% pull(long)
  label_value <- ifelse(length(label_value) == 0, selected_type, str_to_lower(label_value))

  legend_title <- case_when(
    info_to_display == "value" & indicator_choice == "count" ~
      ifelse(selected_type == "TOTAL", "Nombre de prestations Accueil Vélo", paste0("Nombre de ", label_value)),
    info_to_display == "evol" & indicator_choice == "count" ~
      paste0("Evolution du nombre de ", ifelse(selected_type == "TOTAL", "prestations<br/>Accueil Vélo ", paste0(label_value, "<br/>")), "par rapport à ", current_year - 1),
    info_to_display == "value" & indicator_choice == "ratio" ~
      ifelse(selected_type == "TOTAL", paste0("Nombre de prestations Accueil Vélo <br/> par km de véloroute (", cycle_route_type, ")"),
             paste0("Nombre de ", label_value, "<br/>par km de véloroute (", cycle_route_type, ")")),
    info_to_display == "evol" & indicator_choice == "ratio" ~
      paste0("Evolution du nombre de ", ifelse(selected_type == "TOTAL", paste0("prestations<br/>Accueil Vélo par km de véloroute <br/>(", cycle_route_type, ") "), paste0("<br/>", label_value, " par km de véloroute <br/>(", cycle_route_type, ") ")), "par rapport à ", current_year - 1),
    TRUE ~ "Légende"
  )

  # ---- MAP CENTERING ----

  if(tab_typology == "france"){
    lng <- 2.30; lat <- 46.50; zoom <- 6.0
  } else {
    centroids <- get_centroids(selected_region, "region")
    lng <- centroids$lng; lat <- centroids$lat;
    if (selected_region %in% c("75")){
      zoom <- 7
    } else if (selected_region %in% c("76", "84", "03", "94", "44", "27")){
      zoom <- 7.5
    } else if (selected_region %in% c("01", "02", "04", "06", "11")){
      zoom <- 9
    } else {zoom <- 8}
  }

  # ---- LEAFLET MAP ----

  map <- leaflet(admin_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    setView(lng = lng, lat = lat, zoom = zoom) %>%

    draw_territory_outline(
      selected_territory = selected_region,
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

      label = ~paste0(
        "<div style='margin-bottom:2px;'>",
        "<b><span style='font-size:1em;'>", territory_name, "</span></b></div>",
        "<span style='font-size:0.9em;'>",
        if (indicator_choice == "count") {
          label_function_value(value, label, total,
                               info_to_display, selected_type, presta_types, current_year)
        } else if (indicator_choice == "ratio") {
          label_function_ratio(value, label, total,
                               info_to_display, selected_type, presta_types, current_year, cycle_route_type)
        }
      ) %>%

        lapply(htmltools::HTML),

      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "14px",direction = "auto")
    ) %>%

    addLegend(
      colors = colors,
      labels = legend_labels,
      opacity = 0.7,
      title = legend_title,
      position = "bottomleft",
      labFormat = labelFormat(big.mark = "")) %>%

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carte_accueil_velo",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  # Add select input to set the view on a DROM
  select_width <- case_when(
    indicator_choice == "count" & info_to_display == "value" ~ 250,
    indicator_choice == "count" & info_to_display == "evol" ~ 250,
    indicator_choice == "ratio" & info_to_display == "value" ~ 250,
    indicator_choice == "ratio" & info_to_display == "evol" ~ 250,)

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




#' @title Create a Leaflet Map Showing Accueil Vélo services density per km²
#'
#' @description This function generates an interactive Leaflet map visualizing the density of selected services
#' per square kilometer in hexagonal zones within a 5 or 10 km radius around French cycle routes
#' The map zoom and center adapt dynamically depending on the chosen territorial level (France, region, or department).
#'
#' @param density_sf A `sf` object containing spatial polygons with columns for service counts and surface area.
#'   It must include columns named like `"NB_<selected_type>_<current_year>"` representing counts for the selected service type and year.
#' @param admin_sf An sf object containing the administrative boundaries to display on the map.
#' @param tab_typology Character. Typology of the displayed territory ("france" for national, "region" for regional or "departement" for departemental maps).
#' @param selected_territory A character string representing the selected region or department code (INSEE codes).
#'   Ignored if `tab_typology == "france"`.
#' @param selected_type A character string indicating the selected service type (e.g., `"TOTAL"`, or other service short codes).
#' @param current_year Numeric. The latest year with data.
#' @param presta_types A dataframe containing the types of Accueil Vélo services with `short` and `long` labels.
#'
#' @return A Leaflet map object (`leaflet` class) showing service density with polygons colored by density categories.
#'   Includes tooltips with detailed density information and a legend.
#'
#' @import dplyr
#' @import sf
#' @import leaflet
#' @importFrom rlang sym
#' @importFrom htmltools HTML
#' @importFrom stringr str_to_lower
#' @importFrom stats quantile
#' @noRd
map_services_density <- function(density_sf, admin_sf, tab_typology,
                                 selected_territory, selected_type,
                                 current_year, presta_types){

  ## Zoom and map centering

  if(tab_typology == "france"){
    lng <- 2.30; lat <- 46.50; zoom <- 6
  } else if (tab_typology == "region") {
    centroids <- get_centroids(selected_territory, "region")
    lng <- centroids$lng
    lat <- centroids$lat;
    if (selected_territory %in% c("75")){
      zoom <- 7
    } else if (selected_territory %in% c("76", "84", "03", "94", "44", "27")){
      zoom <- 7.5
    } else if (selected_territory %in% c("01", "02", "04", "06", "11")){
      zoom <- 9
    } else {zoom <- 8}
  } else if (tab_typology == "departement") {
    centroids <- get_centroids(selected_territory, "departement")
    lng <- centroids$lng; lat <- centroids$lat;
    if (selected_territory %in% c("6AE", "26", "20R", "33")){
      zoom <- 8.5
    } else if (selected_territory %in% c("973")){
      zoom <- 8
    } else {zoom <- 9}
  }


  ## Compute density per km² for the selected service

  selected_col <- paste0("NB_", selected_type, "_", current_year)
  density_sf <- density_sf %>%
    mutate(density = !!sym(selected_col) / SURFACE)


  ## Handle cases with no data

  if (nrow(density_sf) == 0 || all(is.na(density_sf$density))) {

    nom_col <- names(admin_sf)[grepl("^NOM", names(admin_sf))]

    map <- leaflet(options = leafletOptions(minZoom = 4), height = 700) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lng = lng, lat = lat, zoom = zoom) %>%
      addPolygons(
        data = admin_sf,
        color = "#666",
        weight = 1,
        fill = "#d3d3d3",
        opacity = 0.9,
        label = ~paste0(
          "<strong>", admin_sf[[nom_col]], "</strong>",
          "<br/>Ce territoire ne dispose pas de ",
          ifelse(selected_type == "TOTAL",
                 "prestations Accueil Vélo",
                 str_to_lower(presta_types |> filter(short == selected_type) |> pull(long)))
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px", direction = "auto")
      )

    return(map)

  } else {

    ## Cases with data

    # Replace NAs with 0s
    density_sf <- density_sf %>%
      mutate(density = replace(density, is.na(density), 0))

    # Create labels and color palette
    if (tab_typology == "france"){
      percentile_95 <- quantile(density_sf$density, 0.95, na.rm = TRUE)

      breaks_raw <- c(seq(0, percentile_95, length.out = 5), Inf)
      breaks <- round(breaks_raw, 3)
      breaks <- unique(breaks)

      labels <- c(
        paste0(format(breaks[1], decimal.mark = ",", nsmall = 2), " - ", format(breaks[2], decimal.mark = ",", nsmall = 2)),
        paste0(format(breaks[2], decimal.mark = ",", nsmall = 2), " - ", format(breaks[3], decimal.mark = ",", nsmall = 2)),
        paste0(format(breaks[3], decimal.mark = ",", nsmall = 2), " - ", format(breaks[4], decimal.mark = ",", nsmall = 2)),
        paste0(format(breaks[4], decimal.mark = ",", nsmall = 2), " - ", format(breaks[5], decimal.mark = ",", nsmall = 2)),
        paste0("> ", format(breaks[5], decimal.mark = ",", nsmall = 2))
      )

      density_sf <- density_sf %>%
        mutate(density_cat = cut(density,
                                 breaks = breaks,
                                 labels = labels,
                                 include.lowest = TRUE,
                                 right = FALSE))

      palette_colors <- c("#d2d0e2", "#a7a3c4", "#7c79a6", "#54558c", "#303876")
      names(palette_colors) <- labels
      palette_colors <- as.list(palette_colors)

    } else {

      if(all(density_sf$density == 0, na.rm = TRUE)){

        labels <- c("0")
        density_sf <- density_sf %>%
          mutate(density_cat = "0")
        palette_colors <- c("#d2d0e2")
        names(palette_colors) <- labels
        palette_colors <- as.list(palette_colors)

      } else{

        percentile_99 <- quantile(density_sf$density, 0.99, na.rm = TRUE)

        if (percentile_99 == 0){
          percentile_99 <- max(density_sf$density, na.rm = TRUE)
        }

        breaks_raw <- c(seq(0, percentile_99, length.out = 4), Inf)
        breaks <- round(breaks_raw, 3)
        breaks <- unique(breaks)

        labels <- c(
          paste0(format(breaks[1], decimal.mark = ",", nsmall = 2), " - ", format(breaks[2], decimal.mark = ",", nsmall = 2)),
          paste0(format(breaks[2], decimal.mark = ",", nsmall = 2), " - ", format(breaks[3], decimal.mark = ",", nsmall = 2)),
          paste0(format(breaks[3], decimal.mark = ",", nsmall = 2), " - ", format(breaks[4], decimal.mark = ",", nsmall = 2)),
          paste0("> ", format(breaks[4], decimal.mark = ",", nsmall = 2))
        )

        density_sf <- density_sf %>%
          mutate(density_cat = cut(density,
                                   breaks = breaks,
                                   labels = labels,
                                   include.lowest = TRUE,
                                   right = FALSE))

        palette_colors <- c("#d2d0e2", "#7c79a6", "#54558c", "#303876")
        names(palette_colors) <- labels
        palette_colors <- as.list(palette_colors)}
    }


    # Leaflet map

    map <- leaflet(density_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) %>%

      addProviderTiles("CartoDB.PositronNoLabels") %>%

      setView(lng = lng, lat = lat, zoom = zoom) %>%

      addPolygons(
        fillColor = ~ifelse(
          is.na(density_cat),
          "#d3d3d3",
          palette_colors[as.character(density_cat)]
        ),

        weight = 0.5,
        opacity = 1,
        color = "white",
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),

        label = ~paste0(
          "Densité de ",
          ifelse(selected_type == "TOTAL",
                 "prestations Accueil <br/>Vélo",
                 paste0(str_to_lower(presta_types |> filter(short == selected_type) |> pull(long)), "<br/>")),
          " dans cette zone : <strong>", format(round(density, 2), decimal.mark = ","), "/km²</strong>")
        %>%

          lapply(htmltools::HTML),

        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px",direction = "auto")
      ) %>%

      {if (tab_typology != "france") {
        addPolygons(.,
                    data = admin_sf,
                    color = "#666",
                    weight = 1,
                    fill = NA,
                    opacity = 0.8
        )
      } else {.}
      } %>%

      addLegend(
        colors = palette_colors,
        labels = names(palette_colors),
        opacity = 0.7,
        title = paste0(ifelse(selected_type == "TOTAL",
                              "Prestations Accueil Vélo",
                              presta_types |> filter(short == selected_type) |> pull(long)),
                       " par km²"),
        position = "bottomleft"
      )%>%

      addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                              exportOnly = TRUE, filename = "carte_densite_accueil_velo",
                                              hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                              defaultSizeTitles = list(
                                                "A4Portrait" = "Télécharger en A4 Portrait",
                                                "A4Landscape" = "Télécharger en A4 Paysage",
                                                "CurrentSize" = "Télécharger à la taille actuelle"
                                              )))
    return(map)
  }

}



#' @title Generate value box content for Accueil Velo services
#'
#' @description This function prepares formatted textual and numerical content
#' to be displayed inside a **Shiny value box**.
#'
#' @param accueil_velo_data A data frame containing annual values of the indicator for the selected territory (prefiltered).
#' @param presta_types_table A dataframe mapping short to long prestation names.
#' @param typology A character string specifying the typology of the selected territory.
#' @param current_year An integer indicating the current year of analysis.
#' @param indicator_choice A string specifying the indicator. Must be one of: `"count"`, `"ratio"`, or `"density"`.
#' @param selected_type A character string indicating the type of services to display.
#'   Must match the suffix used in column names (e.g., "TOTAL" or short codes present
#'   in `presta_types_table()`).
#' @param cycle_route_type Character. When `indicator_choice` is "ratio", specifies the type of cycle route included ("SNV" or "SRV").
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
value_box_content_accueil_velo <- function(accueil_velo_data, presta_types_table, typology, current_year,
                                           indicator_choice, selected_type, cycle_route_type){

  if (nrow(accueil_velo_data) == 0){
    value <- "-"
    evol <- ""

    if (indicator_choice %in% c("count", "density")){
      title <- ifelse(selected_type == "TOTAL", "Total prestations", presta_types_table$long[match(selected_type, presta_types_table$short)])
    } else if (indicator_choice == "ratio"){
      title <- ifelse(selected_type == "TOTAL",
                      paste0("Prestations par km de ", cycle_route_type),
                      paste0(presta_types_table$long[match(selected_type, presta_types_table$short)], " par km de ", cycle_route_type))
    }
  } else if (indicator_choice %in% c("count", "density")){

    if (indicator_choice == "density"){selected_type <- "TOTAL"}

    value_current <- accueil_velo_data[[paste0("NB_", selected_type, "_", current_year)]]
    value_previous <- accueil_velo_data[[paste0("NB_", selected_type, "_", as.numeric(current_year) - 1)]]

    evol <- 100 * (value_current - value_previous) / value_previous

    title <- ifelse(selected_type == "TOTAL", "Total prestations", presta_types_table$long[match(selected_type, presta_types_table$short)])

    if (length(value_current) == 0 | is.na(value_current) | nrow(accueil_velo_data) == 0){
      value <- "-"
    } else {
      value <- format(round(value_current, 0), big.mark = " ", trim = TRUE)
    }

    if (length(evol) == 0 | is.na(evol) | nrow(accueil_velo_data) == 0){
      evol <- "-"
    } else {
      evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 1), decimal.mark = ",", big.mark = " ", trim = TRUE), "% par rapport à ", as.numeric(current_year) - 1)
    }

  } else if (indicator_choice == "ratio"){

    if (indicator_choice == "density"){selected_type <- "TOTAL"}

    value_current <- accueil_velo_data[[paste0("NB_", selected_type, "_KM", cycle_route_type, "_", current_year)]]
    value_previous <- accueil_velo_data[[paste0("NB_", selected_type, "_KM", cycle_route_type, "_", as.numeric(current_year) - 1)]]

    evol <- 100 * (value_current - value_previous) / value_previous

    title <- ifelse(selected_type == "TOTAL",
                    paste0("Prestations par km de ", cycle_route_type),
                    paste0(presta_types_table$long[match(selected_type, presta_types_table$short)], " par km de ", cycle_route_type))

    if (length(value_current) == 0 | is.na(value_current) | nrow(accueil_velo_data) == 0){
      value <- "-"
    } else {
      value <- format(round(value_current, 2), decimal.mark = ",", big.mark = " ", trim = TRUE)
    }

    if (length(evol) == 0 | is.na(evol) | nrow(accueil_velo_data) == 0){
      evol <- ""
    } else {
      evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 1), decimal.mark = ",", big.mark = " ", trim = TRUE), "% par rapport à ", as.numeric(current_year) - 1)
    }
  }

  return(list(title = title, value = value, evol = evol))
}
