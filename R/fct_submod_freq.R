#' @title Plot Global Bicycle Traffic Evolution
#'
#' @description This function creates an interactive `plotly` chart showing the
#' evolution of bicycle traffic relative to a reference year (`start_year`).
#' Depending on the display typology (`france` or `region`), it displays either
#' the national-level evolution or the evolution within a specific region.
#'
#' @param data A `data.frame` containing bicycle traffic evolution data.
#'   It must include columns:
#'   - `milieu_region`: typology of the territory (France, Grands centres urbains, etc.)
#'   - `nb_compteurs`: number of counters used in the dataset
#'   - `evo_xx_xx`: yearly evolution values compared to the reference year.
#' @param start_year A `numeric` value. The baseline year against which
#'   bicycle traffic evolution is computed.
#' @param current_year A `numeric` value. The latest year to display in the chart.
#' @param tab_typology A `character` string indicating the typology of the view.
#'   Must be either `"france"` (national chart with multiple typologies)
#'   or `"region"` (chart for a single region).
#' @param selected_cog A `character` string or `numeric` value. The INSEE region code
#'   if `tab_typology = "region"`. Not used otherwise.
#' @param region_name A `character` string. The human-readable region name if
#'   `tab_typology = "region"`. Not used otherwise.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom stringr str_to_lower
#' @importFrom scales percent_format
#' @importFrom plotly ggplotly add_annotations layout
#' @noRd
plot_global_evolution <- function(data, start_year, current_year,
                                  tab_typology, selected_cog = NULL, region_name = NULL){

  # Case when selected territory has no bike counters : display message
  if(nrow(data) == 0){
    return(plot_message_no_data("Aucune donnée de comptage vélo partagée pour ce territoire", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.5, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0))
  }

  # Format dataframe for display
  df_long <- data  |>
    pivot_longer(cols = starts_with("evo_"), names_to = "year", values_to = "evolution") |>
    mutate(year = 2000 + as.integer(sub("evo_19_", "", year))) |>
    bind_rows(data |> transmute(milieu_region, nb_compteurs, year = start_year, evolution = 0)) |>
    arrange(milieu_region, year)

  if (tab_typology == "france"){
    df_long <- df_long |>
      mutate(milieu_region = factor(milieu_region, levels = c("France", "Grands centres urbains", "Communes intermédiaires", "Communes rurales")))

    territory_name <- "France"

    # Custom color palette
    colors <- c(
      "France" = "#303876",
      "Communes intermédiaires" = "#9185BE",
      "Communes rurales" = "#C6CE41",
      "Grands centres urbains" = "#E94F35"
    )


  } else if (tab_typology == "region"){
    territory_name <- region_name

    # Custom color palette
    colors <- setNames("#303876", region_name)
  }


  # Labels for latest year
  df_labels <- df_long |> filter(year == current_year) |>
    mutate(label = ifelse(evolution >= 0,
                          paste0("+", round(evolution*100), "%"),
                          paste0(round(evolution*100), "%")))


  # Tooltip
  df_long <- df_long |>
    mutate(
      tooltip = ifelse(year == start_year, "",
                       paste0(
                         "Évolution de la fréquentation vélo globale depuis ", start_year,
                         ifelse(milieu_region == territory_name,
                                paste0("\nen ", territory_name),
                                paste0("\ndans les ", str_to_lower(milieu_region))),
                         " : <span style='font-weight:bold'>",
                         ifelse(evolution >= 0, "+", ""),
                         format(round(evolution*100, 1), decimal.mark = ","),
                         "%</span>",
                         "<br><span style='font-size:10px;'>À échantillon comparable, base ", nb_compteurs, " compteurs</span>"
                       ))
    )


  # ggplot chart
  p <- ggplot(df_long, aes(x = year, y = evolution, color = milieu_region, group = milieu_region)) +
    #geom_line(size = 1) +
    geom_smooth(se = FALSE, method = "loess", span = 0.8) +
    geom_point(aes(text = tooltip), size = 2) +
    scale_color_manual(values = colors) +
    scale_x_continuous(breaks = seq(start_year, current_year, 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "",
      x = "",
      y = paste0("Pourcentage d'évolution par rapport à ", start_year),
      color = ""
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, b = 0, l = 25, r = 30),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title = element_text(color = "#294754"),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )

  p <- p +
    annotate("segment", x = 2020, xend = 2022, y = -0.03, yend = -0.03,
             color = "grey70", size = 0.5) +
    annotate("text", x = 2021, y = -0.015,
             label = "Période de pandémie",
             color = "grey40", size = 3.5, fontface = "bold")


  # plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    config(displayModeBar = FALSE)

  for (i in 1:nrow(df_labels)) {
    reg <- df_labels$milieu_region[i]
    val <- df_labels$evolution[i]
    label_text <- df_labels$label[i]

    if (reg == "France") {
      g <- g |>
        add_annotations(
          x = df_labels$year[i],
          y = val,
          text = label_text,
          showarrow = FALSE,
          xshift = 30,
          font = list(color = "white", size = 14, family = "Poppins"),
          bgcolor = colors[reg],
          bordercolor = colors[reg]
        )
    } else {
      g <- g |>
        add_annotations(
          x = df_labels$year[i],
          y = val,
          text = label_text,
          showarrow = FALSE,
          xshift = 30,
          font = list(color = "black", size = 13, family = "Poppins")
        )
    }
  }

  if (tab_typology == "france"){
    g <- g |>
      layout(
        legend = list(
          y = -0.20,
          yanchor = "bottom",
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          font = list(size = 15)
        )
      )
  } else if (tab_typology == "region"){
    g <- g |>
      layout(showlegend = FALSE)
  }

  return(g)
}



#' @title Create a Daily Average Bicycle Traffic Table
#'
#' @description Generates a GT table displaying the **average daily bicycle traffic**
#' (passages/day) for a given year, broken down by:
#' - settlement type ("Milieu" / urban density)
#' - type of use ("Utilitaire", "Loisirs", "Mixte")
#' - period ("Juillet - Août", "Reste de l'année", "Week-end", "Reste de la semaine")
#'
#' The table also shows the **evolution rate** compared to the previous year.
#' If the table is requested for a regional context (`tab_typology = "region"`),
#' it additionally shows the national (France-wide) values for comparison.
#'
#' @param data A data frame containing pre-calculated daily traffic data for all territories. Must include at least the following columns:
#'   - `category` : grouping variable for the row groups
#'   - `subcategory` : subcategory of the metric (e.g., "Grands centres urbains", "Utilitaire", etc.)
#'   - `value_current` : numeric average value for the current year
#'   - `value_previous` : numeric evolution compared to the previous year
#'   - `cog` : optional INSEE code of the territory
#'   - `region` : optional region name
#' @param current_year Numeric value of the year for which the table is generated.
#' @param tab_typology A `character` string indicating the typology of the view.
#'   Must be either `"france"` or `"region"`.
#' @param selected_cog Character or numeric, the INSEE code of the selected region (NULL if `tab_typology = "france"`).
#' @param selected_region Character, the name of the selected region (NULL if `tab_typology = "france"`).
#'
#' @return A `gt` table object with formatted values:
#' - Current year values (`value_current`) formatted with thousands separator.
#' - Previous year evolution (`value_previous`) formatted as percentages with sign.
#' - Row groups by `category` and rows by `subcategory`.
#' - Optional regional columns if `tab_typology = "region"`.
#'
#' @import dplyr
#' @import gt
#' @importFrom rlang .data
#' @noRd
table_avg_daily_traffic <- function(data, current_year, tab_typology,
                                    selected_cog = NULL, selected_region = NULL){

  previous_year <- current_year - 1

  # Formatting dataframes
  if (tab_typology == "region"){
    df_region <- data |>
      filter(cog == selected_cog) |>
      mutate(value_current_region = ifelse(is.na(value_current),
                                           "--",
                                           format(round(value_current, 0), big.mark = " ")),
             value_previous_region = format(round(value_previous, 1), decimal.mark = ",", trim = TRUE),
             value_previous_region = ifelse(is.na(value_previous),
                                            "--",
                                            paste0(
                                              ifelse(as.numeric(value_previous) >=0, "+", ""),
                                              value_previous_region,
                                              "%"))) |>
      select(-value_current, -value_previous, -region, -cog, -nb_compteurs)

    # If territory has no data, print message instead of table
    if (nrow(df_region) == 0) {
      tab <- gt::gt(data.frame(txt = "Aucune donnée de comptage vélo partagée pour ce territoire")) %>%
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
      return(list(table = tab, evol = "-")
      )
    }

    evol <- data |> filter(cog == selected_cog) |> filter(category == "Global") |> select(value_previous) |> pull()
  } else {
    evol <- data |> filter(is.na(cog)) |> filter(category == "Global") |> select(value_previous) |> pull()
  }

  df_france <- data |>
    filter(is.na(cog)) |>
    mutate(value_current = ifelse(is.na(value_current),
                                  "--",
                                  format(round(value_current, 0), big.mark = " ")),
           value_previous_chr = format(round(value_previous, 1), decimal.mark = ",", trim = TRUE),
           value_previous_chr = ifelse(is.na(value_previous),
                                       "--",
                                       paste0(
                                         ifelse(as.numeric(value_previous) >=0, "+", ""),
                                         value_previous_chr,
                                         "%"))) |>
    select(-value_previous, -region, -cog, -nb_compteurs) |>
    rename(value_previous = value_previous_chr)

  df <- if (tab_typology == "region") {
    left_join(df_region, df_france, by = c("category", "subcategory"))
  } else {
    df_france
  }

  df <- df |>
    mutate(subcategory = ifelse(is.na(subcategory), "", subcategory))

  # Create gt table
  tab <- df |>
    gt(rowname_col = "subcategory", groupname_col = "category") |>
    tab_stubhead(label = "") |>
    fmt_markdown(columns = everything())|>
    cols_width(
      subcategory ~ px(210),
      !subcategory ~ px(230)
    ) |>
    cols_label(
      value_current = html("<strong>France entière</strong><br>Passages/jour en 2024"),
      value_previous = html(paste0("<strong>France entière</strong><br>Evolution /", previous_year))
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = cell_borders(
        sides = "left",
        color = "grey40",
        weight = px(2)
      ),
      locations = cells_body(
        columns = c("value_previous")
      )
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = list(
        cells_column_labels(columns = c("value_current", "value_previous")),
        cells_body(columns = c("value_current", "value_previous")))
    ) |>
    tab_options(
      table.font.size = px(14),
      table.font.color = "#294754",
      table.font.names = "Poppins",
      data_row.padding = px(4),
      heading.padding = px(4),
      row_group.padding = px(5),
      column_labels.font.size = px(13),
      row_group.font.size = px(13),
      row_group.background.color = "#f4f6f6",
    )

  # Further styling to add the region columns
  if (tab_typology == "region"){
    tab <- tab |>
      cols_label(
        value_current_region = html(paste0("<strong>", selected_region, "</strong><br>Passages/jour en ", current_year)),
        value_previous_region = html(paste0("<strong>", selected_region, "</strong><br>Evolution /", previous_year))
      ) |>
      tab_style(
        style = cell_borders(
          sides = "left",
          color = "grey40",
          weight = px(2)
        ),
        locations = cells_body(
          columns = c("value_previous_region", "value_current")
        )
      ) |>
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_column_labels(columns = c("value_current_region", "value_previous_region"))
      ) |>
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = c("value_current_region", "value_previous_region"))
      ) |>
      cols_width(
        subcategory ~ px(200),
        !subcategory ~ px(180)
      )
  }

  return(list(table = tab,
               evol = evol))

}



#' @title Generate a GT Table of Average Daily Bicycle Passages by Commune Size
#'
#' @description This function creates a formatted \code{gt} table showing the
#' average daily bike passages in communes of different sizes, for the two most
#' recent years, along with the year-over-year evolution and the number of counters
#' included in the calculation.
#'
#' @param data A data frame containing pre-aggregated bike passage data.
#'   The data should include at least the following columns:
#'   \itemize{
#'     \item \code{comm_taille}: commune size category
#'     \item \code{nb_compteurs}: number of counters included
#'     \item columns for each year (numeric)
#'     \item columns for year-over-year evolution
#'   }
#' @param current_year Numeric, the latest year to display.
#'   The previous year is calculated automatically as current_year - 1.
#'
#' @return A \code{gt} object representing the formatted table.
#'
#' @import dplyr
#' @import gt
#' @importFrom rlang sym
#' @noRd
table_traffic_by_commune_size <- function(data, current_year){

  previous_year <- current_year - 1

  ## If territory has no data, print message instead of table
  if (nrow(data) == 0) {
    return(
      gt::gt(data.frame(txt = "Aucune donnée de comptage vélo partagée pour ce territoire")) %>%
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

  ## Format dataframe for gt table display
  df <- data |>
    # Rename years columns to allow generalization (and update each year)
    rename(value_current = !!sym(as.character(current_year)),
           value_previous = !!sym(as.character(previous_year)),
           evo = !!sym(paste0("evo_", substr(previous_year, 3, 4), "_", substr(current_year, 3, 4)))) |>
    mutate(
      # Replace values with NA if fewer than 3 counters
      value_current = ifelse(nb_compteurs < 3, NA, value_current),
      value_previous = ifelse(nb_compteurs < 3, NA, value_previous),
      evo = ifelse(nb_compteurs < 3, NA, evo),

      # Format numeric columns for display, replace NA with "--"
      value_current = ifelse(is.na(value_current),
                             "--",
                             format(round(value_current, 0), big.mark = " ")),
      value_previous = ifelse(is.na(value_previous),
                              "--",
                              format(round(value_previous, 0), big.mark = " ")),
      evo = ifelse(is.na(evo),
                   "--",
                   paste0(ifelse(evo >=0, "+", ""),
                          format(round(100*evo, 1), decimal.mark = ",", trim = TRUE),
                          "%")),

      # Recode commune size labels, escape ">" symbol for HTML rendering
      comm_taille = recode(
        comm_taille,
        "<2000" = "< 2 000",
        "entre 2000 et 5k" = "entre 2 000 et 5 000",
        "entre 5k et 20k" = "entre 5 000 et 20 000",
        "entre 20k et 100k" = "entre 20 000 et 100 000",
        "entre 100k et 500k" = "entre 100 000 et 500 000",
        ">500k" = "&gt; 500 000" ),

      # Convert to ordered factor for logical display
      comm_taille = factor(comm_taille,
                           levels = c("< 2 000", "entre 2 000 et 5 000", "entre 5 000 et 20 000",
                                      "entre 20 000 et 100 000", "entre 100 000 et 500 000",
                                      "&gt; 500 000"))) |>
    # Arrange by commune size factor
    arrange(comm_taille)  |>
    select(comm_taille, nb_compteurs, value_current, value_previous, evo)


  ## Create gt table
  tab <- df |>
    gt() |>
    # Enable markdown/HTML formatting
    fmt_markdown(columns = everything())|>
    # Set column widths
    cols_width(everything() ~ px(240)) |>
    # Add HTML-formatted column labels
    cols_label(
      comm_taille = html(paste0("<strong>Nombre d'habitants</strong>")),
      nb_compteurs = html(paste0("<strong>Nombre de compteurs</strong>")),
      value_current = html(paste0("<strong>Moyenne journalière ", current_year, "</strong>")),
      value_previous = html(paste0("<strong>Moyenne journalière ", previous_year, "</strong>")),
      evo = html(paste0("<strong>Evolution /", previous_year, "</strong>"))
    ) |>
    # Center numeric columns only
    cols_align(
      align = "center",
      columns = c(nb_compteurs, value_current, value_previous, evo)
    ) |>
    # Apply alternating row background color
    tab_style(
      style = cell_fill(color = "#f4f6f6"),
      locations = cells_body(
        rows = seq(1, nrow(df), by = 2)
      )
    ) |>
    # Table-wide formatting options
    tab_options(
      table.font.size = px(14),
      table.font.color = "#294754",
      table.font.names = "Poppins",
      data_row.padding = px(4),
      heading.padding = px(4),
      column_labels.font.size = px(14)
    )

  return(tab)
}



#' @title Generate monthly distribution of bicycle passages per day
#'
#' @description This function computes the monthly distribution of bicycle traffic
#' (average daily passages per month) based on daily count data, and produces tooltip
#' labels for visualization.
#' It allows comparisons at the France level (with typologies of urbanization)
#' or at the region level (with comparison to the same region or France).
#'
#' @param data_daily A data frame containing daily bicycle counts.
#'   Must include at least the following columns:
#'   - `site_id` (counter identifier),
#'   - `date` (date of measurement),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`).
#' @param current_year Integer. The most recent year with complete data.
#' @param tab_typology Character. The typology of the visualization tab:
#'   - `"france"`: data aggregated at the France level,
#'   - `"region"`: data filtered for a specific region.
#' @param comparison Character. The element used for comparison of monthly distribution.
#'   - If `tab_typology == "france"`: can be `"all"` (all environments) or a specific
#'     type of urbanization (`"Grands centres urbains"`, `"Communes intermédiaires"`, `"Communes rurales"`).
#'   - If `tab_typology == "region"`: can be `"region"` (compare with same region, previous year),
#'     or `"france"` (compare with France in the same year).
#' @param territory_name Character. Name of the selected territory (region name if `tab_typology == "region"`,
#'   or `"France"` if national).
#'
#' @return A list with three elements:
#'   - `monthly_distribution`: a tibble with monthly percentages and tooltips for visualization,
#'   - `nb_counters`: integer, number of counters included in the analysis,
#'   - `avg_pct`: numeric, average monthly percentage for the current year.
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @noRd
generate_monthly_passages_per_day <- function(data_daily, current_year,
                                              tab_typology, comparison,
                                              territory_name){

  previous_year <- current_year - 1

  ## Apply filters depending on typology and comparison
  if (tab_typology == "france" && comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    df_daily <- data_daily |> filter(milieu == comparison)
  } else if (tab_typology == "france" && comparison %in% c("all", "france")){
    df_daily <- data_daily
  } else if (tab_typology == "region"){
    df_daily <- data_daily |> filter(region == territory_name)
  }

  # Return empty df if territory has no data
  if (nrow(df_daily) == 0){
    return(list(monthly_distribution = df_daily, nb_counters = 0, avg_pct = 0))
  }


  ## Number of unique counters included in the analysis
  nb_counters <- df_daily |> distinct(site_id) |> nrow()


  ## Compute average bicycle traffic by month (passages/day)
  avg_passages_per_month <- df_daily |>
    filter(year(date) %in% c(previous_year, current_year)) |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month) |>
    summarise(count = mean(count), .groups = "drop") |>
    mutate(
      year = year(month),
      month = month(month, label = TRUE, abbr = FALSE, locale = "fr_FR.UTF-8")
    )

  ## Compute monthly distribution
  monthly_distribution <- avg_passages_per_month |>
    (\(df) {
      if (comparison == "france") {filter(df, year == current_year)} else {df}
    })() |>
    group_by(year) |>
    mutate(pct = 100 * count / sum(count, na.rm = TRUE)) |>
    ungroup()


  ## Average percentage
  avg_pct <- mean((monthly_distribution |> filter(year == current_year))$pct)


  ## Tooltip for visualization
  if (comparison %in% c("region", "france", "all")){
    prefix <- paste0("En ", territory_name)
  } else if (comparison %in%  c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    prefix <- paste0("Dans les ", str_to_lower(comparison))
  }

  monthly_distribution <- monthly_distribution |>
    mutate(
      tooltip = paste0(prefix,
                       " en ", year, ", <span style='font-weight:bold'>", month,
                       "</span> représente <span style='font-weight:bold'>", format(round(pct, 1), decimal.mark = ",", trim = TRUE),
                       "% </span> des passages<br>annuels, avec en moyenne <span style='font-weight:bold'>",
                       format(round(count, 0), big.mark = " "),
                       "</span> passages par jour")) |>
    (\(df) {
      if (comparison %in% c("france", "region", "all")) {mutate(df, year = paste0(territory_name, " ", year))} else {df}
    })() |>
    (\(df) {
      if (comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")) {mutate(df, year = paste0(comparison, " ", year))} else {df}
    })()

  return(list(monthly_distribution = monthly_distribution,
              nb_counters = nb_counters,
              avg_pct = avg_pct))
}



#' @title Plot Monthly Distribution of Bicycle Passages
#'
#' @description This function generates an interactive Plotly chart showing the
#' monthly distribution of bicycle passages based on daily count data. It visualizes
#' average daily counts per month for the current year, optionally comparing with a
#' previous year or France/region. A horizontal line indicates the monthly average,
#' and labels display the percentage above each bar.
#'
#' @param data_daily A data frame containing daily bicycle counts. Must be compatible
#'   with `generate_monthly_passages_per_day()`. Required columns include:
#'   - `site_id` (counter identifier),
#'   - `date` (date of measurement),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`).
#' @param current_year Integer specifying the most recent year to visualize.
#' @param tab_typology Character string specifying the tab typology:
#'   - `"france"`: aggregate at national level,
#'   - `"region"`: filter for a specific region.
#' @param comparison Character string specifying the comparison element:
#'   - If `tab_typology == "france"`: `"all"` or specific urbanization types (`"Grands centres urbains"`, `"Communes intermédiaires"`, `"Communes rurales"`).
#'   - If `tab_typology == "region"`: `"region"` (compare with previous year for the same region) or `"france"` (compare with France for the current year).
#' @param territory_name Character string specifying the name of the territory (region name or `"France"`).
#'
#' @return A `plotly` object representing the monthly distribution. The plot includes:
#'   - Colored bars grouped by year or comparison category.
#'   - A horizontal dotted line indicating the average monthly percentage (`avg_pct`).
#'   - Labels above each bar showing the monthly percentage.
#'   - Interactive tooltips from the `tooltip` column of the underlying data.
#'
#' @import ggplot2
#' @import dplyr
#' @import plotly
#' @importFrom stats setNames
#' @noRd
plot_monthly_distribution <- function(data_daily, current_year,
                                      tab_typology, comparison,
                                      territory_name){

  previous_year <- current_year - 1

  ## Generate data
  monthly_passages <- generate_monthly_passages_per_day(data_daily, current_year,
                                                        tab_typology, comparison,
                                                        territory_name)
  nb_counters <- monthly_passages$nb_counters
  avg_pct <- monthly_passages$avg_pct

  # If territory has no data, print message
  if (nrow(monthly_passages$monthly_distribution) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo partagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.5, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0),
                nb_counters = 0))
  }

  # If comparison region/France, compute the two dataframes separately and merge
  if (comparison == "france"){
    monthly_region <- monthly_passages$monthly_distribution
    monthly_france <- generate_monthly_passages_per_day(data_daily, current_year,
                                                        "france", "france",
                                                        "France")$monthly_distribution
    monthly_distribution <- rbind(monthly_france, monthly_region)
  } else {
    monthly_distribution <- monthly_passages$monthly_distribution
  }


  ## Further formatting for month display
  month_labels <- c("janv.", "févr.", "mars", "avr.", "mai", "juin",
                    "juil.", "août", "sept.", "oct.", "nov.", "déc.")

  monthly_distribution$month <- factor(
    monthly_distribution$month,
    levels = unique(monthly_distribution$month),
    labels = month_labels
  )


  ## Custom color palette
  if (comparison == "france"){
    color_palette <- setNames(c("#cacaca", "#C6CE41"),
                              c(paste0("France ", current_year), paste0(territory_name, " ", current_year)))
  } else if (comparison == "Grands centres urbains"){
    color_palette <- setNames(c("#294754", "#f08472"),
                              c(paste0(comparison, " ", previous_year), paste0(comparison, " ", current_year)))
  } else if (comparison == "Communes intermédiaires"){
    color_palette <- setNames(c("#294754", "#9185BE"),
                              c(paste0(comparison, " ", previous_year), paste0(comparison, " ", current_year)))
  } else if (comparison == "Communes rurales"){
    color_palette <- setNames(c("#294754", "#C6CE41"),
                              c(paste0(comparison, " ", previous_year), paste0(comparison, " ", current_year)))
  } else {
    color_palette <- setNames(c("#294754", "#C6CE41"),
                              c(paste0(territory_name, " ", previous_year), paste0(territory_name, " ", current_year)))
  }

  monthly_distribution <- monthly_distribution |>
    mutate(bar_color = color_palette[as.character(year)])


  ## Set position for bar labels
  y_range <- max(monthly_distribution$pct, na.rm = TRUE) - min(monthly_distribution$pct, na.rm = TRUE)

  monthly_distribution <- monthly_distribution |>
    mutate(pct_label = pct + 0.07 * y_range)


  ## ggplot chart
  p <- ggplot(monthly_distribution, aes(x = month, y = pct, fill = factor(year), text = tooltip)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.87) +
    # Horizontal line for the average
    geom_segment(
      aes(x = as.numeric(month) - 0.5, xend = as.numeric(month) + 0.5,
          y = avg_pct, yend = avg_pct),
      color = "#4d4d4d",
      size = 0.1,
      linetype = "dotted"
    ) +
    # Colors
    scale_fill_manual(values = color_palette) +
    # Y axe in percentage
    scale_y_continuous(
      breaks = c(0, 5, 10, 15),
      labels = function(x) paste0(x, "%   "),
      limits = c(0, max(monthly_distribution$pct) * 1.2)
    ) +
    labs(title = "", x = NULL, y = NULL, fill = "") +
    # Bar labels with percentages
    geom_text(
      aes(y = pct_label, label = paste0(format(round(pct, 0), decimal.mark = ","), "%")),
      position = position_dodge(width = 0.85),
      size = 3.5,
      angle = 90,
      color = monthly_distribution$bar_color
    ) +
    # Months on top
    annotate(
      "text",
      x = seq_along(month_labels),
      y = max(monthly_distribution$pct) * 1.2,
      label = month_labels,
      hjust = 0.5,
      size = 3
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(
        size = 9,
        color = "#294754"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    geom_vline(
      xintercept = seq(1.5, length(month_labels) - 0.5, by = 1),
      color = "grey80",
      linewidth = 0.25
    ) +
    coord_cartesian(clip = "off")


  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        xanchor = "left",
        y = -0.05,
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


  return(list(plot = g, nb_counters = nb_counters))
}



#' @title Generate average weekday bicycle passages per day
#'
#' @description This function computes the average number of bicycle passages per
#' day of the week for a given year and territory. It returns both the absolute
#' counts (passages/day) and the relative distribution (%) across weekdays,
#' along with the number of counters used in the analysis and the average percentage
#' share per weekday.
#'
#' @param data_daily A data frame containing daily bicycle counts. Must be compatible
#'   with `generate_monthly_passages_per_day()`. Required columns include:
#'   - `site_id` (counter identifier),
#'   - `date` (date of measurement),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`).
#' @param current_year Integer. The most recent year to visualize.
#' @param tab_typology Character string specifying the tab typology:
#'   - `"france"`: aggregate at national level,
#'   - `"region"`: filter for a specific region.
#' @param comparison For `tab_typology == "france"`, a character string specifying the
#'   urbanization type:
#'   - `"Grands centres urbains"`,
#'   - `"Communes intermédiaires"`,
#'   - `"Communes rurales"`,
#'   - `"all"` for all urbanization types.
#'   For `tab_typology == "region"`, this must be `NULL`.
#' @param territory_name Character string specifying the name of the territory
#'   (region name or `"France"`).
#'
#' @return A list with three elements:
#' \describe{
#'   \item{`weekday_distribution`}{A tibble with the average passages/day and the
#'   percentage distribution per weekday, enriched with tooltip text for visualization
#'   and a "week" indicator (weekend vs. weekdays).}
#'   \item{`nb_counters`}{Integer. Number of unique counters included in the analysis.}
#'   \item{`avg_pct`}{Numeric. Average percentage share across weekdays.}
#' }
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @importFrom stats aggregate
#' @noRd
generate_weekday_passages_per_day <- function(data_daily, current_year,
                                              tab_typology, comparison,
                                              territory_name){

  ## Apply filters depending on typology and comparison
  if (tab_typology == "france" && comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    df_daily <- data_daily |> filter(milieu == comparison)
  } else if (tab_typology == "france"){
    df_daily <- data_daily
  } else if (tab_typology == "region"){
    df_daily <- data_daily |> filter(region == territory_name)
  }

  # Return empty df if territory has no data
  if (nrow(df_daily) == 0){
    return(list(weekday_distribution = df_daily, nb_counters = 0, avg_pct = 0))
  }


  ## Number of unique counters included in the analysis
  nb_counters <- df_daily |> distinct(site_id) |> nrow()


  ## Compute average bicycle traffic by day of the week (passages/day)
  avg_passages_per_weekday <- df_daily |>
    filter(year(date) == current_year) |>
    mutate(day = wday(date, label = TRUE, abbr = FALSE, week_start = 1, locale = "fr_FR.UTF-8")) |>
    group_by(day) |>
    summarise(count = mean(count), .groups = "drop")


  ## Compute weekday distribution in percentage
  weekday_distribution <- avg_passages_per_weekday |>
    mutate(pct = 100 * count / sum(count, na.rm = TRUE))


  ## Average percentage
  avg_pct <- mean((weekday_distribution$pct))


  ## Tooltip for visualization
  if (tab_typology == "france" && comparison %in%  c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    prefix <- paste0("Dans les ", str_to_lower(comparison))
  } else {
    prefix <- paste0("En ", territory_name)
  }

  weekday_distribution <- weekday_distribution |>
    mutate(
      tooltip = paste0(prefix,
                       " en ", current_year, ", le <span style='font-weight:bold'>", day,
                       "</span> représente <span style='font-weight:bold'>", format(round(pct, 1), decimal.mark = ",", trim = TRUE),
                       "% </span> des passages<br>de la semaine, avec en moyenne <span style='font-weight:bold'>",
                       format(round(count, 0), big.mark = " "),
                       "</span> passages par jour")) |>
    mutate(week = ifelse(day %in% c("samedi", "dimanche"),
                         "Week-end",
                         "Semaine"))

  return(list(weekday_distribution = weekday_distribution,
              nb_counters = nb_counters,
              avg_pct = avg_pct))
}



#' @title Plot weekday bicycle passages distribution
#'
#' @description This function generates an interactive bar chart (using **ggplot2**
#' and **plotly**) that visualizes the distribution of average daily bicycle passages
#' per weekday, expressed in percentages. It can show the national average (France)
#' or the distribution for a specific region, with comparison against national
#' values.
#'
#' @param data_daily A data frame containing daily bicycle counts. Must be compatible
#'   with `generate_monthly_passages_per_day()`. Required columns include:
#'   - `site_id` (counter identifier),
#'   - `date` (date of measurement),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`).
#' @param current_year Integer. The most recent year to visualize.
#' @param tab_typology Character string specifying the typology of analysis:
#'   - `"france"`: aggregate at national level,
#'   - `"region"`: filter for a specific region.
#' @param comparison For `tab_typology == "france"`, a character string specifying the
#'   urbanization type:
#'   - `"Grands centres urbains"`,
#'   - `"Communes intermédiaires"`,
#'   - `"Communes rurales"`,
#'   - `"all"` for all urbanization types.
#'   For `tab_typology == "region"`, this must be `NULL`.
#' @param territory_name Character string specifying the name of the territory
#'   (region name or `"France"`).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`plot`}{A **plotly** interactive object showing the weekday distribution
#'   of passages, with tooltips, average line, and custom colors. If
#'   `tab_typology == "region"`, the France distribution is added as a line for
#'   comparison.}
#'   \item{`nb_counters`}{Integer. Number of unique counters included in the analysis.}
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import stringr
#' @importFrom stats aggregate
#' @noRd
plot_weekday_distribution <- function(data_daily, current_year,
                                      tab_typology, comparison,
                                      territory_name){

  ## Generate data
  weekday_passages <- generate_weekday_passages_per_day(data_daily, current_year,
                                                        tab_typology, comparison,
                                                        territory_name)
  nb_counters <- weekday_passages$nb_counters
  avg_pct <- weekday_passages$avg_pct
  weekday_distribution <- weekday_passages$weekday_distribution

  # If territory has no data, print message
  if (nrow(weekday_distribution) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo\npartagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.4, y_coord = 0.93, t = 12, l = 0, b = 20, r = 0),
                nb_counters = 0))
  }

  # Add France data for comparison if tab_typoloy == region
  if (tab_typology == "region"){
    weekday_france <- generate_weekday_passages_per_day(data_daily, current_year,
                                                        "france", "all",
                                                        "France")$weekday_distribution
    weekday_france <- weekday_france |>
      rename(pct_france = pct)

    weekday_distribution <- weekday_distribution |>
      left_join(weekday_france |> select(day, pct_france), by = "day") |>
      mutate(line_label = "France",
             tooltip_france = paste0("En France en ", current_year, ", le <span style='font-weight:bold'>", day,
                                     "</span> représente<br><span style='font-weight:bold'>", format(round(pct_france, 1), decimal.mark = ",", trim = TRUE),
                                     "% </span>des passages de la semaine")
      )
  }


  ## Further formatting for weekday display
  weekday_labels <- c("Lu", "Ma", "Me", "Je", "Ve", "Sa", "Di")

  weekday_distribution$day <- factor(
    weekday_distribution$day,
    levels = unique(weekday_distribution$day),
    labels = weekday_labels
  )


  ## Custom color palette
  if (tab_typology == "france" && comparison == "Grands centres urbains"){
    color_palette <- setNames(c("#294754", "#f08472"),
                              c("Week-end", "Semaine"))
  } else if (tab_typology == "france" && comparison == "Communes intermédiaires"){
    color_palette <- setNames(c("#294754", "#9185BE"),
                              c("Week-end", "Semaine"))
  } else{
    color_palette <- setNames(c("#294754", "#C6CE41"),
                              c("Week-end", "Semaine"))
  }

  weekday_distribution <- weekday_distribution |>
    mutate(bar_color = color_palette[week])


  ## Set position for bar labels
  #y_range <- max(weekday_distribution$pct, na.rm = TRUE) - min(weekday_distribution$pct, na.rm = TRUE)

  weekday_distribution <- weekday_distribution |>
    mutate(pct_label = pct + 0.5)

  # Adjust position of bar label if tab_typology == region and if superposition with France data
  if (tab_typology == "region"){
    weekday_distribution <- weekday_distribution |>
      mutate(pct_label = ifelse(
        abs(pct_label - pct_france) < 0.7,
        pct_label + 1,
        pct_label
      ))
  }


  ## ggplot chart
  p <- ggplot(weekday_distribution, aes(x = day, y = pct, fill = factor(week), text = tooltip)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.8) +
    # Horizontal line for the average
    geom_segment(
      aes(x = as.numeric(day) - 0.5, xend = as.numeric(day) + 0.5,
          y = avg_pct, yend = avg_pct),
      color = "#4d4d4d",
      linewidth = 0.1,
      linetype = "dotted"
    ) +
    # Colors
    scale_fill_manual(values = color_palette) +
    # Y axe in percentage
    scale_y_continuous(
      breaks = c(0, 5, 10, 15),
      labels = function(x) paste0(x, "%   "),
      limits = c(0, max(weekday_distribution$pct) * 1.2)
    ) +
    labs(title = "", x = NULL, y = NULL, fill = "") +
    # Bar labels with percentages
    geom_text(
      aes(y = pct_label, label = paste0(format(round(pct, 0), decimal.mark = ","), "%")),
      position = position_dodge(width = 0.85),
      size = 3.5,
      color = weekday_distribution$bar_color
    ) +
    # Days on top
    annotate(
      "text",
      x = seq_along(weekday_labels),
      y = max(weekday_distribution$pct) * 1.2,
      label = weekday_labels,
      hjust = 0.5,
      size = 3
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 9, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    geom_vline(
      xintercept = seq(1.5, length(weekday_labels) - 0.5, by = 1),
      color = "grey90",
      linewidth = 0.25
    ) +
    coord_cartesian(clip = "off")

  if (tab_typology == "region"){
    p <- p +
      # Line + points for France data
      geom_line(
        data = weekday_distribution,
        aes(x = day, y = pct_france, color = line_label, group = 1, text = tooltip_france),
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      geom_point(
        data = weekday_distribution,
        aes(x = day, y = pct_france, color = line_label, text = tooltip_france),
        size = 1.5,
        inherit.aes = FALSE
      ) +
      # Color
      scale_color_manual(values = c("France" = "#cacaca"), name = "")
  }


  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        xanchor = "left",
        y = -0.05,
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


  # Clean legend
  if (tab_typology == "region"){
    g <- g |>
      style(name = "Semaine", traces = 1, showlegend = TRUE) |>
      style(name = "Week-end", traces = 2, showlegend = TRUE) |>
      style(name = "France", traces = 9, showlegend = TRUE) |>
      style(traces = c(3, 4, 5, 6, 7, 8), showlegend = FALSE)
  }

  return(list(plot = g, nb_counters = nb_counters))
}



#' @title Plot Seasonal Bicycle Traffic Distribution as Donut Chart
#'
#' @description This function generates an interactive donut chart showing the
#' distribution of average daily bicycle passages by season for a specified year
#' and territory. It calculates the percentage of passages per season and creates
#' hover tooltips with detailed information. Labels for each season can be manually
#' positioned  via `label_coords`.
#'
#' @param data_daily A `data.frame` containing daily bicycle counts. Required columns:
#'   - `site_id` (unique counter identifier),
#'   - `date` (Date or POSIXt object),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`),
#'   - `season` (factor or character with seasons, e.g., "Printemps", "Été", "Automne", "Hiver").
#' @param current_year Integer specifying the year to analyze.
#' @param tab_typology Character specifying the typology: `"france"` or `"region"`.
#' @param comparison Character. For `tab_typology == "france"`, allows filtering by milieu
#'   ("Grands centres urbains", "Communes intermédiaires", "Communes rurales") or "all".
#'   For `tab_typology == "region"`, ignored.
#' @param territory_name Character specifying the name of the region or `"France"`.
#' @param label_coords Optional `data.frame` containing manual label positions for each season.
#'   Must have columns:
#'   - `season`: season name,
#'   - `x`: x-coordinate (0–1),
#'   - `y`: y-coordinate (0–1).
#'   If `NULL`, default positions are used.
#'
#' @return A list containing:
#' \describe{
#'   \item{plot}{Interactive `plotly` donut chart with percentages and tooltips.}
#'   \item{nb_counters}{Number of unique counters included in the analysis.}
#' }
#'
#' @import dplyr
#' @importFrom plotly plot_ly layout
#' @importFrom stats setNames
#' @noRd
plot_season_distribution <- function(data_daily, current_year,
                                     tab_typology, comparison,
                                     territory_name, label_coords = NULL){

  ## Apply filters depending on typology and comparison
  if (tab_typology == "france" && comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    df_daily <- data_daily |> filter(milieu == comparison)
  } else if (tab_typology == "france"){
    df_daily <- data_daily
  } else if (tab_typology == "region"){
    df_daily <- data_daily |> filter(region == territory_name)
  }

  # If territory has no data, print message
  if (nrow(df_daily) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo\npartagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.4, y_coord = 0.97, t = 12, l = 0, b = 20, r = 0),
                nb_counters = 0))
  }


  ## Number of unique counters included in the analysis
  nb_counters <- df_daily |> distinct(site_id) |> nrow()


  ## Compute average bicycle traffic by season (passages/day)
  avg_passages_per_season <- df_daily |>
    filter(year(date) == current_year) |>
    group_by(season) |>
    summarise(count = mean(count), .groups = "drop")


  ## Compute season distribution in percentage
  season_distribution <- avg_passages_per_season |>
    mutate(pct = 100 * count / sum(count, na.rm = TRUE))


  ## Tooltip for visualization
  if (tab_typology == "france" && comparison %in%  c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    prefix <- paste0("Dans les ", str_to_lower(comparison))
  } else {
    prefix <- paste0("En ", territory_name)
  }

  season_distribution <- season_distribution |>
    mutate(
      tooltip = paste0(prefix,
                       " en ", current_year,
                       ifelse(season == "Printemps", ", le ", ", l'"),
                       "<span style='font-weight:bold'>", str_to_lower(season),
                       "</span> représente en moyenne <span style='font-weight:bold'>",
                       format(round(pct, 1), decimal.mark = ",", trim = TRUE),
                       "%</span> des<br>passages annuels, avec en moyenne <span style='font-weight:bold'>",
                       format(round(count, 0), big.mark = " "), "</span> passages par jour")
    )


  ## Set order for seasons
  season_distribution <- season_distribution |>
    mutate(season = factor(season, levels = c("Été", "Printemps", "Hiver", "Automne"))) |>
    arrange(season)


  ## Custom color palette
  season_colors <- c(
    "Hiver" = "#294754",
    "Printemps" = "#C6CE41",
    "Été" = "#E94F35",
    "Automne" = "#acacac"
  )


  ## Set positions for label
  if (is.null(label_coords)){    # Case when label_coords is not provided
    label_coords <- data.frame(
      season = c("Printemps", "Été", "Automne", "Hiver"),
      x = c(0.18, 0.84, 0.76, 0.18),
      y = c(0.85, 0.75, 0.01, 0.15)
    )
  }

  season_distribution <- season_distribution |>
    left_join(label_coords, by = "season")


  ## Plotly chart
  g <- plot_ly(
    data = season_distribution,
    labels = ~season,
    values = ~pct,
    type = "pie",
    hole = 0.45,
    textinfo = "none",
    hoverinfo = "text",
    text = ~tooltip,
    marker = list(colors = unname(season_colors[season_distribution$season])),
    rotation = 0,
    sort = FALSE
  ) |>
    layout(
      showlegend = FALSE,
      margin = list(t=20,b=20,l=20,r=20),
      annotations = lapply(1:nrow(season_distribution), function(i) {
        list(
          x = season_distribution$x[i],
          y = season_distribution$y[i],
          text = paste0("<b style='font-family:Poppins'>", season_distribution$season[i], "</b><br>",
                        "<span style='font-family:Poppins'>", format(round(season_distribution$pct[i],0), decimal.mark = ","), "%</span>"),
          showarrow = FALSE,
          font = list(
            family = "Poppins",
            size = 14,
            color = season_colors[season_distribution$season[i]]
          ),
          xanchor = "center",
          yanchor = "middle"
        )
      })
    ) |>
    config(displayModeBar = FALSE)

  return(list(plot = g, nb_counters = nb_counters))
}



#' @title Plot monthly bicycle traffic evolution compared to the previous year
#'
#' @description This function computes and visualizes the monthly evolution of daily bicycle passages
#' between two consecutive years (`current_year` and `current_year - 1`).
#' The evolution is expressed as a percentage rate of change relative to the previous year.
#' Results are shown as an interactive bar chart (via **plotly**), with optional comparison to
#' national-level trends ("France") when analyzing a specific region.
#'
#' @param data_daily A data frame containing daily bicycle counts. Must be compatible
#'   with `generate_monthly_passages_per_day()`. Required columns include:
#'   - `site_id` (counter identifier),
#'   - `date` (date of measurement),
#'   - `count` (number of passages),
#'   - `milieu` (urbanization type, required if `tab_typology == "france"`),
#'   - `region` (region name, required if `tab_typology == "region"`).
#' @param current_year Integer specifying the most recent year to visualize.
#'   The previous year is automatically computed as `current_year - 1`.
#' @param tab_typology Character string specifying the typology of analysis:
#'   - `"france"`: aggregate at the national level,
#'   - `"region"`: filter for a specific region.
#' @param comparison Character string controlling the comparison scope:
#'   - For `tab_typology == "france"`: may specify an urbanization type
#'     (`"Grands centres urbains"`, `"Communes intermédiaires"`, `"Communes rurales"`)
#'     or `"all"` for all contexts.
#'   - For `tab_typology == "region"`: should be set to `"region"`.
#' @param territory_name Character string specifying the name of the analyzed territory
#'   (region name or `"France"`).
#'
#' @return A list with two elements:
#'   - `plot`: an interactive **plotly** object showing monthly evolution
#'     of bicycle traffic compared to the previous year,
#'   - `nb_counters`: integer, the number of unique counters included in the analysis.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @import lubridate
#' @import stringr
#' @noRd
plot_monthly_evolution <- function(data_daily, current_year,
                                   tab_typology, comparison,
                                   territory_name){

  previous_year <- current_year - 1

  ## Get monthly data from current year and previous year (avg passages/day)
  monthly_passages <- generate_monthly_passages_per_day(data_daily, current_year,
                                                        tab_typology, comparison,
                                                        territory_name)
  nb_counters <- monthly_passages$nb_counters
  monthly_distribution <- monthly_passages$monthly_distribution

  # If territory has no data, print message
  if (nrow(monthly_distribution) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo partagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.5, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0),
                nb_counters = 0))
  }


  ## Compute evolution rate
  if (tab_typology == "france" && comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    prefix_year <- comparison
    prefix <- paste0("Dans les ", str_to_lower(comparison))
  } else {
    prefix_year <- territory_name
    prefix <- paste0("En ", territory_name)
  }

  monthly_evolution <- monthly_distribution |>
    mutate(year = gsub(paste0(prefix_year, " "), "", year)) |>
    select(month, year, count) |>
    pivot_wider(names_from = year, values_from = count, names_prefix = "count_") |>
    mutate(evolution_rate = (!!sym(paste0("count_", current_year)) - !!sym(paste0("count_", previous_year))) / !!sym(paste0("count_", previous_year)) * 100)


  ## Tooltip for visualization
  monthly_evolution <- monthly_evolution |>
    mutate(
      tooltip = paste0(prefix,
                       " en <span style='font-weight:bold'>", month, " ", current_year,
                       "</span>, le nombre de passages évolue<br>de <span style='font-weight:bold'>",
                       ifelse(evolution_rate >= 0, "+", ""),
                       format(round(evolution_rate, 1), decimal.mark = ",", trim = TRUE),
                       "%</span> par rapport à ", previous_year)
    )


  ## Add France data for comparison if tab_typoloy == region
  if (tab_typology == "region"){
    # Get data for France
    monthly_distribution_france <- generate_monthly_passages_per_day(data_daily, current_year,
                                                                     "france", "all",
                                                                     "France")$monthly_distribution
    # Compute evolution rate
    monthly_evolution_france <- monthly_distribution_france |>
      mutate(year = gsub("France ", "", year)) |>
      select(month, year, count) |>
      pivot_wider(names_from = year, values_from = count, names_prefix = "count_") |>
      mutate(evolution_rate_france = (!!sym(paste0("count_", current_year)) - !!sym(paste0("count_", previous_year))) / !!sym(paste0("count_", previous_year)) * 100,
             tooltip_france = paste0("En France en <span style='font-weight:bold'>", month, " ", current_year,
                                     "</span>, le nombre de passages évolue<br>de <span style='font-weight:bold'>",
                                     ifelse(evolution_rate_france >= 0, "+", ""),
                                     format(round(evolution_rate_france, 1), decimal.mark = ",", trim = TRUE),
                                     "%</span> par rapport à ", previous_year))

    # Merge
    monthly_evolution <- monthly_evolution |>
      left_join(monthly_evolution_france |> select(month, evolution_rate_france, tooltip_france), by = "month") |>
      mutate(line_label = "France")
  }


  ## Further formatting for month display
  month_labels <- c("janv.", "févr.", "mars", "avr.", "mai", "juin",
                    "juil.", "août", "sept.", "oct.", "nov.", "déc.")

  monthly_evolution$month <- factor(
    monthly_evolution$month,
    levels = unique(monthly_evolution$month),
    labels = month_labels
  )


  ## Set position for bar labels
  y_range <- max(monthly_evolution$evolution_rate, na.rm = TRUE) - min(monthly_evolution$evolution_rate, na.rm = TRUE)

  monthly_evolution <- monthly_evolution |>
    mutate(evolution_label = ifelse(evolution_rate >=0,
                                    evolution_rate + 0.035 * y_range,
                                    evolution_rate - 0.045 * y_range))

  # Adjust position of bar label if tab_typology == region and if superposition with France data
  if (tab_typology == "region"){
    monthly_evolution <- monthly_evolution |>
      mutate(evolution_label = ifelse(
        abs(evolution_label - evolution_rate_france) < 1,
        ifelse(evolution_label >=0, evolution_label + 3, evolution_label - 3),
        evolution_label
      ))
  }


  ## Colors
  monthly_evolution <- monthly_evolution |>
    mutate(evolution_type = ifelse(evolution_rate>=0, "Augmentation", "Diminution"),
           bar_color = ifelse(evolution_rate>=0, "#294754", "#E94F35"))

  color_palette <- setNames(c("#294754", "#E94F35"),
                            c("Augmentation", "Diminution"))


  ## ggplot chart
  p <- ggplot(monthly_evolution, aes(x = month, y = evolution_rate, fill = evolution_type, text = tooltip)) +
    geom_col() +
    # Colors
    scale_fill_manual(values = color_palette) +
    # Y axe in percentage
    scale_y_continuous(
      breaks = seq(-50, 80, 10),
      labels = function(x) paste0(x, "%   "),
      limits = c(min(min(monthly_evolution$evolution_rate) * 1.25, -8.5), max(monthly_evolution$evolution_rate) * 1.35)
    ) +
    labs(title = "", x = NULL, y = NULL, fill = "") +
    # Months on top
    annotate(
      "text",
      x = seq_along(month_labels),
      y = max(monthly_evolution$evolution_rate) * 1.35,
      label = month_labels,
      hjust = 0.5,
      size = 3
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(
        size = 9,
        color = "#294754"
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    geom_vline(
      xintercept = seq(1.5, length(month_labels) - 0.5, by = 1),
      color = "grey80",
      linewidth = 0.25
    ) +
    coord_cartesian(clip = "off")

  if (tab_typology == "region"){
    p <- p +
      # Line + points for France data
      geom_line(
        data = monthly_evolution,
        aes(x = month, y = evolution_rate_france, color = line_label, group = 1, text = tooltip_france),
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      geom_point(
        data = monthly_evolution,
        aes(x = month, y = evolution_rate_france, color = line_label, text = tooltip_france),
        size = 1.5,
        inherit.aes = FALSE
      ) +
      # Color
      scale_color_manual(values = c("France" = "#cacaca"), name = "")
  }

  p <- p +
    # Bar labels with percentages
    geom_text(
      aes(y = evolution_label,
          label = paste0(ifelse(evolution_rate >= 0, "+", "") ,
                         format(round(evolution_rate, 0), decimal.mark = ","), "%")),
      size = 3.5,
      color = monthly_evolution$bar_color
    )



  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        xanchor = "left",
        y = -0.05,
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

  # Clean legend
  if (tab_typology == "region"){
    g <- g |>
      style(name = "Augmentation", traces = 1, showlegend = TRUE) |>
      style(name = "Diminution", traces = 2, showlegend = TRUE) |>
      style(name = "France", traces = 5, showlegend = TRUE) |>
      style(traces = c(3, 4, 6, 7), showlegend = FALSE)
  }

  return(list(plot = g, nb_counters = nb_counters))
}



#' @title Plot Weekly Average Bicycle Passages
#'
#' @description This function computes and visualizes the average number of
#' bicycle passages per day by week. It supports filtering by national or regional
#' typology, comparison categories, and highlights school holiday weeks.
#' The output is a Plotly interactive chart and the number of counters
#' included in the analysis.
#'
#' @param data_daily A `data.frame` containing daily bicycle counts. Must include at least:
#'   \describe{
#'     \item{site_id}{Unique identifier of each counting site.}
#'     \item{date}{Date of measurement (Date or POSIXct).}
#'     \item{count}{Number of passages recorded.}
#'     \item{milieu}{Urbanization type (required if `tab_typology == "france"`).}
#'     \item{region}{Region name (required if `tab_typology == "region"`).}
#'   }
#' @param current_year Integer. The most recent year to visualize. The previous year is computed automatically.
#' @param tab_typology Character string specifying the typology:
#'   \describe{
#'     \item{"france"}{Aggregate data at the national level.}
#'     \item{"region"}{Filter data for a specific region.}
#'   }
#' @param comparison Character string controlling the comparison scope:
#'   \describe{
#'     \item{For `tab_typology == "france"`}{Can specify an urbanization type: `"Grands centres urbains"`, `"Communes intermédiaires"`, `"Communes rurales"`, or `"all"` for the entire country.}
#'     \item{For `tab_typology == "region"`}{Should be set to `"region"`.}
#'   }
#' @param territory_name Character string specifying the territory name for filtering
#'   (e.g., region name or `"France"`).
#'
#' @return A list containing:
#'   \describe{
#'     \item{plot}{A Plotly interactive line chart showing weekly average bicycle passages per day. The chart highlights school holiday weeks in gray and shows the average of the current year.}
#'     \item{nb_counters}{Integer. Number of unique counting sites included in the analysis.}
#'   }
#'
#' @importFrom dplyr filter mutate group_by summarise distinct
#' @importFrom lubridate isoweek month year
#' @importFrom stringr str_to_lower
#' @import ggplot2
#' @import plotly
#' @noRd
plot_weekly_passages <- function(data_daily, current_year,
                                 tab_typology, comparison,
                                 territory_name){

  previous_year <- current_year - 1

  ## Apply filters depending on typology and comparison
  if (tab_typology == "france" && comparison %in% c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    df_daily <- data_daily |> filter(milieu == comparison)
  } else if (tab_typology == "france"){
    df_daily <- data_daily
  } else if (tab_typology == "region"){
    df_daily <- data_daily |> filter(region == territory_name)
  }

  # If territory has no data, print message
  if (nrow(df_daily) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo partagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.5, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0),
                nb_counters = 0))
  }


  ## Number of unique counters included in the analysis
  nb_counters <- df_daily |> distinct(site_id) |> nrow()


  ## Compute average bicycle traffic by week (passages/day)
  avg_passages_per_week <- df_daily |>
    mutate(
      week = isoweek(date),
      month = month(date),
      year = year(date)
    )  |>
    # Discard days that belong to years outside the scope of analysis (current_year + previous_year)
    filter(!(month == 1 & week >= 52 & year == previous_year)) |>
    filter(!(month == 12 & week == 1 & year == current_year)) |>
    group_by(week, year) |>
    summarise(count = mean(count), .groups = "drop")

  global_avg <- mean((avg_passages_per_week |> filter(year == current_year))$count)


  ## Tooltip for visualization
  if (tab_typology == "france" && comparison %in%  c("Grands centres urbains", "Communes intermédiaires", "Communes rurales")){
    suffix <- paste0("dans les ", str_to_lower(comparison))
  } else {
    suffix <- paste0("en ", territory_name)
  }

  avg_passages_per_week <- avg_passages_per_week |>
    mutate(
      tooltip = paste0("<span style='font-weight:bold'> Semaine ", week, "</span> de l'année ",
                       year, " : <span style='font-weight:bold'>",
                       format(round(count, 0), big.mark = " "),
                       "</span> passages par jour<br>en moyenne ", suffix))


  ## Custom color palette
  color_palette <- setNames(c("#303876", "#C6CE41"),
                            c(previous_year, current_year))


  ## Custom sizes for lines
  size_vals <- setNames(c(0.5, 1),
                        c(previous_year, current_year))


  ## School breaks
  holiday_weeks <- c(1, 7, 8, 9, 10, 15, 16, 17, 18, 28, 29, 30, 31, 32, 33, 34, 35, 43, 44, 52)
  holiday_df <- data.frame(
    week = holiday_weeks,
    y = max(avg_passages_per_week$count)/2,
    height = 1.5 * (max(avg_passages_per_week$count))
  )


  ## ggplot chart
  p <- ggplot(avg_passages_per_week, aes(x = week, y = count, color = factor(year),
                                         group = year, text = tooltip, size = factor(year))) +
    # Highlight for school breaks
    geom_tile(data = holiday_df,
              inherit.aes = FALSE,
              aes(x = week, y = y, width = 1, height = height),
              fill = "grey90", alpha = 0.5) +
    # Vertical lines every two weeks
    geom_vline(xintercept = seq(1, max(avg_passages_per_week$week), by = 2),
               color = "grey90", size = 0.25) +
    geom_line() +
    # Horizontal line for the average
    geom_segment(
      aes(x = as.numeric(week) - 0.5, xend = as.numeric(week) + 0.5,
          y = global_avg, yend = global_avg),
      color = "#4d4d4d",
      linewidth = 0.1,
      linetype = "dotted"
    ) +
    annotate("text", x = 2.5, y = 1.04 * global_avg, label = paste0("Moy. ", current_year),
             color = "#4d4d4d", size = 3, fontface = "bold") +
    # Colors
    scale_color_manual(values = color_palette)  +
    # Sizes
    scale_size_manual(values = size_vals, guide = "none") +
    scale_x_continuous(breaks = seq(0, max(avg_passages_per_week$week), by = 5)) +
    labs(x = "Semaine", y = "Nombre moyen de passages par jour",
         color = "") +

    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(color = "#294754"),
      axis.text.y = element_text(size = 10, color = "#294754"),
      axis.text.x = element_text(size = 10, color = "#294754"),
      axis.title.y = element_text(size = 12, color = "#294754"),
      axis.title.x = element_text(size = 12, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    coord_cartesian(ylim = c(10, max(avg_passages_per_week$count) + 20))


  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        # orientation = "h",
        # xanchor = "left",
        # y = -0.15,
        # traceorder = "normal",
        font = list(size = 15, color = "#294754")
      )
    ) |>
    style(traces = 1, name = "Vacances\nscolaires", showlegend = TRUE) |>
    style(traces = c(3, 4) , showlegend = TRUE) |>
    style(traces = c(2, 5, 6, 7) , showlegend = FALSE) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png",  filename = "plot_export")
    )

  return(list(plot = g, nb_counters = nb_counters))
}



#' @title Interactive Leaflet Map of Bicycle Counters
#'
#' @title This function builds an interactive Leaflet map to visualize bicycle counters
#' in France (or within a specific region). Each counter is represented by a circle
#' with visual encoding for its environment ("milieu d'implantation"),
#' cycling practice type, ownership, associated cycling routes, and whether
#' the counter is included in the analysis for the selected year.
#'
#' @param admin_compteurs A data frame containing the counters metadata.
#'   Must include columns: `site_id`, `region13_code`, `comm_nom`,
#'   `pratique_type`, `densite3_libelle`, `domaine_nom`, `europeen`, `national`,
#'   `xlong`, `ylat`, and year-specific columns such as
#'   `analyse_<year>` (e.g., `analyse_2024`).
#' @param admin_region An `sf` object containing the administrative regions
#'   geometries, with at least columns `INSEE_REG` and `geometry`.
#' @param tab_typology A character string defining the scale of visualization.
#'   Must be either `"france"` to show all counters, or `"region"` to filter
#'   to a specific administrative region.
#' @param selected_territory A character string giving the INSEE code of the
#'   selected region when `tab_typology = "region"`. Ignored otherwise.
#' @param current_year Integer specifying the reference year for analysis.
#'
#' @return
#' A `leaflet` map widget object that can be printed or embedded in a Shiny app.
#'
#' @import dplyr
#' @import stringr
#' @import leaflet
#' @importFrom htmltools HTML htmlEscape
#' @importFrom sf st_as_sf st_geometry
#' @noRd
map_info_compteurs <- function(admin_compteurs, admin_region,
                               tab_typology, selected_territory,
                               current_year, ns){

  previous_year <- current_year - 1

  # ---- DATA PREPROCESSING ----

  df_info_compteurs <- admin_compteurs |>
    select(site_id, region13_code, comm_nom, pratique_type,
           densite3_libelle, domaine_nom, europeen, national,  xlong, ylat,
           all_of(c(paste0("analyse_", current_year), paste0("analyse_", previous_year)))
    ) |>
    mutate(
      lon = as.numeric(str_replace(xlong, ",", ".")),
      lat = ylat / 1e6,
      milieu = case_when(
        densite3_libelle == "Communes de densité intermédiaire" ~ "Communes intermédiaires",
        densite3_libelle == "Communes rurales" ~ "Communes rurales",
        densite3_libelle == "Communes densément peuplées" ~ "Grands centres urbains"
      ),
      pratique_type = ifelse(is.na(pratique_type), "-", pratique_type)
    )

  if (tab_typology == "region"){
    df_info_compteurs <- df_info_compteurs |>
      filter(region13_code == selected_territory)
  }

  # Dynamic variables
  col_current <- paste0("analyse_", current_year)

  # Custom color palette
  palette_vals <- c(
    "Grands centres urbains" = "#E94F35",
    "Communes intermédiaires" = "#9185BE",
    "Communes rurales"   = "#C6CE41"
  )
  default_color <- "#808080"


  df_map <- df_info_compteurs |>
    filter(!is.na(lon) & !is.na(lat)) |>
    mutate(
      # Map color to the milieu
      color_val = palette_vals[as.character(milieu)],
      color_val = as.character(ifelse(is.na(color_val), default_color, color_val)),
      # Normalize the current-year flag to logical (treat NA as FALSE for the map)
      analysed = as.logical(.data[[col_current]]),
      analysed = ifelse(is.na(analysed), FALSE, analysed)
    ) |>
    # Build tooltips (rowwise to concat europeen/national cleanly)
    rowwise() |>
    mutate(
      itineraire = {
        parts <- c(europeen, national)
        parts <- parts[!is.na(parts) & parts != ""]
        if (length(parts) == 0) "-" else paste(parts, collapse = ", ")
      },
      tooltip_html = paste0(
        "<div style='margin-bottom:4px;'>",
        "<b><span style='font-size:1.1em;'>", htmlEscape(comm_nom), "</span></b> ",
        "<span style='font-size:0.85em; color:#666'>(", htmlEscape(site_id), ")</span>",
        "</div>",
        "<b>Propriétaire:</b> ", htmlEscape(domaine_nom), "<br/>",
        "<b>Milieu:</b> ", htmlEscape(milieu), "<br/>",
        "<b>Pratique:</b> ", htmlEscape(pratique_type), "<br/>",
        "<b>Itinéraire cyclable:</b> ", htmlEscape(itineraire)
      ),
      # cross HTML (centered, colored like the outline)
      cross_html = paste0(
        "<div style='font-size:12px; font-weight:700; color:", color_val,
        "; line-height:1; text-shadow: 0 0 1px #fff;'>✕</div>"
      )
    ) |>
    ungroup()


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
  }


  # ---- LEAFLET MAP ----

  map <- leaflet(df_map, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom)

  # Region outline if relevant
  if (tab_typology == "region"){
    map <- map |>
      addPolygons(
        data = admin_region |> filter(INSEE_REG == selected_territory),
        fill = FALSE,
        color = "#444444",
        weight = 1.5,
        opacity = 0.8
      )
  }

  # France outline if relevant
  if (tab_typology == "france"){
    map <- map |>
      addPolygons(
        data = contour_metropole,
        color = "#000000", weight = 1.5, opacity = 0.6,
        fill = FALSE
      )
  }

  if (nrow(filter(df_map, analysed == TRUE)) > 0){
    map <- map |>
      # Full filled circles for analysed == TRUE
      addCircles(
        data = filter(df_map, analysed == TRUE),
        lng = ~lon, lat = ~lat,
        color = ~color_val, weight = 1, opacity = 1,
        fillColor = ~color_val, fillOpacity = 0.9,
        #popup = ~lapply(tooltip_html, HTML),
        label = ~lapply(tooltip_html, HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE),
        highlightOptions = highlightOptions(
          color = "#000000", weight = 2, bringToFront = TRUE
        )
      ) |>

      # Outline circles for analysed == FALSE
      addCircles(
        data = filter(df_map, analysed == FALSE),
        lng = ~lon, lat = ~lat,
        color = ~color_val, weight = 1.5, opacity = 1,
        fill = TRUE, fillOpacity = 0.2,
        #popup = ~lapply(tooltip_html, HTML),
        label = ~lapply(tooltip_html, HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px", sticky = TRUE),
        highlightOptions = highlightOptions(
          color = "#000000", weight = 2, bringToFront = TRUE
        )
      ) |>

      # Adjust circle size depending on the zoom
      htmlwidgets::onRender("
          function(el, x) {
            var map = this;

            // Convert zoom into radius or cross size
            function zoomToRadius(zoom) {
              if(zoom >= 11) return 500;
              else if(zoom >= 10) return 1000;
              else if(zoom >= 9) return 2500;
              else if(zoom >= 8) return 3300;
              else if(zoom >= 7) return 5000;
              else if(zoom >= 6) return 6700;
              else return 8000;
            }

            function updateRadius() {
              var zoom = map.getZoom();

              map.eachLayer(function(layer) {
                if(layer instanceof L.CircleMarker){
                  layer.setRadius(zoomToRadius(zoom));
                }
              });
            }

            map.on('zoomend', updateRadius);
            updateRadius();
          }
        ")
  }


  # ---- CUSTOM LEGEND ----

  legend_html <- sprintf('
  <div style="background: white; padding: 10px; border-radius: 8px;
              font-size: 14px; line-height: 1.6;
              box-shadow: 0 0 5px rgba(0,0,0,0.2);">

    <!-- Milieu d implantation -->
    <div style="margin-bottom:6px;">
      <b>Milieu d\'implantation</b><br/>
      <div style="display:flex; align-items:center;">
        <div style="width:14px; height:14px; background:#E94F35; margin-right:6px;"></div>
        Grands centres urbains
      </div>
      <div style="display:flex; align-items:center;">
        <div style="width:14px; height:14px; background:#9185BE; margin-right:6px;"></div>
        Communes intermédiaires
      </div>
      <div style="display:flex; align-items:center;">
        <div style="width:14px; height:14px; background:#C6CE41; margin-right:6px;"></div>
        Communes rurales
      </div>
    </div>

    <div style="height:6px;"></div>

    <!-- Analyse -->
    <div>
      <b>Analyse %s</b><br/>
      <div style="display:flex; align-items:center; margin-top:4px;">
        <div style="width:14px; height:14px; border-radius:50%%;
                    background:#666; margin-right:6px;"></div>
        Oui
      </div>
      <div style="display:flex; align-items:center;">
        <div style="width:14px; height:14px; border-radius:50%%;
                    background:rgba(102,102,102,0.15); border:1.5px solid #666;
                    margin-right:6px;"></div>
        Non
      </div>
    </div>
  </div>
', current_year)

  map <- map |> addControl(html = legend_html, position = "bottomleft")

  # Add select input to set the view on a DROM
  select_width <- 229
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



#' @title Create an Interactive Leaflet Map Showing Average Bike Traffic and its Evolution
#'
#' @description This function generates an interactive Leaflet map of bike traffic counters,
#' showing the average daily passages for a given year and their evolution
#' compared to the previous year. Circles are sized according to average traffic
#' and colored according to the percentage change from the previous year.
#' A combined legend for both colors and circle sizes is added.
#'
#' @param data_daily A `data.frame` containing daily bicycle counts. Must include at least:
#'   \describe{
#'     \item{site_id}{Unique identifier of each counting site.}
#'     \item{date}{Date of measurement (Date or POSIXct).}
#'     \item{count}{Number of passages recorded.}
#'     \item{milieu}{Urbanization type (required if `tab_typology == "france"`).}
#'     \item{region}{Region name (required if `tab_typology == "region"`).}
#'   }
#' @param admin_compteurs A data frame of traffic counters, including
#'   \code{site_id}, \code{xlong}, \code{ylat}, \code{densite3_libelle},
#'   \code{pratique_type}, \code{domaine_nom}, \code{europeen}, \code{national}, etc.
#' @param admin_region An sf object containing regional boundaries.
#' @param tab_typology Character. Either \code{"france"} or \code{"region"} to filter data.
#' @param selected_territory Character. The region INSEE code to filter if \code{tab_typology = "region"}.
#' @param territory_name Character. The name of the region to filter if \code{tab_typology = "region"}.
#' @param current_year Numeric. The year for which to compute average traffic.
#'
#' @return A \code{leaflet} object with circle markers representing bike traffic counters,
#'   colored by evolution (%) and sized by average daily passages. Includes
#'   a combined legend for both color and size.
#'
#' @importFrom dplyr filter mutate group_by summarise select rename left_join rowwise ungroup case_when
#' @importFrom scales rescale
#' @importFrom leaflet leaflet addProviderTiles setView addCircleMarkers addPolygons addControl leafletOptions
#' @importFrom htmltools HTML
#' @importFrom stringr str_replace
#' @importFrom lubridate year
#' @noRd
map_avg_traffic <- function(data_daily, admin_compteurs, admin_region,
                            tab_typology, selected_territory,
                            territory_name, current_year, ns){

  # ---- DATA PREPROCESSING ----

  previous_year <- current_year - 1

  # Filter by region if needed
  if (tab_typology == "region"){
    df_daily <- data_daily |>
      filter(region == territory_name)
  } else{df_daily <- data_daily}

  # Compute average passages per day
  avg_passages_per_counter <- df_daily |>
    mutate(year = year(date)) |>
    group_by(site_id, year) |>
    summarise(count = mean(count), .groups = "drop") |>
    pivot_wider(
      names_from = year,
      values_from = count,
      names_prefix = "count_"
    ) |>
    mutate(
      evo = 100 * (!!sym(paste0("count_", current_year)) / !!sym(paste0("count_", previous_year)) - 1)
    ) |>
    select(-!!sym(paste0("count_", previous_year))) |>
    rename(count = !!sym(paste0("count_", current_year)))

  # Join with counters info
  admin_compteurs <- admin_compteurs |>
    mutate(
      lon = as.numeric(str_replace(xlong, ",", ".")),
      lat = ylat / 1e6,
      milieu = case_when(
        densite3_libelle == "Communes de densité intermédiaire" ~ "Communes intermédiaires",
        densite3_libelle == "Communes rurales" ~ "Communes rurales",
        densite3_libelle == "Communes densément peuplées" ~ "Grands centres urbains"
      ),
      pratique_type = ifelse(is.na(pratique_type), "-", pratique_type),
      site_id = as.character(site_id)
    ) |>
    select(site_id, region13_code, comm_nom, domaine_nom, pratique_type, milieu,
           europeen, national, lon, lat)

  df_map <- avg_passages_per_counter |>
    left_join(admin_compteurs, by = "site_id") |>
    # Build tooltips (rowwise to concat europeen/national cleanly)
    rowwise() |>
    mutate(
      itineraire = {
        parts <- c(europeen, national)
        parts <- parts[!is.na(parts) & parts != ""]
        if (length(parts) == 0) "-" else paste(parts, collapse = ", ")
      },
      tooltip_html = paste0(
        "<div style='margin-bottom:4px;'>",
        "<b><span style='font-size:1.1em;'>", htmlEscape(comm_nom), "</span></b> ",
        "<span style='font-size:0.85em; color:#666'>(", htmlEscape(site_id), ")</span>",
        "</div>",
        "<b>Fréquentation ", current_year, ":</b> ", htmlEscape(format(round(count, 0), big.mark = " ")), " passages/jour<br/>",
        "<b>Évolution /", previous_year, ":</b> ", htmlEscape(paste0(ifelse(evo>=0, "+", ""), format(round(evo, 1), decimal.mark = ","), "%")), "<br/>",
        "<b>Propriétaire:</b> ", htmlEscape(domaine_nom), "<br/>",
        "<b>Milieu:</b> ", htmlEscape(milieu), "<br/>",
        "<b>Pratique:</b> ", htmlEscape(pratique_type), "<br/>",
        "<b>Itinéraire cyclable:</b> ", htmlEscape(itineraire)
      )
    ) |>
    ungroup()


  # ---- COLOR PALETTE AND CIRCLE SIZE ----

  start <- ifelse(tab_typology == "france", 3.5, 7)

  df_map <- df_map |>
    mutate(
      color = case_when(
        evo < -10             ~ "#4e659b",
        evo >= -10 & evo < -5 ~ "#7190da",
        evo >= -5  & evo < 0  ~ "#ccd7f1",
        evo >= 0   & evo < 5  ~ "#eff3de",
        evo >= 5   & evo < 10 ~ "#d7e0ac",
        evo >= 10  & evo < 25 ~ "#c6d289",
        evo >= 25             ~ "#b1c068",
        TRUE                  ~ "#BDBDBD"
      ),
      radius = scales::rescale(count, to = c(start, 30), from = c(0, 14000), na.rm = TRUE)
    )


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
  }


  # ---- LEGEND ----

  # Colors
  color_bins <- c("< -10 %", "-10 % à -5 %", "-5 % à 0 %", "0 % à +5 %",
                  "+5 % à +10 %", "+10 % à +25 %", "> +25 %")

  color_values <- c("#4e659b", "#7190da", "#ccd7f1",
                    "#eff3de", "#d7e0ac", "#c6d289", "#b1c068")

  legend_colors <- paste0(
    "<div style='margin-bottom:8px;'>",
    "<b>Évolution /", previous_year, "</b><br/>",
    paste0(
      "<div style='display:flex; align-items:center;'>",
      "<div style='width:15px; height:15px; background:", color_values,
      "; border:1px solid #333; margin-right:6px;'></div>",
      color_bins,
      "</div>",
      collapse = ""
    ),
    "</div>"
  )

  # Sizes
  size_vals <- c(100, 5000, 10000)
  size_radii <- scales::rescale(size_vals, to = c(start, 30), from = c(0, 14000))

  make_half_circle <- function(r, val, r_max) {
    svg_size <- 2 * r_max
    svg_height <- r_max
    dy <- r_max - r
    path <- paste0(
      "M ", r_max - r, " ", svg_height,
      " A ", r, " ", r, " 0 0 1 ", r_max + r, " ", svg_height
    )
    paste0(
      "<div style='display:flex; align-items:center; margin-bottom:2px;'>",
      "<svg height='", svg_height, "' width='", svg_size, "' style='overflow:visible;'>",
      "<path d='", path,
      "' fill='grey' fill-opacity='0.6' stroke='black' stroke-width='0.5'/>",
      "</svg>",
      "<div style='margin-left:8px; min-width:50px;'>",
      format(val, big.mark = " "),
      "</div>",
      "</div>"
    )
  }

  r_max <- max(size_radii)

  size_items <- mapply(make_half_circle, size_radii, size_vals, MoreArgs = list(r_max = r_max), SIMPLIFY = TRUE)

  legend_sizes <- paste0(
    "<div style='margin-top:8px;'>",
    "<b>Fréquentation moyenne<br/>(passages/jour)</b><br/>",
    paste(size_items, collapse = ""),
    "</div>"
  )

  # Merge
  legend_all <- paste0(
    "<div style='background:white; padding:8px; border-radius:6px; width: 200px;",
    "box-shadow:0 0 6px rgba(0,0,0,0.3); font-size:1em;'>",
    legend_colors,
    legend_sizes,
    "</div>"
  )

  # ---- LEAFLET MAP ----

  map <- leaflet(df_map, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom)

  # Region outline if relevant
  if (tab_typology == "region"){
    map <- map |>
      addPolygons(
        data = admin_region |> filter(INSEE_REG == selected_territory),
        fill = FALSE,
        color = "#444444",
        weight = 1.5,
        opacity = 0.8
      )
  }

  # France outline if relevant
  if (tab_typology == "france"){
    map <- map |>
      addPolygons(
        data = contour_metropole,
        color = "#000000", weight = 1.5, opacity = 0.6,
        fill = FALSE
      )
  }

  if (nrow(df_map) > 0){
    map <- map |>
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~radius,
        fillColor = ~color,
        color = "black",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.8,
        label = lapply(df_map$tooltip_html, htmltools::HTML)
      ) |>
      addControl(
        html = legend_all,
        position = "bottomleft"
      )
    }

  # Add select input to set the view on a DROM
  select_width <- 200
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



#' @title Plot Shared and Analyzed Bike Counters by Territory and Commune Type
#'
#' @description This function generates an interactive horizontal bar chart showing the number of bike counters
#' that are shared and analyzed within a specified territory. Bars are colored according to the
#' type of commune (milieu), and a textual annotation displays the total counts above each bar.
#' The function also calculates the percentage of analyzed counters relative to shared counters.
#'
#' @param data A data frame containing bike counter data. Must include at least the columns:
#'   \code{site_id}, \code{milieu}, \code{region}, and columns of the form \code{analyse_<year>}
#'   indicating if the counter was analyzed in a given year.
#' @param current_year Numeric or character. The year to filter analyzed counters (used to reference
#'   the column \code{analyse_<current_year>}).
#' @param tab_typology Character. A `character` string indicating the typology of the view.
#'   Must be either `"france"` (national chart with multiple typologies)
#'   or `"region"` (chart for a single region)
#' @param territory_name Character. Name of the region or territory to filter data when
#'   \code{tab_typology == "region"}.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{plot}{A \code{plotly} interactive chart showing shared and analyzed counters by commune type.}
#'     \item{pct_analyzed}{Numeric, the percentage of analyzed counters relative to shared counters.}
#'   }
#'
#' @import ggplot2
#' @importFrom dplyr filter group_by summarise distinct mutate bind_rows ungroup
#' @importFrom rlang sym
#' @importFrom stringr str_to_lower
#' @importFrom plotly ggplotly layout
#' @noRd
plot_analyzed_counters <- function(data, current_year, tab_typology, territory_name){

  # Filter by region if needed
  if (tab_typology == "region"){
    data <- data |> filter(region == territory_name)
  }

  if(nrow(data) == 0){
    return(list(plot = plot_message_no_data("Aucune donnée de comptage vélo partagée pour ce territoire", 4.5,
                                            "", x_title = 0, y_title = 0, size_title = 0,
                                            x_coord = 0.5, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0),
                pct_analyzed = "-"))
  }

  # Compute total number of shared and analyzed counters
  n_shared <- data |> distinct(site_id) |> nrow()
  n_analyzed <- data |> filter(!!sym(paste0("analyse_", current_year))) |> distinct(site_id) |> nrow()

  pct_analyzed <- n_analyzed * 100 / n_shared

  # Compute number of shared and analyzed counters by milieu
  shared_counters <- data |>
    group_by(milieu) |>
    summarise(n = n())

  analyzed_counters <- data |>
    filter(!!sym(paste0("analyse_", current_year))) |>
    group_by(milieu) |>
    summarise(n = n())

  # Merge and prepare plotting
  shared_counters <- shared_counters |>
    mutate(type = "Shared")

  analyzed_counters <- analyzed_counters |>
    mutate(type = "Analyzed")

  df_combined <- bind_rows(shared_counters, analyzed_counters) |>
    mutate(tooltip = paste0("<span style='font-weight:bold'>", n, "</span> compteurs ",
                            ifelse(type == "Shared", "partagés ", "analysés "),
                            "dans les <span style='font-weight:bold'>", str_to_lower(milieu), "</span>",
                            " en ", current_year))

  # ggplot chart
  g <- ggplot(df_combined, aes(x = type, y = n, fill = milieu,
                               text = tooltip)) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(aes(label = n),
              position = position_stack(vjust = 0.5), color = "white", size = 3.5) +
    coord_flip() +
    scale_fill_manual(values = c(
      "Grands centres urbains" = "#E94F35",
      "Communes intermédiaires" = "#9185BE",
      "Communes rurales" = "#C6CE41"
    )) +
    scale_x_discrete(expand = c(0.6, 0)) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )

  # Plotly chart
  p <- ggplotly(g, tooltip = "text") %>%
    layout(
      annotations = list(
        list(
          x = 0.045,
          y = 0.95,
          xref = "paper",
          yref = "paper",
          text = paste0(n_shared, " compteurs partagés"),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 15)
        ),
        list(
          x = 0.045,
          y = 0.43,
          xref = "paper",
          yref = "paper",
          text = paste0(n_analyzed, " compteurs analysés"),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 15)
        )
      )
    )

  return(list(plot = p, pct_analyzed = pct_analyzed))

}
