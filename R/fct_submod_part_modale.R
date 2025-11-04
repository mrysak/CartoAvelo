#' @title Compute modal share distribution by department within a selected region
#'
#' @description This function produces a dataset containing the modal share (part modale)
#' of different transport modes for commuting trips, for all departments within a
#' selected region, the region itself, and the group of regions sharing the same
#' typology (metropolitan vs. overseas regions).
#'
#' The resulting data frame can be directly used to visualize the distribution of modal shares
#' (e.g., with stacked bar charts), showing how each transport mode contributes to commuting mobility
#' in the selected territory.
#'
#' @param part_modale A data frame containing modal share data for multiple years and all territorial levels
#' (from national to commune level). Must include columns:
#' \describe{
#'   \item{insee_niveau}{Territorial level code ("REG", "DEP", "COM", etc.)}
#'   \item{insee_code}{INSEE code identifying the corresponding territory}
#'   \item{annee}{Year of observation}
#'   \item{pm_velo, pm_car, pm_tcommun, pm_pied}{Modal share for cycling, car, public transport, and walking (in proportions, not percentages)}
#'   \item{nb_individu}{Respondents count used for computing weighted averages}
#' }
#' @param data_cat_terr A data frame containing territorial classification information.
#' Must include at least the following columns:
#' \describe{
#'   \item{DEP}{Department code}
#'   \item{REG}{Region code}
#'   \item{LIBDEP}{Department name}
#'   \item{LIBREG}{Region name}
#' }
#' @param current_year Numeric value indicating the most recent year with available modal share data.
#' @param selected_cog Character string indicating the INSEE code of the selected region.
#'
#' @return A tibble containing modal share information and yearly evolution for each
#' department, the selected region, and its category group.
#' The output includes the following columns:
#' \describe{
#'   \item{category}{Territorial name (department, region, or regional category)}
#'   \item{annee}{Reference year}
#'   \item{mode_transport}{Transport mode (factorized and labeled)}
#'   \item{pm}{Modal share (in percentage)}
#'   \item{evol_pm}{Year-over-year change in modal share (percentage points)}
#'   \item{tooltip}{Formatted text description for interactive visualization}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @noRd
pm_distribution_by_departement <- function(part_modale, data_cat_terr, current_year, selected_cog){

  ## Get selected region data
  part_modale_reg <- part_modale |>
    filter(insee_niveau == "REG", insee_code == selected_cog, annee %in% c(current_year - 1, current_year)) |>
    mutate(category = data_cat_terr |> filter(REG == selected_cog) |> distinct(LIBREG) |> pull(),
           category = paste0("<b>", category, "</b>")) |>
    select(category, annee, pm_velo, pm_car, pm_tcommun, pm_pied)


  ## Filter departements belonging to the region (only for Régions de métropole)
  if (!selected_cog %in% c("01", "02", "03", "04", "06")){
    part_modale_dep <- part_modale |>
      filter(insee_niveau == "DEP") |>
      select(-insee_niveau) |>
      rename(DEP = insee_code) |>
      filter(annee %in% c(current_year - 1, current_year)) |>
      left_join(data_cat_terr |> select(DEP, LIBDEP, REG) |> distinct(), by = "DEP") |>
      filter(REG == selected_cog) |>
      rename (category = LIBDEP) |>
      select(category, annee, pm_velo, pm_car, pm_tcommun, pm_pied)

    pm_dep <- rbind(part_modale_dep, part_modale_reg)

  } else {
    pm_dep <- part_modale_reg
  }


  ## If territory has no data, return empty df
  if (nrow(part_modale_reg) == 0){
    return(part_modale_reg)
  }


  ## Compute modal share and evolution by milieu
  pm_dep <- pm_dep |>
    # Compute modal share for other transportation modes
    mutate(
      across(starts_with("pm_"), ~ .x *100, .names = "{.col}"),
      pm_autres = 100 - (pm_velo + pm_car + pm_tcommun + pm_pied)
    ) |>
    group_by(category) |>
    # Compute evolution compared to the previous year
    mutate(
      across(
        starts_with("pm_"),
        ~ .x - dplyr::lag(.x),
        .names = "evolution_{.col}"
      )
    ) |>
    ungroup() |>
    filter(annee == current_year)


  ## Compute global modal share in regions within the same category
  # Filter
  if (selected_cog %in% c("01", "02", "03", "04", "06")){
    list_reg_category <- c("01", "02", "03", "04", "06")
    reg_category <- "Régions d'outre-mer"
  } else {
    list_reg_category <- c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76", "84", "93", "94")
    reg_category <- "Régions de métropole"
  }

  part_modale_reg_category <- part_modale |>
    filter(insee_niveau == "REG", insee_code %in% list_reg_category, annee %in% c(current_year - 1, current_year)) |>
    mutate(category = reg_category) |>
    select(category, annee, nb_individu, pm_velo, pm_car, pm_tcommun, pm_pied)

  # Compute modal share and evolution
  pm_reg_category <- part_modale_reg_category |>
    group_by(category, annee) |>
    # Compute global modal share by year
    summarise(
      across(starts_with("pm_"), ~ weighted.mean(.x, nb_individu, na.rm = TRUE) * 100),
      .groups = "drop"
    ) |>
    arrange(category, annee) |>
    # Compute modal share for other transportation modes
    mutate(pm_autres = 100 - (pm_velo + pm_car + pm_tcommun + pm_pied)) |>
    group_by(category) |>
    # Compute evolution compared to the previous year
    mutate(
      across(
        starts_with("pm_"),
        ~ .x - dplyr::lag(.x),
        .names = "evolution_{.col}"
      )
    ) |>
    ungroup() |>
    filter(annee == current_year)


  ## Merge with departement data
  pm_dep <- rbind(pm_dep, pm_reg_category)


  ## Further formatting
  pm_long <- pm_dep |>
    select(category, annee, starts_with("pm_")) |>
    pivot_longer(
      cols = starts_with("pm_"),
      names_to  = "mode_transport",
      names_prefix = "pm_",
      values_to = "pm"
    )

  evol_long <- pm_dep |>
    select(category, annee, starts_with("evolution_pm_")) |>
    pivot_longer(
      cols = starts_with("evolution_pm_"),
      names_to = "mode_transport",
      names_prefix = "evolution_pm_",
      values_to = "evol_pm"
    )

  data_plot <- pm_long |>
    left_join(evol_long, by = c("category", "annee", "mode_transport")) |>
    mutate(
      mode_transport = case_when(
        mode_transport == "velo"   ~ "Vélo",
        mode_transport == "car"    ~ "Voiture",
        mode_transport == "tcommun"~ "Transports en commun",
        mode_transport == "pied"   ~ "Marche",
        mode_transport == "autres" ~ "Autres",
        TRUE ~ mode_transport
      ),
      mode_transport = factor(mode_transport,
                              levels = c("Autres", "Voiture", "Transports en commun", "Marche", "Vélo")),
      category = factor(category, levels = rev(unique(category)))
    )


  ## Tooltip
  data_plot <- data_plot |>
    mutate(
      prefix = case_when(
        str_detect(category, "<b>") ~ paste0("En région ", category),
        str_detect(category, "Régions") ~ paste0("Dans les ", str_to_lower(category)),
        TRUE ~ paste0("Dans le département ", category)
      ),
      tooltip = paste0(prefix, ", le mode de transport <span style='font-weight:bold'>",
                       str_to_lower(mode_transport), "</span>\nreprésente <span style='font-weight:bold'>",
                       format(round(pm, 1), decimal.mark = ",", trim = TRUE), "%</span> des déplacements domicile-travail.\n",
                       "Évolution par rapport à ", current_year - 1, " : ",
                       ifelse(evol_pm >= 0, "+", ""), "<span style='font-weight:bold'>",
                       format(round(evol_pm, 2), decimal.mark = ",", trim = TRUE),
                       ifelse(evol_pm < 2, " point", "points"), "</span>")
    )

  return(data_plot)
}



#' @title Compute modal share distribution by INSEE density category within a selected territory
#'
#' @description This function produces a dataset containing the modal share (part modale)
#' of different transport modes for commuting trips, aggregated by INSEE commune density
#' categories within a selected territorial unit commune, EPCI, département, or région).
#'
#' The resulting data frame can be directly used to visualize the distribution of modal shares
#' (e.g., with stacked bar charts), showing how transport mode usage varies by commune density class
#' in the selected territory.
#'
#' @param part_modale A data frame containing modal share data for multiple years and all territorial levels
#' (from national to commune level). Must include columns:
#' \describe{
#'   \item{insee_niveau}{Territorial level code ("REG", "DEP", "EPCI", "COM", etc.)}
#'   \item{insee_code}{INSEE code identifying the corresponding territory}
#'   \item{annee}{Year of observation}
#'   \item{pm_velo, pm_car, pm_tcommun, pm_pied}{Modal share for cycling, car, public transport, and walking (in proportions, not percentages)}
#'   \item{nb_individu}{Respondents count used for computing weighted averages}
#' }
#' @param data_cat_terr A data frame mapping communes to their upper-level territorial units and INSEE density class.
#' Must include at least the following columns:
#' \describe{
#'   \item{COM}{Commune code}
#'   \item{EPCI}{EPCI code}
#'   \item{DEP}{Department code}
#'   \item{REG}{Region code}
#'   \item{LIB_CAT_COM}{INSEE density category label (e.g., "Grands centres urbains", "Bourgs ruraux")}
#' }
#' @param current_year Numeric value indicating the most recent year with available modal share data.
#' @param tab_typology Character string indicating the territorial level of analysis.
#' Accepted values: `"france"`, `"region"`, `"departement"`, `"epci"`, `"commune"`.
#' @param selected_cog Character string indicating the INSEE code of the selected territory
#' (region, department, EPCI, or commune depending on \code{tab_typology}).
#' If \code{tab_typology = "france"}, the entire dataset is considered.
#'
#' @return A tibble containing modal share information and yearly evolution for each
#' INSEE commune density category within the selected area.
#' The output includes the following columns:
#' \describe{
#'   \item{category}{INSEE commune density category label}
#'   \item{annee}{Reference year}
#'   \item{mode_transport}{Transport mode (factorized and labeled)}
#'   \item{pm}{Modal share (in percentage)}
#'   \item{evol_pm}{Year-over-year change in modal share (percentage points)}
#'   \item{tooltip}{Formatted text description for interactive visualization}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @noRd
pm_distribution_by_milieu <- function(part_modale, data_cat_terr, current_year, tab_typology, selected_cog){

  ## Quick preprocessing and formatting
  part_modale_com <- part_modale |>
    filter(insee_niveau == "COM") |>
    select(-insee_niveau) |>
    rename(COM = insee_code) |>
    left_join(data_cat_terr |> select(COM, EPCI, DEP, REG, LIB_CAT_COM), by = "COM") |>
    rename(category = LIB_CAT_COM) |>
    filter(annee %in% c(current_year - 1, current_year))


  ## Filter selected_territory if needed
  col_insee <- switch(tab_typology,
                      region = "REG",
                      departement = "DEP",
                      epci = "EPCI",
                      commune = "COM")


  if (tab_typology != "france"){
    part_modale_com <- part_modale_com |>
      filter(!!sym(col_insee) == selected_cog)
  }

  if (tab_typology == "commune"){
    com_name <- data_cat_terr |> filter(COM == selected_cog) |> pull(LIBCOM)
  }


  ## Compute modal share and evolution by milieu
  pm_by_density <- part_modale_com |>
    group_by(category, annee) |>
    # Compute global modal share by INSEE density level and by year
    summarise(
      across(starts_with("pm_"), ~ weighted.mean(.x, nb_individu, na.rm = TRUE) * 100),
      .groups = "drop"
    ) |>
    arrange(category, annee) |>
    # Compute modal share for other transportation modes
    mutate(pm_autres = 100 - (pm_velo + pm_car + pm_tcommun + pm_pied)) |>
    group_by(category) |>
    # Compute evolution compared to the previous year
    mutate(
      across(
        starts_with("pm_"),
        ~ .x - dplyr::lag(.x),
        .names = "evolution_{.col}"
      )
    ) |>
    ungroup() |>
    filter(annee == current_year)


  ## Further formatting
  pm_long <- pm_by_density |>
    select(category, annee, starts_with("pm_")) |>
    pivot_longer(
      cols = starts_with("pm_"),
      names_to  = "mode_transport",
      names_prefix = "pm_",
      values_to = "pm"
    )

  evol_long <- pm_by_density |>
    select(category, annee, starts_with("evolution_pm_")) |>
    pivot_longer(
      cols = starts_with("evolution_pm_"),
      names_to = "mode_transport",
      names_prefix = "evolution_pm_",
      values_to = "evol_pm"
    )

  data_plot <- pm_long |>
    left_join(evol_long, by = c("category", "annee", "mode_transport")) |>
    mutate(
      mode_transport = case_when(
        mode_transport == "velo"   ~ "Vélo",
        mode_transport == "car"    ~ "Voiture",
        mode_transport == "tcommun"~ "Transports en commun",
        mode_transport == "pied"   ~ "Marche",
        mode_transport == "autres" ~ "Autres",
        TRUE ~ mode_transport
      ),
      mode_transport = factor(mode_transport,
                              levels = c("Autres", "Voiture", "Transports en commun", "Marche", "Vélo")),
      category = factor(category, levels = c("Rural à habitat très dispersé", "Rural à habitat dispersé", "Bourgs ruraux", "Ceintures urbaines",
                                             "Petites villes", "Centres urbains intermédiaires", "Grands centres urbains"))
    )


  ## Tooltip
  data_plot <- data_plot |>
    mutate(
      prefix = case_when(
        category %in% c("Rural à habitat dispersé", "Rural à habitat très dispersé") ~ paste0("En milieu ", str_to_lower(category)),
        TRUE ~ paste0("Dans les ", str_to_lower(category), " du territoire"))
    )

  if (tab_typology == "commune"){
    data_plot <- data_plot |> mutate(prefix = paste0("À ", com_name), category = com_name)
  }

  data_plot <- data_plot |>
    mutate(
      tooltip = paste0(prefix, ", le mode de transport <span style='font-weight:bold'>",
                       str_to_lower(mode_transport), "</span>\nreprésente <span style='font-weight:bold'>",
                       format(round(pm, 1), decimal.mark = ",", trim = TRUE), "%</span> des déplacements domicile-travail.\n",
                       "Évolution par rapport à ", current_year - 1, " : ",
                       ifelse(evol_pm >= 0, "+", ""), format(round(evol_pm, 2), decimal.mark = ",", trim = TRUE),
                       ifelse(evol_pm < 2, " point", "points"))
    )

  return(data_plot)
}



#' @title Plot modal share distribution by commune density or department
#'
#' @description This function generates an interactive stacked bar chart
#' showing the distribution of commuting modal shares either by INSEE commune
#' density category or by department, depending on the input data.
#' Each horizontal bar represents a territorial category, subdivided by transport mode.
#' The chart includes percentage labels and interactive tooltips describing each segment.
#'
#' @param data_plot A tibble or data frame structured for visualization, typically the output
#' of \code{\link{pm_distribution_by_milieu}} or \code{\link{pm_distribution_by_departement}}.
#' Must contain at least the following columns:
#' \describe{
#'   \item{category}{Territorial category (e.g., INSEE density level or department name)}
#'   \item{mode_transport}{Transport mode label (e.g., "Voiture", "Vélo")}
#'   \item{pm}{Modal share in percentage points}
#'   \item{tooltip}{HTML-formatted tooltip text for interactive display}
#' }
#'
#' @return An interactive \code{plotly} object representing a horizontal stacked bar chart,
#' ready to be rendered in a Shiny app.
#'
#' @import ggplot2
#' @import plotly
#' @import dplyr
#' @noRd
plot_pm_distribution_by_milieu_departement <- function(data_plot){

  ## If territory has no data, return message
  if (nrow(data_plot) == 0){
    return(plot_message_no_data("Pas de données de part modale\nexistantes pour ce territoire", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.4, y_coord = 0.95, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  }

  ## Height threshold for bar labels display
  seuil <- 4.5

  data_plot <- data_plot |>
    mutate(label = case_when(
      pm == 0 ~ "",
      pm < seuil ~ "  ",
      TRUE ~ paste0(round(pm, 0), "%")
    ))


  ## Color palette
  color_palette <- setNames(c("#cacaca", "#8e8f91", "#294754", "#B1D6E4", "#C6CE41"),
                            c("Autres", "Voiture", "Transports en commun", "Marche", "Vélo"))

  ## ggplot_chart
  p <- ggplot(data_plot, aes(x = pm, y = category, fill = mode_transport, text = tooltip)) +
    geom_col(position = "stack", width = 0.7) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white", size = 2.8) +
    scale_fill_manual(values = color_palette) +
    labs(
      x = "", y = "", title = "", fill = "Mode de déplacement"
    ) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 0, l = -10),
      axis.title = element_text(color = "#294754"),
      axis.text = element_text(color = "#294754"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 10, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    )


  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      showlegend = FALSE
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



#' @title Generate a tooltip label for a map displaying modal shares
#'
#' @description Creates a formatted HTML tooltip label string to display on a leaflet
#' map, showing the indicator value and its evolution from the previous year.
#'
#' @param value Numeric. The current value of the selected indicator, or its evolution.
#' @param label Numeric. Additional information to display in the label.
#' @param info_to_display Character. Either `"value"` or `"evol"` to determine which value to display as the main value.
#' @param type_transport Character. Transport type selected by the user.
#' @param current_year Character. The latest year with data.
#'
#' @return A character string with HTML formatting, ready to be used in a leaflet tooltip.
#' The string includes the main value, unit, and the evolution from the previous year.
#'
#' @noRd
tooltip_label_part_modale <- function(value, label, info_to_display, type_transport, current_year){

  general_label <- switch(type_transport,
                          velo = "Part modale du vélo dans les<br>déplacements domicile-travail : <span style='font-weight:bold'>",
                          tcommun = "Part modale des transports en commun dans les<br>déplacements domicile-travail : <span style='font-weight:bold'>",
                          car = "Part modale de la voiture dans les<br>déplacements domicile-travail : <span style='font-weight:bold'>",
                          pied = "Part modale de la marche dans les<br>déplacements domicile-travail : <span style='font-weight:bold'>"
  )

  # Format value
  val <- ifelse(info_to_display == "value", value, label)
  val <- ifelse(is.na(val), "-", format(round(val, 1), decimal.mark = ",", trim = TRUE))

  # Format evolution
  evol <- ifelse(info_to_display == "value", label, value)
  prefix_evol <- ifelse(!is.na(evol) && evol >= 0, "+", "")
  if (is.na(evol)){unit_evol <- ""} else {unit_evol <- ifelse(evol <= 2, " point", " points")}
  evol <- ifelse(is.na(evol), "-", format(round(evol, 2), decimal.mark = ",", trim = TRUE))

  legend <- paste0(general_label, val, "%</span><br>",
                   "Évolution /", current_year - 1, " : <span style='font-weight:bold'>",
                   prefix_evol, evol, unit_evol, "</span>")

  return(legend)
}



#' @title Interactive Map of Modal Share by Transport Mode in France
#'
#' @description This function generates an interactive **Leaflet map** visualizing
#' the modal share of different transport modes (e.g., cycling, car, walking, public
#' transport) for commuting trips across French administrative territories (communes,
#' EPCI, departments, or regions).
#' The map can display either the **current modal share (in %)** or its **evolution (in points)**
#' between two consecutive years. It automatically adjusts the zoom level and view
#' depending on the selected administrative level and territory.
#'
#' @param data_cat_terr A data frame containing territorial classification metadata.
#'   Each row corresponds to a commune and includes administrative codes (EPCI, department, region)
#'   and density classification (INSEE grid of density).
#' @param part_modale A data frame containing modal share data across years
#'   and administrative levels. Must include:
#'   - `insee_code`: INSEE code of the unit,
#'   - `insee_niveau`: administrative level (e.g., "REG", "DEP", "EPCI", "COM"),
#'   - `annee`: year,
#'   - `nb_individu`: population count used for weighting,
#'   - `pm_velo`, `pm_car`, `pm_tcommun`, `pm_pied`: modal shares (in proportions or %).
#' @param admin_sf An `sf` object containing the geometries of the administrative units
#'   to be displayed, matching the administrative level given by `display_typology`.
#' @param tab_typology Character string defining the typology of the **selected territory**
#'   (`"france"`, `"region"`, `"departement"`, `"epci"`, or `"commune"`).
#'   This determines which area the user has selected for display.
#' @param display_typology Character string defining the typology of the **display scale**.
#'   Determines at what administrative level (`"region"`, `"departement"`, `"epci"`, or `"commune"`)
#'   the map will display modal shares.
#' @param selected_territory Character string giving the INSEE code of the selected
#'   territory (region, department, EPCI, or commune) for which data should be shown.
#' @param type_transport Character string indicating the transport mode to display.
#'   Possible values are `"velo"`, `"car"`, `"tcommun"`, `"pied"`.
#' @param info_to_display Character string indicating what should be shown on the map:
#'   - `"value"`: displays the current modal share (in %);
#'   - `"evol"`: displays the evolution in modal share (in percentage points)
#'     between the last two available years.
#' @param current_year Integer. The most recent year for which modal share data is available.
#'
#' @return
#' A **Leaflet map widget** (`leaflet` object) displaying:
#' - The modal share or evolution by administrative unit;
#' - Contextual tooltips with formatted values and text;
#' - A dynamic legend adapted to the transport mode and display type;
#' - Download options for exporting the map in various formats (A4 portrait/landscape or current view).
#'
#' @import dplyr
#' @import sf
#' @import leaflet
#' @import leaflet.extras2
#' @import htmltools
#' @noRd
map_part_modale <- function(data_cat_terr, part_modale, admin_sf,
                            tab_typology, display_typology, selected_territory,
                            type_transport, info_to_display, current_year,
                            admin_cache = NULL, ns){

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

  # Extract data corresponding to the territory selected by the user, according to the selected display scale
  part_modale <- part_modale |>
    filter(insee_niveau == insee_col_cat_terr, annee %in% c(current_year - 1, current_year)) |>
    rename(!!sym(insee_col_admin) := insee_code) |>
    select(all_of(insee_col_admin), "annee", "nb_individu", paste0("pm_", type_transport)) |>
    mutate(!!sym(paste0("pm_", type_transport)) := !!sym(paste0("pm_", type_transport)) * 100)

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

  # Join administrative data with modal share data
  admin_sf <- admin_sf |>
    left_join(part_modale |>  mutate(across(all_of(insee_col_admin), as.character)),
              by = insee_col_admin)

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
  type_transport_label <- switch(type_transport,
                                 velo = "du vélo",
                                 car = "de la voiture",
                                 tcommun = "des transports en commun",
                                 pied = "de la marche")

  legend_title <- case_when(
    info_to_display == "value" ~
      paste0("Part modale ", type_transport_label, " dans les<br>déplacements domicile-travail (%)"),
    info_to_display == "evol"  ~
      paste0("Évolution de la part modale ", type_transport_label, " dans<br>les déplacements domicile-travail (points)"),
    TRUE ~ "Légende"
  )


  # ----- ADDITIONAL DATA PREPARATION -----

  # Compute evolution
  admin_sf <- admin_sf |>
    group_by(!!sym(insee_col_admin)) |>
    mutate(
      across(
        starts_with("pm_"),
        ~ .x - dplyr::lag(.x),
        .names = "evolution_{.col}"
      )
    ) |>
    filter(annee == current_year | is.na(annee))

  # Rename columns
  admin_sf <- admin_sf  |>
    rename(!!ifelse(info_to_display == "value", "value", "label") := !!sym(paste0("pm_", type_transport)),
           !!ifelse(info_to_display == "value", "label", "value") := !!sym(paste0("evolution_pm_", type_transport)))


  # ---- COLOR PALETTE AND LEGEND ----

  if (info_to_display == "value" & type_transport == "velo"){
    color_range <- c("#f6f9ed", "#e8ebb3", "#d7dd7a", "#b2b93b")
    color_min <- "#f6f9ed"
    color_max <- "#b2b93b"
  } else if (type_transport == "pied"){
    color_range <- c("#f1f7fa", "#e0eff4", "#c1dee9", "#9fc1cd")
    color_min <- "#f1f7fa"
    color_max <- "#9fc1cd"
  } else if (type_transport == "tcommun"){
    color_range <- c("#d4dadd", "#94a3aa", "#546c76", "#25404c")
    color_min <- "#d4dadd"
    color_max <- "#25404c"
  } else if (type_transport == "car"){
    color_range <- c("#f4f4f4", "#d2d2d3", "#b0b1b2", "#727274")
    color_min <- "#f4f4f4"
    color_max <- "#727274"
  }

  if (info_to_display == "evol") {
    if (type_transport == "car"){
      color_palette_legend <- color_palette_map_evol(-1, 1,
                                                     c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("< -1 point", "-1 à -0,5 point", "-0,5 à 0 point", "0 à 0,5 point", "0,5 à 1 point", "> 1 point"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    } else {
      color_palette_legend <- color_palette_map_evol(-0.5, 0.5,
                                                     c("#223f82", "#919fc1", "#e9ecf3", "#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("< -0,5 point", "-0,5 à -0,25 point", "-0,25 à 0 point", "0 à 0,25 point", "0,25 à 0,5 point", "> 1 point"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    }
  } else if (info_to_display == "value"){
    if (type_transport == "car" & display_typology == "commune"){
      color_palette_legend <- color_palette_map_value(100,
                                                      color_range,
                                                      c("30 à 50%", "50 à 70%", "70 à 90%", "90 à 100%"),
                                                      color_min, color_max,
                                                      admin_sf, 30)
    } else if (display_typology == "commune"){
      color_palette_legend <- color_palette_map_value(15,
                                                      color_range,
                                                      c("0 à 5%", "5 à 10%", "10 à 15%", "> 15%"),
                                                      color_min, color_max,
                                                      admin_sf)
    } else if (type_transport == "velo"){
      color_palette_legend <- color_palette_map_value(5,
                                                      color_range,
                                                      c("0 à 1%", "1 à 3%", "3 à 5%", "> 5%"),
                                                      color_min, color_max,
                                                      admin_sf)
    } else if (type_transport == "tcommun"){
      color_palette_legend <- color_palette_map_value(20,
                                                      color_range,
                                                      c("0 à 7%", "7 à 15%", "15 à 20%", "> 20%"),
                                                      color_min, color_max,
                                                      admin_sf)
    } else if (type_transport == "pied" & display_typology == "epci"){
      color_palette_legend <- color_palette_map_value(10,
                                                      color_range,
                                                      c("0 à 4%", "4 à 7%", "7 à 10%", "> 10%"),
                                                      color_min, color_max,
                                                      admin_sf)
    } else if (type_transport == "pied"){
      color_palette_legend <- color_palette_map_value(7,
                                                      color_range,
                                                      c("2 à 4%", "4 à 6%", "6 à 7%", "> 7%"),
                                                      color_min, color_max,
                                                      admin_sf, 2)
    } else if (type_transport == "car" & display_typology == "region"){
      color_palette_legend <- color_palette_map_value(80,
                                                      color_range,
                                                      c("50 à 60%", "60 à 70%", "70 à 80%", "> 80%"),
                                                      color_min, color_max,
                                                      admin_sf, 50)
    } else if (type_transport == "car"){
      color_palette_legend <- color_palette_map_value(90,
                                                      color_range,
                                                      c("40 à 60%", "60 à 80%", "80 à 90%", "> 90%"),
                                                      color_min, color_max,
                                                      admin_sf, 40)
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
          tooltip_label_part_modale(val, lab, info_to_display, type_transport, current_year), "</span>"
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
    type_transport %in% c("velo", "pied", "car") & info_to_display == "value" ~ 245,
    TRUE ~ 220)

  if (tab_typology == "france"){
    map <- map |>
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



#' @title Generate value box content for modal share
#'
#' @description This function prepares formatted textual and numerical content
#' to be displayed inside a **Shiny value box**.
#' It summarizes the **modal share** (in %) of a given transport mode
#' and its **year-over-year evolution** for a selected French territory
#' and typology (France, region, department, EPCI, or commune).
#'
#' @param part_modale A data frame containing modal share data across multiple years
#'   and administrative levels. Must include:
#'   - `insee_code`: INSEE code of the administrative unit;
#'   - `insee_niveau`: administrative level (`"NAT"`, `"REG"`, `"DEP"`, `"EPCI"`, `"COM"`);
#'   - `annee`: year of observation;
#'   - `pm_velo`, `pm_car`, `pm_tcommun`, `pm_pied`: modal shares (as proportions, not %).
#' @param current_year Integer. The most recent year of data available.
#' @param tab_typology Character string specifying the territorial level to use.
#'   Possible values:
#'   - `"france"` (national level),
#'   - `"region"`,
#'   - `"departement"`,
#'   - `"epci"`,
#'   - `"commune"`.
#' @param selected_cog Character string corresponding to the **INSEE code**
#'   of the selected administrative unit (region, department, EPCI, or commune).
#'   Ignored when `tab_typology = "france"`.
#' @param type_transport Character string indicating which transport mode
#'   should be displayed. Must be one of:
#'   - `"velo"` (cycling),
#'   - `"car"` (car),
#'   - `"tcommun"` (public transport),
#'   - `"pied"` (walking).
#'
#' @return
#' A **list** with three elements, suitable for populating a Shiny value box:
#' \describe{
#'   \item{`title`}{Character string. The title to display in the value box (with HTML line breaks).}
#'   \item{`value`}{Character string. The formatted modal share for `current_year`, expressed as a percentage with two decimal places.}
#'   \item{`evol`}{Character string. The formatted evolution compared to the previous year, expressed in points.}
#' }
#'
#' @import dplyr
#' @noRd
value_box_content_pm <- function(part_modale, current_year,
                                 tab_typology, selected_cog, type_transport){

  previous_year <- current_year - 1

  ## Filter territory
  if (tab_typology != "france"){
    insee_col <- switch(tab_typology,
                        region = "REG",
                        departement = "DEP",
                        epci = "EPCI",
                        commune = "COM")

    part_modale <- part_modale |>
      filter(insee_niveau == insee_col, annee %in% c(current_year - 1, current_year), insee_code == selected_cog)
  } else if (tab_typology == "france"){
    part_modale <- part_modale |>
      filter(insee_niveau == "NAT", annee %in% c(current_year - 1, current_year))
  }

  ## Extract modal shares values
  pm_current <- part_modale |> filter(annee == current_year) |> pull(!!sym(paste0("pm_", type_transport))) * 100
  pm_previous <- part_modale |> filter(annee == previous_year) |> pull(!!sym(paste0("pm_", type_transport))) * 100

  evol <- pm_current - pm_previous


  ## Format outputs
  title <- switch(type_transport,
                  velo = "Part modale du vélo",
                  car = "Part modale<br>de la voiture",
                  tcommun = "Part modale<br>des transports en commun",
                  pied = "Part modale<br>de la marche")

  if (length(pm_current) == 0){
    value <- "-"
  } else {
    value <- paste0(format(round(pm_current, 2), decimal.mark = ",", trim = TRUE), "%")
  }

  if (length(evol) != 0){
    evol <- paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 2), decimal.mark = ",", trim = TRUE),
                   ifelse(evol <= 2, " point", " points"), " par rapport à ", previous_year)
  }

  return(list(title = title, value = value, evol = evol))
}



#' @title Generate National Comparison Data for Modal Shares
#'
#' @description This function computes comparison statistics for the modal share
#' of a selected transport mode across three categories of French communes:
#' *Grands centres urbains*, *Communes intermédiaires*, and *Communes rurales*.
#'
#' The commune typology is based on `CAT_COM` codes:
#' - `1` for *Grands centres urbains*
#' - `2, 3, 4` for *Communes intermédiaires*
#' - `5, 6, 7` for *Communes rurales*
#'
#' @param data_cat_terr A data frame describing all territories and their categories (e.g., `COM`, `EPCI`, `DEP`, `REG`, and associated category labels).
#' @param part_modale A data frame containing modal share data for multiple years and all territorial levels
#' (from national to commune level).
#' @param current_year Latest year with modal share data.
#' @param start_year First year to include.
#' @param type_transport Character. Transport type selected by the user.
#'
#' @return A data frame with one row per year and commune category (`LIBCAT`), including:
#' - `CAT`: numerical category code
#' - `LIBCAT`: category label (e.g., "Communes rurales")
#' - `year`: year as character
#' - `pm`: computed aggregated value of the modal share
#'
#' @import dplyr
#' @noRd
generate_pm_comparison_data_national <- function(data_cat_terr, part_modale,
                                                 current_year, start_year, type_transport){

  time_period <- seq(2018, current_year)

  # Filter column containing the selected transport mode
  pm_communal <- part_modale |>
    filter(annee %in% time_period, insee_niveau == "COM") |>
    select(insee_niveau, insee_code, annee, nb_individu, all_of(paste0("pm_", type_transport)))

  # Retrieve relevant data and label communes categories
  data_comparison <- data_cat_terr |>
    mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                           CAT_COM %in% c(2, 3, 4) ~ 2,
                           CAT_COM %in% c(5, 6, 7) ~ 3),
           LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                              CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                              CAT_COM %in% c(5, 6, 7) ~ "Communes rurales")) |>
    select(COM, CAT, LIBCAT) |>
    left_join(pm_communal, by = c("COM" = "insee_code"))

  # Compute modal share by milieu and by year
  data_comparison_cat <- data_comparison |>
    filter(!is.na(annee)) |>
    group_by(CAT, LIBCAT, annee) |>
    summarise(
      across(starts_with("pm_"), ~ weighted.mean(.x, nb_individu, na.rm = TRUE) * 100),
      .groups = "drop"
    ) |>
    rename(pm = paste0("pm_", type_transport))

  return(data_comparison_cat)
}



#' @title Generate Local Comparison Data for Modal Shares
#'
#' @description This function computes comparison statistics for the modal share
#' of a selected transport mode across all territories belonging to the same category
#' as a selected territory (e.g., same type of EPCI, département, or region).
#' It enables localized benchmarking by calculating the **minimum**, **maximum**,
#' and **weighted average** of the modal share for comparable territories.
#'
#' The function also returns the formatted label of the comparison group (e.g., "communautés d'agglomération",
#' or "régions d'outre-mer") for use in visualization.
#'
#' @param data_cat_terr A data frame describing all territories and their categories (e.g., `COM`, `EPCI`, `DEP`, `REG`, and associated category labels).
#' @param part_modale A data frame containing modal share data for multiple years and all territorial levels
#' (from national to commune level).
#' @param current_year Latest year with modal share data.
#' @param start_year First year to include.
#' @param type_transport Character. Transport type selected by the user.
#' @param typology A string defining the level of analysis. One of `"commune"`, `"epci"`, `"departement"`, or `"region"`.
#' @param selected_cog A string or code identifying the selected territory (e.g., INSEE code).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`data_comparison_cat`}{A data frame with columns `year`, `LIBCAT` (category: Min, Max, Moy), and `pm` (computed value)}
#'   \item{`lib_category`}{A character string representing the label of the category to which the selected territory belongs, formatted for display (e.g., "communes rurales")}
#' }
#'
#' @import dplyr
#' @noRd
generate_pm_comparison_data_local <- function(data_cat_terr, part_modale,
                                              current_year, start_year, type_transport,
                                              typology, selected_cog){
  time_period <- seq(2018, current_year)

  # Retrieve relevant data and filter to only keep territories belonging to the same category
  colname <- switch(typology,
                    commune = c("COM", "LIB_CAT_COM", "INSEE_COM"),
                    epci = c("EPCI", "NATURE_EPCI", "INSEE_EPCI"),
                    departement = c("DEP", "LIB_CAT_DEP", "INSEE_DEP"),
                    region = c("REG", "LIB_CAT_REG", "INSEE_REG"))

  part_modale <- part_modale |>
    filter(insee_niveau == colname[1], annee %in% time_period) |>
    select(insee_niveau, insee_code, annee, nb_individu, all_of(paste0("pm_", type_transport))) |>
    rename(pm = paste0("pm_", type_transport))

  if (typology == "region"){
    data_cat_terr <- data_cat_terr |>
      mutate(LIB_CAT_REG = ifelse(REG %in% c("01", "02", "03", "04", "06"), "régions d'outre-mer", "régions de métropole"))
  }

  lib_category <- data_cat_terr |> filter(.data[[colname[1]]] == selected_cog) |> pull(.data[[colname[2]]]) |> unique()

  data_comparison <- data_cat_terr |>
    mutate(!!sym(colname[1]) := as.character(!!sym(colname[1]))) |>
    filter(.data[[colname[2]]] == lib_category) |>
    select(all_of(colname[1])) |>
    unique() |>
    left_join(part_modale,
              by = setNames("insee_code", colname[1]))

  # Compute modal share (min, max and weighted average) of the category by year
  data_comparison_cat <- data_comparison |>
    filter(!is.na(annee)) |>
    group_by(annee) |>
    summarise(
      Moy = sum(pm * nb_individu, na.rm = TRUE) / sum(nb_individu, na.rm = TRUE) * 100,
      Min = min(pm, na.rm = TRUE) * 100,
      Max = max(pm, na.rm = TRUE) * 100,
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c("Moy", "Min", "Max"),
      names_to = "LIBCAT",
      values_to = "pm"
    )

  lib_category <- transform_category(lib_category)

  return(list(data_comparison_cat = data_comparison_cat, lib_category = lib_category))
}



#' @title Plot the Evolution of Modal Shares with Comparison
#'
#' @description This function generates an interactive `plotly` bar and line chart showing the temporal evolution
#' of modal shares for a selected territory. It also overlays comparative reference lines from other territories in
#' the same category (e.g., similar communes, EPCI, or regions).
#'
#' @param data A data frame containing annual values of the modal share for the selected territory.
#' @param data_comparison_cat A data frame containing modal shares for the comparison categories (e.g., min, max, average of similar territories).
#' @param type_transport Character. Transport type selected by the user.
#' @param start_year First year to include.
#' @param current_year Latest year with modal share data.
#' @param selected_territory A character string naming the territory of interest (e.g., a commune, EPCI, département, or région).
#' @param typology A string indicating the territory typology. Typically `"france"` for national categories (e.g., urban/rural), or a grouping like `"region"`, `"epci"`, etc.
#' @param lib_category A label identifying the category or group the selected territory belongs to (e.g., "Bourgs ruraux") for display purposes.
#'
#' @return A `plotly` interactive chart combining:
#' - A bar chart showing the modal share value over time for the selected territory.
#' - One or more line plots representing comparison benchmarks (e.g., min, max, average).
#' - Customized tooltips with units, evolution rates, and contextual information.
#'
#' @importFrom dplyr select mutate left_join filter pull case_when all_of
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove str_to_lower
#' @importFrom ggplot2 ggplot aes geom_bar geom_line geom_point scale_color_manual scale_fill_manual labs theme_minimal theme element_blank element_text margin
#' @importFrom plotly ggplotly layout style
#' @noRd
plot_evolution_part_modale <- function(data, data_comparison_cat,
                                       type_transport, start_year, current_year,
                                       selected_territory, typology, lib_category){

  indicator_title <- switch(type_transport,
                            velo    = "Part modale du vélo",
                            car     = "Part modale de la voiture",
                            pied    = "Part modale de la marche",
                            tcommun = "Part modale des transports en commun")

  ## Extract modal share of the selected transport type
  data_to_display <- data |>
    filter(annee %in% seq(start_year, current_year)) |>
    select(annee, nb_individu, paste0("pm_", type_transport)) |>
    rename(pm = paste0("pm_", type_transport)) |>
    mutate(pm = pm * 100)


  ## Custom tooltip for main data, with evolution

  # Compute evolution
  data_to_display <- data_to_display |>
    mutate(
      across(
        pm,
        ~ .x - dplyr::lag(.x),
        .names = "evol"
      )
    )

  # Create tooltip
  data_to_display <- data_to_display |>
    mutate(
      tooltip = ifelse(annee == start_year,
                       paste0(indicator_title,
                              " en ", annee, " : <span style='font-weight:bold'>",
                              format(round(pm, 2), decimal.mark = ",", trim = TRUE), "%</span>"),
                       paste0(indicator_title,
                              " en ", annee, " : <span style='font-weight:bold'>",
                              format(round(pm, 2), decimal.mark = ",", trim = TRUE), "%</span>\n",
                              "Évolution par rapport à ", annee - 1, " : <span style='font-weight:bold'>",
                              ifelse(!is.finite(evol),
                                     "-",
                                     paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 2), decimal.mark = ",", trim = TRUE),
                                            ifelse(abs(evol) < 2, " point", " points"), "</span>"))))
    ) |>
    mutate(territoire = selected_territory)


  ## Custom tooltip and colors for comparison data
  if (typology == "france"){
    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(indicator_title,
                         " dans les\n", str_to_lower(LIBCAT),
                         " en ", annee, " : <span style='font-weight:bold'>",
                         format(round(pm, 2), decimal.mark = ",", trim = TRUE), "%</span>")
      )

    custom_colors <- c(
      "Grands centres urbains" = "#E94F35",
      "Communes intermédiaires" = "#9185BE",
      "Communes rurales" = "#C6CE41"
    )
  } else {
    data_comparison_cat <- data_comparison_cat |>
      mutate(
        tooltip = paste0(case_when(LIBCAT == "Moy" ~ "Moyenne pondérée de la ",
                                   LIBCAT == "Min" ~ "Minimum de la ",
                                   LIBCAT == "Max" ~ "Maximum de la ",
                                   TRUE ~ ""),
                         str_to_lower(indicator_title), "\ndans les ", str_to_lower(lib_category),
                         " en ", annee, " : <span style='font-weight:bold'>",
                         format(round(pm, 2), decimal.mark = ",", trim = TRUE), "%</span>")
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
  y_label <- paste0(indicator_title, " (%)")


  ## ggplot chart
  p <- ggplot(data_to_display, aes(x = annee, y = pm, text = tooltip)) +
    geom_bar(data = data_to_display,
             aes(fill = territoire),
             stat = "identity", width = 0.4) +
    geom_line(data = data_comparison_cat,
              aes(x = annee, y = pm, group = LIBCAT, color = LIBCAT, text = tooltip),
              inherit.aes = FALSE,
              linewidth = 1) +
    geom_point(data = data_comparison_cat,
               aes(x = annee, y = pm, group = LIBCAT, color = LIBCAT, text = tooltip),
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
          paste0(indicator_title, " dans les déplacements domicile-travail"),
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
    g <- g |>
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 4, showlegend = TRUE) %>%
      style(name = line_categories[2], traces = 2, showlegend = TRUE) %>%
      style(name = line_categories[3], traces = 3, showlegend = TRUE)
  } else {
    g <- g |>
      style(name = selected_territory, traces = 1, showlegend = TRUE) %>%
      style(name = line_categories[3], traces = 2, showlegend = TRUE) %>%
      style(name = line_categories[2], traces = 3, showlegend = TRUE) %>%
      style(name = line_categories[1], traces = 4, showlegend = TRUE)
  }

  return(g)
}



#' @title Visualize modal share by age group and commune density
#'
#' @description This function generates an interactive heatmap showing the modal share
#' of a given transport mode across age groups, and — when relevant — across commune density
#' (urban, intermediate, rural).
#' It combines modal share data (`distrib_age_pm`) with territorial typology data
#' (`data_cat_terr`), adapting the visualization according to the selected territorial level
#' (`France`, `region`, `department`, `EPCI`, or `commune`).
#'
#' @param distrib_age_pm `data.frame` Dataset containing modal shares by age group,
#' INSEE level and code. Must include at least the following columns:
#' `insee_niveau`, `insee_code`, `age`, `nb_individu`, and `pm_<type_transport>`.
#' @param data_cat_terr `data.frame` Territorial categorization dataset (e.g. INSEE
#' density classification) containing mappings between communes (`COM`) and higher levels
#' (`EPCI`, `DEP`, `REG`), as well as a variable `CAT_COM` defining urban/rural typology.
#' @param tab_typology `character` Territorial level of analysis. Must be one of:
#' `"france"`, `"region"`, `"departement"`, `"epci"`, or `"commune"`.
#' @param selected_cog `character` INSEE code of the selected territory.
#' Ignored when `tab_typology == "france"`.
#' @param territory_name `character` Human-readable name of the selected territory.
#' @param type_transport `character` Type of transport mode to display.
#' Must match a suffix in the dataset columns, e.g. `"velo"`, `"car"`, `"tcommun"`, or `"pied"`.
#'
#' @return
#' A `plotly` object (interactive heatmap) showing:
#' - Rows: commune density (or the selected territory);
#' - Columns: age groups;
#' - Fill color: modal share (%) of the selected transport mode.
#' Tooltips display detailed values when hovering over tiles.
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @importFrom stringr str_to_lower
#' @noRd
plot_distrib_age_pm <- function(distrib_age_pm, data_cat_terr,
                                tab_typology, selected_cog, territory_name,
                                type_transport){

  # --- Modal share by age for the selected territory ---

  ## Get data for the selected territory
  col_insee <- switch(tab_typology,
                      region = "REG",
                      departement = "DEP",
                      epci = "EPCI",
                      commune = "COM")


  if (tab_typology != "france"){
    distrib_age_territory <- distrib_age_pm |>
      filter(insee_niveau == col_insee & insee_code == selected_cog)
  } else {
    distrib_age_territory <- distrib_age_pm |>
      filter(insee_niveau == "NAT")
  }

  ## If territory has no data, print message
  if (nrow(distrib_age_territory) == 0 & type_transport == "velo"){
    return(plot_message_no_data("Pas de données de part modale\nexistantes pour ce territoire", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.45, y_coord = 0.85, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  } else if (nrow(distrib_age_territory) == 0){
    return(plot_message_no_data("", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.45, y_coord = 0.65, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  }

  ## Shorten EPCI names
  territory_name <- str_replace_all(territory_name,
                                    c("Communauté d'agglomération" = "CA",
                                      "Communauté urbaine" = "CU",
                                      "Communauté de communes" = "CC"))
  territory_name <- insert_linebreak_middle(territory_name, 23)

  ## Quick formatting
  distrib_age_territory <- distrib_age_territory |>
    mutate(category = paste0("<b>", territory_name, "</b>")) |>
    select(category, age, all_of(paste0("pm_", type_transport))) |>
    rename(pm = !!sym(paste0("pm_", type_transport))) |>
    mutate(pm := pm * 100)

  ## Tooltip
  label_type_transport <- switch(type_transport,
                                 "velo"    = "du vélo",
                                 "car"     = "de la voiture",
                                 "tcommun" = "des transports en commun",
                                 "pied"    = "de la marche")

  data_plot <- distrib_age_territory |>
    mutate(tooltip = paste0("Part modale ", label_type_transport, "\ndes ",
                            case_when(age == "15-24" ~ "15-24 ans",
                                      age == "25-39" ~ "25-39 ans",
                                      age == "40-59" ~ "40-59 ans",
                                      age == "60+" ~ "60 ans et plus"),
                            " : <span style='font-weight:bold'>",
                            format(round(pm, 1), decimal.mark = ",", trim = TRUE), "%</span>"))


  # --- Modal share by age and by milieu ---

  if (tab_typology %in% c("departement", "region", "france")){

    ## Join modal share data by commune with territorial infos (EPCI, dep, reg and category)
    data_cat_terr <- data_cat_terr |>
      mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                             CAT_COM %in% c(2, 3, 4) ~ 2,
                             CAT_COM %in% c(5, 6, 7) ~ 3),
             LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                                CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                                CAT_COM %in% c(5, 6, 7) ~ "Communes rurales"))

    distrib_age_com <- distrib_age_pm |>
      filter(insee_niveau == "COM") |>
      select(-insee_niveau) |>
      select(insee_code, age, nb_individu, all_of(paste0("pm_", type_transport))) |>
      rename(COM = insee_code) |>
      left_join(data_cat_terr |> select(COM, EPCI, DEP, REG, LIBCAT), by = "COM") |>
      rename(category = LIBCAT)

    ## Filter selected_territory if needed
    if (tab_typology != "france"){
      distrib_age_territory <- distrib_age_com |>
        filter(!!sym(col_insee) == selected_cog)
    } else {
      distrib_age_territory <- distrib_age_com
    }

    ## Compute modal share by milieu
    distrib_age_milieu <- distrib_age_territory |>
      group_by(category, age) |>
      # Compute global modal share by INSEE density level and by age
      summarise(
        across(starts_with("pm_"), ~ weighted.mean(.x, nb_individu, na.rm = TRUE) * 100),
        .groups = "drop"
      ) |>
      rename(pm = !!sym(paste0("pm_", type_transport)))

    ## Tooltip
    distrib_age_milieu <- distrib_age_milieu |>
      mutate(
        category = factor(category, levels = c("Communes rurales", "Communes intermédiaires",  "Grands centres urbains")),
        tooltip = paste0("Part modale ", label_type_transport, "\ndes ",
                         case_when(age == "15-24" ~ "15-24 ans",
                                   age == "25-39" ~ "25-39 ans",
                                   age == "40-59" ~ "40-59 ans",
                                   age == "60+" ~ "60 ans et plus"),
                         " dans les\n", str_to_lower(category), " : <span style='font-weight:bold'>",
                         format(round(pm, 1), decimal.mark = ",", trim = TRUE), "%</span>")
      ) |>
      arrange(category)

    ## Merge
    data_plot <- rbind(data_plot, distrib_age_milieu)
  }

  data_plot <- data_plot |>
    mutate(category = factor(category, levels = c(paste0("<b>", territory_name, "</b>"), "Communes rurales", "Communes intermédiaires",  "Grands centres urbains"))
    )

  # --- Plot ---

  colors <- switch(type_transport,
                   velo    = c("#f9faec", "#C6CE41", "grey20"),
                   car     = c("#f4f4f4", "#8e8f91", "grey20"),
                   tcommun = c("#d4dadd", "#294754", "white"),
                   pied    = c("#f7fbfc", "#B1D6E4", "grey20"))

  val_min <- min(data_plot$pm)
  val_max <- max(data_plot$pm)

  p <- ggplot(data_plot, aes(x = age, y = category, fill = pm, text = tooltip)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(format(round(pm, 1), decimal.mark = ","), "%")), color = colors[3], size = 3.2) +
    scale_fill_gradient(low = colors[1], high = colors[2], limits = c(val_min, val_max)) +
    theme_minimal(base_size = 15, base_family = "Poppins") +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.position = "none",
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.5, color = "#294754", size = 13),
      axis.text.y = if (type_transport == "velo") {
        element_text(size = 10, color = "#294754")
      } else {
        element_blank()
      },
      axis.text.x = element_text(size = 10, color = "#294754"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    ggtitle(switch(type_transport,
                   velo    = "Vélo",
                   car     = "Voiture",
                   tcommun = "Transports en commun",
                   pied    = "Marche"))

  ## Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
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



#' @title Plot Gender Distribution by Transport Mode
#'
#' @description Generates an interactive stacked bar chart showing the proportion
#' of male and female users for each transport mode and for different territorial categories.
#'
#' @param distrib_age_pm `data.frame` Dataset containing modal shares by gender,
#' INSEE level and code. Must include at least the following columns:
#' `insee_niveau`, `insee_code`, `sexe`, `nb_individu`, and `pm_<type_transport>`.
#' @param data_cat_terr `data.frame` Territorial categorization dataset (e.g. INSEE
#' density classification) containing mappings between communes (`COM`) and higher levels
#' (`EPCI`, `DEP`, `REG`), as well as a variable `CAT_COM` defining urban/rural typology.
#' @param tab_typology `character` Territorial level of analysis. Must be one of:
#' `"france"`, `"region"`, `"departement"`, `"epci"`, or `"commune"`.
#' @param selected_cog `character` INSEE code of the selected territory.
#' Ignored when `tab_typology == "france"`.
#' @param territory_name `character` Human-readable name of the selected territory.
#'
#' @return A `plotly` interactive horizontal stacked bar chart showing the gender distribution by transport mode.
#' Each transport mode is displayed as a separate facet.
#'
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @noRd
plot_distrib_gender_pm <- function(distrib_gender_pm, data_cat_terr,
                                   tab_typology, selected_cog, territory_name){

  # --- Compute the proportion of men and women among users of each transport mode ---

  distrib_gender_pm <- distrib_gender_pm |>
    # Compute the number of users by gender and transport mode
    mutate(
      across(starts_with("pm_"), ~ .x * nb_individu, .names = "n_{str_remove(.col, 'pm_')}")
    ) |>
    # Compute the total of users (men + women) for each territory and mode
    group_by(insee_niveau, insee_code) |>
    mutate(
      across(starts_with("n_"), ~ sum(.x), .names = "total_{str_remove(.col, 'n_')}")
    ) |>
    # Proportion of men and women among users of each mode
    mutate(
      prop_velo = n_velo / total_velo,
      prop_car = n_car / total_car,
      prop_tcommun = n_tcommun / total_tcommun,
      prop_pied = n_pied / total_pied
    ) |>
    ungroup() |>
    pivot_longer(
      cols = matches("^(pm|prop|total|n)_"),
      names_to = c(".value", "mode_transport"),
      names_pattern = "(pm|prop|total|n)_(.*)"
    ) |>
    mutate(
      mode_transport = sub("prop_", "", mode_transport),
      prop = ifelse(is.nan(prop) | is.infinite(prop), NA, prop)
    )


  # --- Gender proportion for the selected territory ---

  ## Get data for the selected territory
  col_insee <- switch(tab_typology,
                      region = "REG",
                      departement = "DEP",
                      epci = "EPCI",
                      commune = "COM")


  if (tab_typology != "france"){
    distrib_gender_territory <- distrib_gender_pm |>
      filter(insee_niveau == col_insee & insee_code == selected_cog)
  } else {
    distrib_gender_territory <- distrib_gender_pm |>
      filter(insee_niveau == "NAT")
  }

  ## If territory has no data, print message
  if (nrow(distrib_gender_territory) == 0){
    return(plot_message_no_data("Pas de données de part modale existantes pour ce territoire", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.45, y_coord = 0.85, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  }

  ## Shorten EPCI names
  territory_name <- str_replace_all(territory_name,
                                    c("Communauté d'agglomération" = "CA",
                                      "Communauté urbaine" = "CU",
                                      "Communauté de communes" = "CC"))
  territory_name <- insert_linebreak_middle(territory_name, 23)

  ## Quick formatting
  distrib_gender_territory <- distrib_gender_territory |>
    mutate(category = paste0("<b>", territory_name, "</b>")) |>
    select(category, sexe, mode_transport, prop) |>
    mutate(prop = ifelse(is.na(prop), 0, prop))

  ## Tooltip
  data_plot <- distrib_gender_territory |>
    mutate(tooltip = paste0("<span style='font-weight:bold'>", format(round(prop*100, 0), trim = TRUE),
                            "%</span> des usagers ",
                            case_when(
                              mode_transport == "velo" ~ "du <span style='font-weight:bold'>vélo",
                              mode_transport == "car" ~ "de la <span style='font-weight:bold'>voiture",
                              mode_transport == "tcommun" ~ "des <span style='font-weight:bold'>transports en commun",
                              mode_transport == "pied" ~ "de la <span style='font-weight:bold'>marche",
                              TRUE ~ ""
                            ),
                            "</span> du territoire sont des <span style='font-weight:bold'>", ifelse(sexe == 1, "hommes", "femmes"), "</span>")
    )


  # --- Gender proportion by milieu ---

  if (tab_typology %in% c("departement", "region", "france")){

    ## Join modal share data by commune with territorial infos (EPCI, dep, reg and category)
    data_cat_terr <- data_cat_terr |>
      mutate(CAT = case_when(CAT_COM == 1 ~ 1,
                             CAT_COM %in% c(2, 3, 4) ~ 2,
                             CAT_COM %in% c(5, 6, 7) ~ 3),
             LIBCAT = case_when(CAT_COM == 1 ~ "Grands centres urbains",
                                CAT_COM %in% c(2, 3, 4) ~ "Communes intermédiaires",
                                CAT_COM %in% c(5, 6, 7) ~ "Communes rurales"))

    distrib_gender_com <- distrib_gender_pm |>
      filter(insee_niveau == "COM") |>
      select(-insee_niveau) |>
      select(insee_code, sexe, nb_individu, mode_transport, n) |>
      rename(COM = insee_code) |>
      left_join(data_cat_terr |> select(COM, EPCI, DEP, REG, LIBCAT), by = "COM") |>
      rename(category = LIBCAT)

    ## Filter selected_territory if needed
    if (tab_typology != "france"){
      distrib_gender_territory <- distrib_gender_com |>
        filter(!!sym(col_insee) == selected_cog)
    } else {
      distrib_gender_territory <- distrib_gender_com
    }

    ## Compute gender proportion by milieu and transport mode
    distrib_gender_milieu <- distrib_gender_territory |>
      group_by(category, sexe, mode_transport) |>
      summarise(
        n_users_total = sum(n, na.rm = TRUE),
        .groups = "drop_last"
      ) |>
      group_by(category, mode_transport) |>
      mutate(
        prop = n_users_total / sum(n_users_total, na.rm = TRUE)
      ) |>
      select(-n_users_total) |>
      ungroup()

    ## Tooltip
    distrib_gender_milieu <- distrib_gender_milieu |>
      mutate(
        category = factor(category, levels = c("Communes rurales", "Communes intermédiaires",  "Grands centres urbains")),
        tooltip = paste0("<span style='font-weight:bold'>", format(round(prop*100, 0), trim = TRUE),
                         "%</span> des usagers ",
                         case_when(
                           mode_transport == "velo" ~ "du <span style='font-weight:bold'>vélo",
                           mode_transport == "car" ~ "de la <span style='font-weight:bold'>voiture",
                           mode_transport == "tcommun" ~ "des <span style='font-weight:bold'>transports en commun",
                           mode_transport == "pied" ~ "de la <span style='font-weight:bold'>marche",
                           TRUE ~ ""
                         ),
                         "</span> dans les \n", str_to_lower(category),
                         " du territoire sont des <span style='font-weight:bold'>", ifelse(sexe == 1, "hommes", "femmes"), "</span>")
      ) |>
      arrange(category)

    ## Merge
    data_plot <- rbind(data_plot, distrib_gender_milieu)
  }

  data_plot <- data_plot |>
    mutate(category = factor(category, levels = c(paste0("<b>", territory_name, "</b>"), "Communes rurales", "Communes intermédiaires",  "Grands centres urbains")))


  # --- Plot ---

  # Custom colors
  mode_colors <- c(
    "velo"    = "#C6CE41",
    "car"     = "#8e8f91",
    "tcommun" = "#294754",
    "pied"    = "#B1D6E4"
  )

  data_plot <- data_plot |>
    group_by(category, mode_transport) |>
    mutate(
      color = ifelse(sexe == 2, mode_colors[mode_transport], "#e0e0e0")
    ) |>
    ungroup()

  # Clean transport mode labels
  data_plot <- data_plot |>
    mutate(
      sexe = factor(sexe, levels = c(1, 2)),
      mode_label = factor(mode_transport,
                          levels = c("velo", "pied", "tcommun", "car"),
                          labels = c("Vélo", "Marche",  "Transports en commun","Voiture"))
    )

  # ggplot chart
  p <- ggplot(data_plot, aes(
    x = category,
    y = prop,
    fill = color,
    text = tooltip
  )) +
    geom_bar(stat = "identity", position = "fill", width = 0.3) +
    facet_wrap(~mode_label, nrow = 1, scales = "fixed") +
    scale_fill_identity() +
    coord_flip() +
    theme_minimal(base_family = "Poppins") +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 10, color = "#294754"),
      strip.text = element_text(size = 13, face = "bold", color = "#294754")
    )

  # Plotly chart
  g <- ggplotly(p, tooltip = "text") |>
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

