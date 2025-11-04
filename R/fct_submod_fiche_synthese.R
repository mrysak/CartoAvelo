#' @title Generate the dataset for a radar chart on cycling-related indicators
#'
#' @description This function compiles and standardizes various cycling-related indicators
#' (e.g., cycling infrastructure, parking facilities, modal share, network completion)
#' for a selected territory and its corresponding category.
#' It gathers comparison data, computes local values, and outputs a
#' harmonized dataset ready for visualization in a radar (spider) chart.
#'
#' @param data_cat_terr A data frame mapping each commune (identified by its INSEE code)
#'   to its higher territorial units (EPCI, department, region) and its INSEE
#'   density category (\code{CAT_COM}).
#' @param amenagement_data A `data.frame` with cycling infrastructure indicators
#'   (e.g., total length of cycle paths, cycling rate) at the specified territorial level.
#' @param stationnement_data A `data.frame` containing parking indicators (e.g.,
#'   parking spots per 1,000 inhabitants or per km of cycle network) at the specified territorial level.
#' @param part_modale A `data.frame` containing modal share data of cycling by year
#'   and territorial unit.
#' @param veloroute_data A `data.frame` containing data on the national cycle route
#'   network (SNV) and EuroVelo routes, including realized and unrealized segments, at the specified territorial level.
#' @param accueil_velo_data A `data.frame` containing data on "Accueil Vélo" certified
#'   establishments and their relationship to the SNV length, at the specified territorial level.
#' @param current_year_ac Numeric. The current year for the cycling infrastructure dataset.
#' @param current_year_st Numeric. The current year for the parking dataset.
#' @param current_year_pm Numeric. The current year for the modal share dataset.
#' @param current_year_vr Numeric. The current year for the SNV dataset.
#' @param current_year_av Numeric. The current year for the "Accueil Vélo" dataset.
#' @param typology Character. The territorial typology to use, one of
#'   `"region"`, `"departement"`, `"epci"`, or `"commune"`.
#' @param selected_cog Character. The INSEE code identifying the selected territory.
#'
#' @return
#' A `list` with two elements:
#' \describe{
#'   \item{\code{data_to_display}}{A `data.frame` containing columns:
#'     \itemize{
#'       \item \code{year}: The reference year for the indicator.
#'       \item \code{LIBCAT}: Category label (e.g., "Moy", "Min", "Max", "Value").
#'       \item \code{indicator}: Numerical value of the indicator.
#'       \item \code{libelle}: Human-readable indicator name.
#'     }
#'   }
#'   \item{\code{lib_category}}{A character string giving the name of the selected
#'   comparison category (e.g., "Régions de métropole").}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import stringr
#' @noRd
generate_data_radar_chart <- function(data_cat_terr, amenagement_data, stationnement_data,
                                      part_modale, veloroute_data, accueil_velo_data,
                                      current_year_ac, current_year_st, current_year_pm,
                                      current_year_vr, current_year_av, typology, selected_cog){

  # --- Taux de cyclabilité ---

  # Elements of comparison
  data_comparison <- generate_comparison_data_local(data_cat_terr, amenagement_data,
                                                    typology, "taux_cyclabilite",
                                                    selected_cog, current_year_ac)

  data_comparison_cat <- data_comparison$data_comparison_cat
  lib_category <- data_comparison$lib_category

  # Value of the selected territory
  filtered_amenagement_data <- switch(typology,
                                      region = amenagement_data |> filter(INSEE_REG == selected_cog),
                                      departement = amenagement_data |> filter(INSEE_DEP == selected_cog),
                                      epci = amenagement_data |> filter(INSEE_EPCI == selected_cog),
                                      commune = amenagement_data |> filter(INSEE_COM == selected_cog))

  taux_cyclabilite <- filtered_amenagement_data |>
    select(all_of(paste0("TAUX_CYCL_", current_year_ac))) |> pull()

  data_to_display <- data_comparison_cat |>
    rbind(
      data.frame(
        year = as.character(current_year_ac),
        LIBCAT = "Value",
        indicator = taux_cyclabilite * 100
      )) |>
    mutate(libelle = "Taux de cyclabilité")


  # --- Voirie cyclable par habitant ---

  # Elements of comparison
  data_comparison <- generate_comparison_data_local(data_cat_terr, amenagement_data,
                                                    typology, "voirie_hab",
                                                    selected_cog, current_year_ac)

  data_comparison_cat <- data_comparison$data_comparison_cat |>
    mutate(libelle = "Voirie cyclable par habitant")

  # Value of the selected territory
  lineaire_total <- filtered_amenagement_data |>
    select(all_of(paste0("NB_TOTAL_", current_year_ac))) |> pull()
  pop <- filtered_amenagement_data |>
    select(POP) |> pull()

  voirie_hab <- 1000 * lineaire_total / pop

  data_to_display <- data_to_display |>
    rbind(data_comparison_cat) |>
    rbind(
      data.frame(
        year = as.character(current_year_ac),
        LIBCAT = "Value",
        indicator = voirie_hab,
        libelle = "Voirie cyclable par habitant"
      ))


  # --- Stationnements pour 1 000 habitants ---

  # Elements of comparison
  data_comparison_cat <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_data,
                                                                      "stat_hab", typology, selected_cog,
                                                                      current_year_st, current_year_st)$data_comparison_cat

  # Value of the selected territory
  filtered_stationnement_data <- switch(typology,
                                        region = stationnement_data |> filter(INSEE_REG == selected_cog),
                                        departement = stationnement_data |> filter(INSEE_DEP == selected_cog),
                                        epci = stationnement_data |> filter(INSEE_EPCI == selected_cog),
                                        commune = stationnement_data |> filter(INSEE_COM == selected_cog))

  stat_hab <- filtered_stationnement_data |>
    select(all_of(paste0("NB_STAT_1000HAB_", current_year_st))) |> pull()

  data_to_display <- data_to_display |>
    rbind(data_comparison_cat |> mutate(libelle = "Stationnements pour 1 000 habitants")) |>
    rbind(
      data.frame(
        year = as.character(current_year_st),
        LIBCAT = "Value",
        indicator = stat_hab,
        libelle = "Stationnements pour 1 000 habitants"
      ))


  # --- Stationnements par km d'aménagement cyclable ---

  # Elements of comparison
  data_comparison_cat <- generate_stationnement_comparison_data_local(data_cat_terr, stationnement_data,
                                                                      "stat_km", typology, selected_cog,
                                                                      current_year_st, current_year_st)$data_comparison_cat

  # Value of the selected territory
  stat_km <- filtered_stationnement_data |>
    select(all_of(paste0("NB_STAT_1KM-AC_", current_year_st))) |> pull()

  data_to_display <- data_to_display |>
    rbind(data_comparison_cat |> mutate(libelle = "Stationnements par km d'aménagement cyclable")) |>
    rbind(
      data.frame(
        year = as.character(current_year_st),
        LIBCAT = "Value",
        indicator = stat_km,
        libelle = "Stationnements par km d'aménagement cyclable"
      ))


  # --- Part modale du vélo ---

  # Elements of comparison
  data_comparison_cat <- generate_pm_comparison_data_local(data_cat_terr, part_modale,
                                                           current_year_pm, current_year_pm, "velo",
                                                           typology, selected_cog)$data_comparison_cat
  data_comparison_cat <- data_comparison_cat |>
    rename(year = annee, indicator = pm) |>
    filter(year == current_year_pm)

  # Value of the selected territory
  filtered_pm_data <- switch(typology,
                             region = part_modale |> filter(insee_niveau == "REG" & insee_code == selected_cog & annee == current_year_pm),
                             departement = part_modale |> filter(insee_niveau == "DEP" & insee_code == selected_cog & annee == current_year_pm),
                             epci = part_modale |> filter(insee_niveau == "EPCI" & insee_code == selected_cog & annee == current_year_pm),
                             commune = part_modale |> filter(insee_niveau == "COM" & insee_code == selected_cog & annee == current_year_pm))

  pm <- filtered_pm_data |> pull(pm_velo) * 100

  data_to_display <- data_to_display |>
    rbind(data_comparison_cat |> mutate(libelle = "Part modale du vélo")) |>
    rbind(
      data.frame(
        year = as.character(current_year_pm),
        LIBCAT = "Value",
        indicator = pm,
        libelle = "Part modale du vélo"
      ))


  if (typology %in% c("region", "departement")){

    # --- Accueil Vélo par km de véloroute du SNV ---

    # Retrieve relevant data and filter to only keep territories belonging to the same category
    colname <- switch(typology,
                      departement = c("DEP", "LIB_CAT_DEP", "INSEE_DEP"),
                      region = c("REG", "LIB_CAT_REG", "INSEE_REG"))

    if (typology == "region"){
      data_cat_terr <- data_cat_terr |>
        mutate(LIB_CAT_REG = ifelse(REG %in% c("01", "02", "03", "04", "06"), "régions d'outre-mer", "régions de métropole"))

      category_to_filter <- lib_category

      accueil_velo_data <- accueil_velo_data |>
        mutate(INSEE_REG = as.character(INSEE_REG))
    } else if (typology == "departement"){
      category_to_filter <- gsub("s\\b", "", lib_category, ignore.case = TRUE)
      category_to_filter  <- str_to_sentence(category_to_filter)
    }

    data_comparison_av <- data_cat_terr |>
      filter(.data[[colname[2]]] == category_to_filter) |>
      select(all_of(colname[1])) |>
      unique() |>
      left_join(accueil_velo_data |> select(all_of(c(colname[3], paste0(c("NB_TOTAL_", "NB_TOTAL_KMSNV_"), current_year_av)))),
                by = setNames(colname[3], colname[1])) |>
      rename(nb_presta = paste0("NB_TOTAL_", current_year_av),
             presta_km_snv = paste0("NB_TOTAL_KMSNV_", current_year_av))

    # Get minimum and maximum
    min_ratio <- min(data_comparison_av$presta_km_snv, na.rm = TRUE)
    max_ratio <- max(data_comparison_av$presta_km_snv, na.rm = TRUE)

    # Get value for the selected territory
    territory_ratio <- data_comparison_av |> filter(!!sym(colname[1]) == selected_cog) |> pull(presta_km_snv)

    # Compute average value in the category
    avg_ratio <- data_comparison_av |>
      mutate(km_snv = nb_presta / presta_km_snv) |>
      summarise(
        prestations_par_km_national = sum(nb_presta, na.rm = TRUE) / sum(km_snv, na.rm = TRUE)
      ) |>
      pull()

    # Merge with existing data
    if (!is.na(territory_ratio)){
      data_to_display <- data_to_display |>
        rbind(
          data.frame(
            year = as.character(current_year_av),
            LIBCAT = c("Moy", "Min", "Max", "Value"),
            indicator = c(avg_ratio, min_ratio, max_ratio, territory_ratio),
            libelle = "Prestations Accueil Vélo par km de SNV"
          ))
    }


    # --- Taux de réalisation SNV ---

    # Retrieve relevant data and filter to only keep territories belonging to the same category
    data_comparison_vr <- data_cat_terr |>
      filter(.data[[colname[2]]] == category_to_filter) |>
      select(all_of(colname[1])) |>
      unique() |>
      left_join(veloroute_data,
                by = setNames(colname[3], colname[1]))

    # Compute data for SNV+EV (completed and non-completed kilometers)
    data_comparison_vr <- data_comparison_vr |>
      mutate(SNV_EV_REAL = !!sym(paste0("EV_SITE-PARTAGE_", current_year_vr)) + !!sym(paste0("EV_SITE-PROPRE_", current_year_vr)) +
               !!sym(paste0("SNV-STRICT_SITE-PARTAGE_", current_year_vr)) + !!sym(paste0("SNV-STRICT_SITE-PROPRE_", current_year_vr)),
             SNV_EV_NON_REAL = !!sym(paste0("EV_NON-REAL_", current_year_vr)) + !!sym(paste0("SNV-STRICT_NON-REAL_", current_year_vr))) |>
      select(all_of(colname[1]), SNV_EV_REAL, SNV_EV_NON_REAL) |>
      mutate(completion_rate = 100 * SNV_EV_REAL / (SNV_EV_REAL + SNV_EV_NON_REAL))

    # Get minimum and maximum
    max_rate <- max(data_comparison_vr$completion_rate, na.rm = TRUE)
    min_rate <- min(data_comparison_vr$completion_rate, na.rm = TRUE)

    # Get value for the selected territory
    territory_rate <- data_comparison_vr |> filter(!!sym(colname[1]) == selected_cog) |> pull(completion_rate)

    # Compute average value in the category
    total_real <- sum(data_comparison_vr$SNV_EV_REAL, na.rm = TRUE)
    total_non_real <- sum(data_comparison_vr$SNV_EV_NON_REAL, na.rm = TRUE)

    avg_rate <- 100 * total_real / (total_real + total_non_real)

    # Merge with existing data
    if (!is.na(territory_rate)){
      data_to_display <- data_to_display |>
        rbind(
          data.frame(
            year = as.character(current_year_vr),
            LIBCAT = c("Moy", "Min", "Max", "Value"),
            indicator = c(avg_rate, min_rate, max_rate, territory_rate),
            libelle = "Taux de réalisation du SNV"
          ))
    }
  }


  # Define a threshold for the maximum to avoid unreadable display
  df_thresholded <- data_to_display |>
    pivot_wider(
      id_cols = c(year, libelle),
      names_from = LIBCAT,
      values_from = indicator
    ) |>
    mutate(
      Max_thresholded = if_else(Max > 10 * Moy, 10 * Moy, Max)
    ) |>
    pivot_longer(
      cols = c(Max_thresholded),
      names_to = "LIBCAT",
      values_to = "indicator"
    ) |>
    select(year, LIBCAT, indicator, libelle)


  data_to_display <- bind_rows(data_to_display, df_thresholded)

  return(list(data_to_display = data_to_display, lib_category = lib_category))
}



#' @title Plot a radar chart for cycling indicators
#'
#' @description This function visualizes multiple cycling-related indicators (e.g., infrastructure,
#' modal share, parking, network completion) as a radar (spider) chart. It compares
#' the selected territory's performance against the average and maximum values of
#' its category (e.g., region, department, etc.).
#'
#' @param data_to_display A `data.frame` generated by
#'   \code{\link{generate_data_radar_chart}}, containing indicator values for
#'   the selected territory and its comparison group. Must include columns:
#'   \itemize{
#'     \item \code{year}: Reference year for each indicator.
#'     \item \code{LIBCAT}: Category label (e.g., "Moy", "Min", "Max", "Value").
#'     \item \code{indicator}: Numeric value of the indicator.
#'     \item \code{libelle}: Indicator name (e.g., "Taux de cyclabilité").
#'   }
#' @param lib_category Character. The label of the comparison group
#'   (e.g., "régions de métropole", "départements ruraux").
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts the input data into a wide format for plotting.
#'   \item Normalizes indicator values between 0 and 100 based on observed min and max.
#'   \item Adds readable units for each indicator (e.g., %, km/hab, places/km).
#'   \item Builds a radar chart using the \pkg{plotly} package with two layers:
#'     \itemize{
#'       \item The average of the comparison category.
#'       \item The actual value of the selected territory.
#'     }
#'   \item Adds dynamic tooltips showing exact indicator values, units, and relative position.
#' }
#'
#' @return A \code{plotly} radar chart object displaying:
#' \itemize{
#'   \item The selected territory's normalized performance.
#'   \item The average normalized performance of the comparison category.
#'   \item Tooltip text with contextual details.
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import plotly
plot_radar_chart <- function(data_to_display, lib_category){

  ## Formatting and normalization
  df_wide <- data_to_display |>
    mutate(indicator = if_else(LIBCAT == "Min", 0, indicator)) |>
    pivot_wider(names_from = LIBCAT, values_from = indicator) |>
    mutate(across(c(Min, Max_thresholded, Moy, Value), as.numeric)) |>
    rowwise() |>
    mutate(
      range = Max_thresholded - Min,
      value_pct = ifelse(range == 0, 50, (Value - Min) / range * 100),
      moy_pct   = ifelse(range == 0, 50, (Moy   - Min) / range * 100)
    ) |>
    ungroup()

  df_closed <- df_wide |>
    add_row(df_wide[1, ]) |>
    mutate(unit = case_when(
      libelle %in% c("Part modale du vélo", "Taux de cyclabilité", "Taux de réalisation du SNV") ~ "%",
      libelle == "Prestations Accueil Vélo par km de SNV" ~ " prestas/km",
      libelle == "Stationnements par km d'aménagement cyclable" ~ " places/km",
      libelle == "Stationnements pour 1 000 habitants" ~ " places/1 000 hab",
      libelle == "Voirie cyclable par habitant" ~ " km/hab"
    ))


  ## Custom tooltip
  tooltip <- paste0(
    "<b>", df_closed$libelle, "</b> en ", df_closed$year, "<br><br>",
    "Valeur du territoire : <b>", format(round(df_closed$Value, 2), big.mark = " ", decimal.mark = ",", trim = TRUE), df_closed$unit, "</b><br>",
    "Moyenne des ", lib_category, " : ", format(round(df_closed$Moy, 2), big.mark = " ", decimal.mark = ",", trim = TRUE), df_closed$unit, "<br>",
    "Maximum des ", lib_category, " : ", format(round(df_closed$Max, 2), big.mark = " ", decimal.mark = ",", trim = TRUE), df_closed$unit, "<br><br>",
    "<i>Position du territoire : ", round(df_closed$value_pct, 0), "% du maximum observé</i>"
  )


  ## Radar plot
  fig <- plot_ly(type = "scatterpolar") |>
    # Mean
    add_trace(
      r = df_closed$moy_pct,
      theta = df_closed$libelle,
      mode = "lines+markers",
      fill = "toself",
      name = paste0("Moyenne des ", lib_category),
      line = list(color = "#E94F35", width = 2),
      fillcolor = "rgba(233, 79, 53, 0.25)",
      marker = list(color = "#E94F35", size = 6),
      hovertemplate = "%{customdata}<extra></extra>",
      customdata = tooltip
    ) |>
    # Territory value
    add_trace(
      r = df_closed$value_pct,
      theta = df_closed$libelle,
      mode = "lines+markers",
      fill = "toself",
      name = "Valeur du territoire",
      line = list(color = "#C6CE41", width = 2),
      fillcolor = "rgba(198, 206, 65, 0.4)",
      marker = list(color = "#C6CE41", size = 6),
      hovertemplate = "%{customdata}<extra></extra>",
      customdata = tooltip
    )  |>
    # Layout
    layout(
      font = list(family = "Poppins", size = 15, color = "#294754"),
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 100),
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0", "", "", "", paste0("Maximum des\n", lib_category)),
          showline = FALSE,
          tickfont = list(size = 11)
        )
      ),
      legend = list(orientation = "h", x = 0, y = -0.2,
                    font = list(family = "Poppins", size = 13, color = "#294754")),
      margin = list(l = 50, r = 50, t = 50, b = 80)
    )

  fig

}
