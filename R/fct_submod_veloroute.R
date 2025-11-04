#' @title Plot the Progress of Cycle Route Development
#'
#' @description This function generates a **treemap visualization** showing the advancement
#' of cycle route development (in km) for a given schema type (e.g., EuroVelo,
#' strictly national, strictly regional, or strictly departmental).
#' The treemap displays the proportion of kilometers realized in
#' \emph{dedicated lanes (site propre)}, \emph{shared lanes (site partagé)},
#' and those \emph{still unrealized (non réalisé)}.
#'
#' A custom annotation is added below the chart, summarizing the total
#' kilometers and the realized portion. Tooltips provide contextual information
#' about percentages, kilometers, and differences compared to the previous year.
#'
#' @param veloroute_data A data frame containing cycle route lengths (in km),
#'   structured with columns representing kilometers realized in site propre,
#'   site partagé, and non réalisé for different schema levels and years.
#' @param current_year An integer specifying the reference year of analysis.
#' @param schema_type A string specifying the schema level. Options include:
#'   \code{"EV"} (EuroVelo), \code{"SNV-STRICT"} (strictly national),
#'   \code{"SRV-STRICT"} (strictly regional), or \code{"SDV-STRICT"}
#'   (strictly departmental).
#' @param tab_typology A string specifying the display typology of the tab.
#'   Must be one of: \code{"france"}, \code{"region"}, or \code{"departement"}.
#' @param selected_cog A string giving the INSEE code of the selected
#'   territory (region or department, depending on \code{tab_typology}).
#' @param territory_name A string giving the display name of the territory.
#' @param tab_id A string identifying the thematic tab on which the visualization
#'   is displayed. Either \code{"vr"} (véloroutes) or \code{"fs"} (fiche synthèse).
#'   Defaults to \code{"vr"}.
#'
#' @return A \code{plotly} treemap object showing the distribution of realized
#'   and unrealized kilometers, with interactive tooltips and an annotation
#'   summarizing total kilometers and realized share.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_to_lower
#' @importFrom plotly plot_ly layout
#' @noRd
plot_avancement_veloroutes<- function(veloroute_data, current_year, schema_type,
                                      tab_typology, selected_cog, territory_name,
                                      tab_id = "vr"){

  ## Filter territory if needed
  if (tab_typology == "region"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_REG == selected_cog)
  } else if (tab_typology == "departement"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_DEP == selected_cog)
  }


  ## Prepare dataset
  previous_year <- current_year - 1

  cols_to_select <- c(paste0(schema_type, "_SITE-PARTAGE_", current_year),
                      paste0(schema_type, "_SITE-PARTAGE_", previous_year),
                      paste0(schema_type, "_SITE-PROPRE_", current_year),
                      paste0(schema_type, "_SITE-PROPRE_", previous_year),
                      paste0(schema_type, "_NON-REAL_", current_year))

  df <- veloroute_data |>
    select(all_of(cols_to_select)) |>
    pivot_longer(
      cols = everything(),
      names_to = c("type", "year"),
      names_pattern = paste0(schema_type, "_([A-Z\\-]+)_(\\d+)"),
      values_to = "km"
    ) |>
    mutate(
      type = case_when(
        type == "SITE-PROPRE"   ~ "site propre",
        type == "SITE-PARTAGE"  ~ "site partagé",
        type == "NON-REAL"      ~ "non réalisé",
        TRUE ~ str_to_lower(type)
      ),
      year = as.integer(year)
    )

  # Labels for display
  df <- df |>
    mutate(label = paste0(format(round(km, 0), big.mark = " "), " km"))

  # Tooltip
  schema_text <- switch(schema_type,
                        "EV" = "EuroVelo",
                        "SNV-STRICT" = "nationales",
                        "SRV-STRICT" = "régionales",
                        "SDV-STRICT" = "départementales")

  df <- df |>
    arrange(type, year) |>
    group_by(type) |>
    mutate(diff_km = km - dplyr::lag(km)) |>
    ungroup() |>
    group_by(year) |>
    mutate(
      pct = km / sum(km) * 100,
      diff_km = case_when(
        type == "non réalisé" ~ -sum(diff_km[type %in% c("site propre", "site partagé")], na.rm = TRUE),
        TRUE ~ diff_km
      )
    ) |>
    ungroup() |>
    mutate(tooltip = paste0("En ", territory_name, " en ", current_year, ", <b>",
                            round(pct, 0), "%</b> des véloroutes strictement ",
                            schema_text,
                            case_when(
                              type == "site propre"  ~ "\nsont réalisées en site propre",
                              type == "site partagé" ~ "\nsont réalisées en site partagé",
                              type == "non réalisé"  ~ "\nrestent à réaliser",
                              TRUE ~ ""
                            ),
                            ", soit <b>", format(round(km, 0), big.mark = " "), " km</b>.",
                            "\nCe sont ", abs(round(diff_km,0)), ifelse(type == "non réalisé", " km de moins ", " km de plus "),
                            "qu'en ", previous_year, "."
    ))

  # Custom color palette
  color_palette <- c("#294754", "#B1D6E4", "#D3D3D3")
  names(color_palette) <- c("site propre", "site partagé", "non réalisé")

  df <- df |>
    mutate(color = color_palette[type])


  ## Treemap
  g <- plot_ly(
    data = df |> filter(year == current_year),
    type = "treemap",
    labels = ~label,
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
    )


  ## Add annotation below
  totals <- df |>
    filter(year == current_year) |>
    summarise(
      total_km = sum(km, na.rm = TRUE),
      realized_km = sum(km[type %in% c("site propre", "site partagé")], na.rm = TRUE),
      real_rate = 100 * realized_km / total_km
    )

  if (tab_id == "vr"){
    total_text <- paste0(
      "<b>", format(round(totals$total_km, 0), big.mark = " "), " km</b> au total dont ",
      "<b>", format(round(totals$realized_km, 0), big.mark = " "), " km</b> réalisés"
    )
  } else if (tab_id == "fs"){
    total_text <- paste0(
      "Taux de réalisation : <b>",
      ifelse(is.na(totals$real_rate), "-", round(totals$real_rate, 0)),
      "<b>%")
  }

  g <- g |>
    layout(
      font = list(family = "Poppins"),
      margin = list(b = 60, t = 0),
      annotations = list(
        list(
          text = total_text,
          x = 0, y = -0.04,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 15, color = "#294754")
        )
      )
    ) |>
    config(
      displaylogo = FALSE,
      toImageButtonOptions = list(format = "png",  filename = "veloroutes_etat_realisation")
    )

  return(g)
}



#' @title Plot the historical development of French cycle routes by schema level
#'
#' @description This function generates an interactive bar chart showing the annual kilometers
#' of French cycle routes (véloroutes) created over time, categorized by schema level:
#' EuroVelo, strictly national (SNV), strictly regional (SRV), and strictly departmental (SDV).
#' The function dynamically extracts the relevant columns from the input dataset based
#' on the selected time period, reshapes the data, and formats tooltips and labels
#' for use in a \pkg{plotly} visualization.
#'
#' @param veloroute_data A data frame containing cumulative cycle route lengths (in km)
#'   by schema level and year. Column names must follow the pattern
#'   \code{"<SCHEMA_TYPE>_REAL_<YEAR>"}, where \code{<SCHEMA_TYPE>} is one of
#'   \code{"EV"}, \code{"SNV-STRICT"}, \code{"SRV-STRICT"}, or \code{"SDV-STRICT"}.
#' @param current_year An integer specifying the reference year for the analysis.
#'   Determines the end of the time range plotted.
#' @param start_year An integer specifying the first year included in the visualization.
#'   Determines the start of the time range plotted.
#' @param tab_typology A string indicating the level of territorial aggregation
#'   for the visualization. Must be one of:
#'   \itemize{
#'     \item \code{"france"} – displays national-level data;
#'     \item \code{"region"} – filters the data for the selected region;
#'     \item \code{"departement"} – filters the data for the selected department.
#'   }
#' @param selected_cog A string specifying the INSEE code of the selected
#'   administrative unit (region or department), used when \code{tab_typology}
#'   is set to \code{"region"} or \code{"departement"}.
#'
#' @return A \code{plotly} object representing an interactive stacked bar chart.
#'   Each bar corresponds to a year, and colors represent schema levels:
#'   \itemize{
#'     \item EuroVelo (EV)
#'     \item Strictly national (SNV)
#'     \item Strictly regional (SRV)
#'     \item Strictly departmental (SDV)
#'   }
#'   The chart includes tooltips showing the kilometers created per year and schema level.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import plotly
#' @importFrom scales label_number
#' @noRd
plot_veloroute_history <- function(veloroute_data, current_year, start_year,
                                   tab_typology, selected_cog){

  ## Filter territory if needed
  if (tab_typology == "region"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_REG == selected_cog)
  } else if (tab_typology == "departement"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_DEP == selected_cog)
  }

  # If territory has no data, print message
  if (nrow(veloroute_data) == 0){
    return(plot_message_no_data("", 4.5,
                                "", x_title = 0, y_title = 0, size_title = 0,
                                x_coord = 0.45, y_coord = 0.9, t = 12, l = 0, b = 20, r = 0) |>
             config(displayModeBar = FALSE))
  }


  ## Extract useful columns
  time_period <- seq(start_year, current_year)
  schema_types <- c("EV", "SNV-STRICT", "SRV-STRICT", "SDV-STRICT")

  cols_to_select <- as.vector(outer(schema_types, time_period,
                                    FUN = function(a, b) paste0(a, "_REAL_", b)))

  ## Data preparation and tooltip formatting
  df <- veloroute_data |>
    select(all_of(cols_to_select)) |>
    pivot_longer(
      cols = everything(),
      names_to = c("schema_type", "year"),
      names_pattern = "(.*)_REAL_(\\d+)",
      values_to = "km"
    ) |>
    mutate(
      year = factor(as.integer(year)),
      schema_type = factor(schema_type, levels = c("SDV-STRICT", "SRV-STRICT", "SNV-STRICT", "EV")),
      tooltip = paste0("Linéaire de véloroutes strictement ",
                       case_when(schema_type == "EV" ~ "EuroVelo",
                                 schema_type == "SNV-STRICT" ~ "nationales",
                                 schema_type == "SRV-STRICT" ~ "regionales",
                                 schema_type == "SDV-STRICT" ~ "departementales"),
                       " créé en ", year, " : <span style='font-weight:bold'>",
                       format(round(km, 1), big.mark = " ", decimal.mark = ",", trim = TRUE), " km</span>")
    ) |>
    mutate(km = ifelse(km < 0, 0, km))


  ## Height threshold for bar labels display
  df_threshold <- df |>
    group_by(year) |>
    mutate(sum_km = sum(km, na.rm = TRUE))

  seuil <- max(df_threshold$sum_km) * 0.04

  df <- df |>
    mutate(label = case_when(
      round(km, 0) == 0 ~ "",
      km < seuil ~ "  ",
      TRUE ~ paste0(format(round(km, 0), big.mark = " ", trim = TRUE), " km")
    ))


  ## Custom color palette
  color_palette <- c(
    "EuroVelo" = "#303876",
    "Strictement nationales" = "#E94F35",
    "Strictement régionales" = "#C6CE41",
    "Strictement départementales" = "#ECB2D2"
  )


  ## Legend labels
  legend_labels <- c("Strictement départementales", "Strictement régionales",
                     "Strictement nationales", "EuroVelo")

  if (tab_typology %in% c("region", "departement")){
    names(color_palette) <- paste0("Véloroutes ", str_to_lower(names(color_palette)))
    legend_labels <- paste0("Véloroutes ", str_to_lower(legend_labels))
  }

  df <- df |>
    mutate(
      schema_type = factor(
        schema_type,
        levels = c("SDV-STRICT", "SRV-STRICT", "SNV-STRICT", "EV"),
        labels = legend_labels
      )
    )


  ## ggplot chart
  p <- ggplot(df, aes(x = year, y = km, fill = schema_type, text = tooltip)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              color = "white", size = 3) +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(labels = label_number(decimal.mark = ",")) +
    labs(
      title = "",
      x = "",
      y = "Kilomètres réalisés",
      fill = ifelse(tab_typology == "france", "Véloroutes", "")
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


  ## Return plotly chart
  g <- ggplotly(p, tooltip = "text") |>
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.07,
        traceorder = "reversed",
        font = list(size = ifelse(tab_typology == "france", 15, 14), color = "#294754")
      )
    ) |>
    config(displayModeBar = FALSE)

  return(g)
}



#' @title Plot the Distribution of New French Cycle Route Creations
#'
#' @description This function generates two interactive \strong{Plotly pie charts} showing the
#' distribution of newly realized French cycle routes ("véloroutes") for a given year:
#' one by \emph{schema level} (EuroVelo, strictly national, regional, and departmental),
#' and one by \emph{status} (site propre vs. site partagé).
#' The plots visualize how much new infrastructure (in kilometers) was created within
#' each category, both in absolute terms (km) and as a percentage of all new realizations.
#'
#' @param veloroute_data A data frame containing cycle route lengths (in km),
#'   structured with columns representing kilometers realized in site propre,
#'   site partagé, and non réalisé for different schema levels and years.
#' @param current_year An integer specifying the reference year of analysis.
#' @param tab_typology A string specifying the display typology of the tab.
#'   Must be one of: \code{"france"}, \code{"region"}, or \code{"departement"}.
#' @param selected_cog A string giving the INSEE code of the selected
#'   territory (region or department, depending on \code{tab_typology}).
#' @param territory_name A string giving the display name of the territory.
#'
#' @return
#' A \code{list} containing two interactive \pkg{plotly} objects:
#' \describe{
#'   \item{\code{$plot_schema}}{Pie chart showing new cycle route creations by schema level.}
#'   \item{\code{$plot_statut}}{Pie chart showing new cycle route creations by development status.}
#' }
#'
#' @importFrom dplyr filter select mutate group_by ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_lower
#' @importFrom plotly plot_ly layout config
#' @importFrom stats setNames
#' @noRd
plot_new_creations <- function(veloroute_data, current_year,
                               tab_typology, selected_cog, territory_name){

  ## Filter territory if needed
  if (tab_typology == "region"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_REG == selected_cog)
  } else if (tab_typology == "departement"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_DEP == selected_cog)
  }

  # If territory has no data, print message
  if (nrow(veloroute_data) == 0){
    return(list(plot_schema = plot_message_no_data("Ce territoire ne dispose pas d'itinéraire connu\nde l'Observatoire national des véloroutes", 4.5,
                                                   "", x_title = 0, y_title = 0, size_title = 0,
                                                   x_coord = 0.45, y_coord = 0.65, t = 12, l = 0, b = 20, r = 0) |>
                  config(displayModeBar = FALSE),
                plot_statut = plot_message_no_data("", 4.5,
                                                   "", x_title = 0, y_title = 0, size_title = 0,
                                                   x_coord = 0.4, y_coord = 0.93, t = 12, l = 0, b = 20, r = 0) |>
                  config(displayModeBar = FALSE)))
  }


  # --- New creations by schema type ---

  ## Prepare dataset
  schema_types <- c("EV", "SNV-STRICT", "SRV-STRICT", "SDV-STRICT")

  cols_to_select <- paste0(schema_types, "_REAL_", current_year)

  df <- veloroute_data |>
    select(all_of(cols_to_select)) |>
    pivot_longer(
      cols = everything(),
      names_to = "schema_type",
      names_pattern = paste0("(.*)_REAL_", current_year),
      values_to = "km"
    ) |>
    mutate(pct = km / sum(km) * 100,
           tooltip = paste0("En ", territory_name, ", <span style='font-weight:bold'>",
                            format(round(km, 1), big.mark = " ", decimal.mark = ","),
                            " km </span> de véloroutes ont été \nnouvellement réalisées sur le <span style='font-weight:bold'>Schéma ",
                            case_when(schema_type == "EV" ~ "EuroVelo",
                                      schema_type == "SNV-STRICT" ~ "National",
                                      schema_type == "SRV-STRICT" ~ "Régional",
                                      schema_type == "SDV-STRICT" ~ "Départemental"),
                            "</span> strict\nen ", current_year, " (soit ", round(pct, 0),
                            "% de l'ensemble des nouvelles réalisations)")
    )


  ## Custom color palette
  color_palette <- c(
    "EuroVelo" = "#303876",
    "Strictement nationales" = "#E94F35",
    "Strictement régionales" = "#C6CE41",
    "Strictement départementales" = "#ECB2D2"
  )


  ## Legend labels
  df <- df |>
    mutate(
      schema_type = factor(
        schema_type,
        levels = c("SDV-STRICT", "SRV-STRICT", "SNV-STRICT", "EV"),
        labels = c("Strictement départementales", "Strictement régionales",
                   "Strictement nationales", "EuroVelo")
      )
    )


  ## Plotly pie chart
  p_schema <- plot_ly(
    data = df,
    labels = ~schema_type,
    values = ~km,
    type = "pie",
    name = "",
    text = ~paste0(format(round(km, 0), big.mark = " "), " km"),
    textinfo = "text",
    textposition = "outside",
    hovertemplate = ~paste0(tooltip, "<extra></extra>"),
    marker = list(colors = color_palette,
                  line = list(color = "#FFFFFF", width = 1)),
    sort = FALSE,
    direction = "anticlockwise",
    showlegend = TRUE,
    textfont = ~list(size = 14, family = "Poppins")
  ) |>
    layout(
      legend = list(
        orientation = "v",
        font = list(size = 14, color = "#294754")
      ),
      margin = list(t = 60, b = 20, l = 0, r = 0),
      showlegend = FALSE,
      font = list(family = "Poppins")
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png", filename = "veloroutes_realisations_schema")
    )



  # --- New creations by status ---

  ## Prepare dataset
  cols_to_select <- paste0(c("TOUS-SCHEMA_SITE-PROPRE_REAL_", "TOUS-SCHEMA_SITE-PARTAGE_REAL_"),
                           current_year)

  df <- veloroute_data |>
    select(all_of(cols_to_select)) |>
    pivot_longer(
      cols = everything(),
      names_to = "statut",
      names_pattern = paste0("TOUS-SCHEMA_(.*)_REAL_", current_year),
      values_to = "km"
    ) |>
    mutate(
      statut = case_when(statut == "SITE-PROPRE" ~ "Site propre",
                         statut == "SITE-PARTAGE" ~ "Site partagé"),
      pct = km / sum(km) * 100,
      tooltip = paste0("En ", territory_name, ", <span style='font-weight:bold'>",
                       format(round(km, 1), big.mark = " ", decimal.mark = ","),
                       " km </span> de véloroutes ont été\nnouvellement réalisés en <span style='font-weight:bold'> ",
                       str_to_lower(statut),
                       "</span> en ", current_year, "\n(soit ", round(pct, 0),
                       "% de l'ensemble des nouvelles réalisations)")
    )


  ## Custom color palette
  color_palette <- c(
    "Site propre" = "#294754",
    "Site partagé" = "#B1D6E4"
  )


  ## Plotly pie chart
  p_statut <- plot_ly(
    data = df,
    labels = ~statut,
    values = ~km,
    type = "pie",
    name = "",
    text = ~paste0(format(round(km, 0), big.mark = " "), " km"),
    textinfo = "text",
    textposition = "outside",
    hovertemplate = ~paste0(tooltip, "<extra></extra>"),
    marker = list(colors = color_palette,
                  line = list(color = "#FFFFFF", width = 1)),
    sort = FALSE,
    direction = "anticlockwise",
    showlegend = TRUE,
    textfont = ~list(size = 14, family = "Poppins")
  ) |>
    layout(
      hoverlabel = list(namelength = 0),
      legend = list(
        orientation = "v",
        font = list(size = 14, color = "#294754")
      ),
      margin = list(t = 60, b = 20, l = 0, r = 0),
      showlegend = FALSE,
      font = list(family = "Poppins")
    ) |>
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        "pan2d", "select2d", "lasso2d", "zoomIn2d", "zoomOut2d",
        "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
        "toggleSpikelines", "sendDataToCloud", "resetViews"
      ),
      toImageButtonOptions = list(format = "png", filename = "veloroutes_realisations_statut")
    )

  return(list(plot_schema = p_schema, plot_statut = p_statut))
}





#' @title Generate a tooltip label for an interactive cycling routes map
#'
#' @description Creates a formatted HTML tooltip label string to display on a leaflet
#' map, showing the indicator value and its evolution from the previous year.
#'
#' @param value Numeric. The current value of the selected indicator, or its evolution.
#' @param label Numeric. Additional information to display in the label.
#' @param info_to_display Character. Either `"value"` or `"evol"` to determine which value to display as the main value.
#' @param indicator_choice Character. One of `"taux_realisation"` or `"km_restant"`.
#' Determines which indicator is displayed and the corresponding units and label.
#' @param current_year Character. The latest year with data.
#' @param schema_type A string specifying the schema level. Options include:
#'   \code{"EV"} (EuroVelo), \code{"SNV-STRICT"} (strictly national),
#'   \code{"SRV-STRICT"} (strictly regional), \code{"SDV-STRICT"}
#'   (strictly departmental) or \code{"SNV-EV"} (national with EuroVelo).
#'
#' @return A character string with HTML formatting, ready to be used in a leaflet tooltip.
#' The string includes the main value, unit, and, if applicable, the evolution from the previous year.
#'
#' @noRd
tooltip_label_veloroute <- function(value, label, info_to_display, indicator_choice, current_year, schema_type){

  schema_type_label <- case_when(
    schema_type == "EV" ~ "schéma EuroVelo strict",
    schema_type == "SNV-STRICT" ~ "schéma national strict",
    schema_type == "SRV-STRICT" ~ "schéma régional strict",
    schema_type == "SDV-STRICT" ~ "schéma départemental strict",
    schema_type == "SNV-EV" & indicator_choice == "taux_realisation" ~ "schéma national</br>des véloroutes (EuroVelo inclus)",
    schema_type == "SNV-EV" & indicator_choice == "km_restant" ~ "schéma national (EuroVelo inclus)"
  )

  if(is.na(value)){
    return(paste0("Ce territoire ne comporte pas de véloroutes</br>appartenant au ", schema_type_label))
  }

  general_label <- switch(indicator_choice,
                          taux_realisation = paste0("Taux de réalisation du ", schema_type_label, " : <span style='font-weight:bold'>"),
                          km_restant = paste0("Nombre de kilomètres du ", schema_type_label, "</br>restant à réaliser : <span style='font-weight:bold'>"))

  unit <- switch(indicator_choice,
                 taux_realisation = c("%", " points"),
                 km_restant = c(" km", " km"))

  val <- ifelse(info_to_display == "value", value, label)

  evol <- ifelse(info_to_display == "value", label, value)

  if (indicator_choice == "taux_realisation"){
    prefix <- ifelse(!is.na(evol) && evol >= 0, "+", "")
    evolution <- paste0("Evolution /", as.numeric(current_year) - 1, " : <span style='font-weight:bold'>",
                        prefix, format_val(evol, unit[2]), "</span>")
  } else {
    evolution <- paste0("Nouvelles réalisations en ", current_year, " : <span style='font-weight:bold'>",
                        format_val(evol, unit[2]), "</span>")
  }



  legend <- paste0(general_label, format_val(val), unit[1], "</span></br>", evolution)

  return(legend)
}



#' @title Create a Leaflet map of véloroute indicators by administrative territory
#'
#' @description Build an interactive \pkg{leaflet} map showing véloroute indicators
#' (completion rate or kilometers remaining) for French administrative units. The function
#' prepares the requested indicator at the chosen territorial scale (region or
#' department), applies a color palette (via helper palette functions), attaches
#' HTML tooltips, and returns a ready-to-render \pkg{leaflet} map object.
#'
#' @param data_cat_terr A dataframe containing territorial categorisation information for French administrative units.
#' @param veloroute_data A data frame with véloroute metrics (km realized, km non-realized, by schema and site status).
#' @param admin_sf sf object. Spatial administrative boundaries (communes, EPCI, départements, régions).
#' @param tab_typology Character; the aggregation level used for the map rendering and filtering.
#'   Expected values: \code{"france"} (no territorial filter), \code{"region"} or \code{"departement"}.
#' @param display_typology Character. Administrative scale used for display. One of `"departement"` or `"region"`.
#' @param selected_territory Character. INSEE code of the territory selected by the user.
#' @param indicator_choice Character; which indicator to compute and display. Supported values:
#'   \code{"taux_realisation"} (completion rate) or \code{"km_restant"} (kilometers remaining).
#' @param info_to_display Character. Whether to display the value itself ("value") or its evolution from the previous year ("evol").
#' @param schema_type Character; one of \code{"EV"}, \code{"SNV-EV"}, \code{"SNV-STRICT"},
#'   \code{"SRV-STRICT"} or \code{"SDV-STRICT"}. When \code{"SNV-EV"} is provided, the function synthesizes
#'   \code{SNV-EV} columns by summing \code{EV_...} and \code{SNV-STRICT_...} per row.
#' @param current_year Numeric. The latest year with data.
#'
#' @return A leaflet map object showing the chosen indicator across the selected geographic level,
#' with an appropriate tooltip and legend.
#'
#' @import dplyr
#' @import stringr
#' @import leaflet
#' @import htmltools
#' @import sf
#' @import leaflet.extras2
#' @noRd
map_veloroute_indicators <- function(data_cat_terr, veloroute_data, admin_sf,
                                     tab_typology, display_typology,
                                     selected_territory, indicator_choice,
                                     info_to_display, schema_type, current_year, ns){

  insee_col_admin <- switch(display_typology,
                            region = "INSEE_REG",
                            departement = "INSEE_DEP")

  insee_col_cat_terr <- switch(display_typology,
                               region = "REG",
                               departement = "DEP")

  previous_year <- current_year - 1


  # ---- COMPUTE INDICATORS ----

  # Completion rate
  if (indicator_choice == "taux_realisation"){
    # Extract useful columns
    suffixes <- c(
      paste0("_SITE-PARTAGE_", previous_year),
      paste0("_SITE-PROPRE_", previous_year),
      paste0("_SITE-PARTAGE_", current_year),
      paste0("_SITE-PROPRE_", current_year),
      paste0("_NON-REAL_", current_year)
    )

    if (schema_type != "SNV-EV"){

      cols_to_select <- c(insee_col_admin, paste0(schema_type, suffixes))

      veloroute_data <- veloroute_data |>
        select(all_of(cols_to_select))

    } else if (schema_type == "SNV-EV"){

      # Sum EV and SNV-STRICT to get full SNV
      for (suf in suffixes) {
        ev_col  <- paste0("EV", suf)
        snv_col <- paste0("SNV-STRICT", suf)
        new_col <- paste0("SNV-EV", suf)

        ev_vals  <- if (ev_col  %in% names(veloroute_data)) veloroute_data[[ev_col]]  else rep(0, nrow(veloroute_data))
        snv_vals <- if (snv_col %in% names(veloroute_data)) veloroute_data[[snv_col]] else rep(0, nrow(veloroute_data))

        ev_vals  <- ifelse(is.na(ev_vals), 0, ev_vals)
        snv_vals <- ifelse(is.na(snv_vals), 0, snv_vals)

        veloroute_data[[new_col]] <- ev_vals + snv_vals
      }
      veloroute_data <- veloroute_data |>
        select(any_of(insee_col_admin), starts_with("SNV-EV"))
    }

    # Compute completion rate
    indicator_data <- veloroute_data |>
      mutate(
        total = rowSums(
          across(ends_with(as.character(current_year))),
          na.rm = TRUE
        ),
        completion_rate_current = (
          rowSums(
            across(matches(paste0("(SITE-PROPRE_|SITE-PARTAGE_)", current_year))),
            na.rm = TRUE
          ) / total
        ) * 100,
        completion_rate_previous = (
          rowSums(
            across(matches(paste0("(SITE-PROPRE_|SITE-PARTAGE_)", previous_year))),
            na.rm = TRUE
          ) / total
        ) * 100,
        evolution = completion_rate_current - completion_rate_previous
      )
  } else if (indicator_choice == "km_restant"){
    # Kilometers left to complete

    suffixes <- c(
      paste0("_REAL_", current_year),
      paste0("_NON-REAL_", current_year)
    )

    # Extract useful columns
    if (schema_type != "SNV-EV"){

      cols_to_select <- c(insee_col_admin, paste0(schema_type, suffixes))

      veloroute_data <- veloroute_data |>
        select(all_of(cols_to_select))

    } else if (schema_type == "SNV-EV"){

      # Sum EV and SNV-STRICT to get full SNV
      for (suf in suffixes) {
        ev_col  <- paste0("EV", suf)
        snv_col <- paste0("SNV-STRICT", suf)
        new_col <- paste0("SNV-EV", suf)

        ev_vals  <- if (ev_col  %in% names(veloroute_data)) veloroute_data[[ev_col]]  else rep(0, nrow(veloroute_data))
        snv_vals <- if (snv_col %in% names(veloroute_data)) veloroute_data[[snv_col]] else rep(0, nrow(veloroute_data))

        ev_vals  <- ifelse(is.na(ev_vals), 0, ev_vals)
        snv_vals <- ifelse(is.na(snv_vals), 0, snv_vals)

        veloroute_data[[new_col]] <- ev_vals + snv_vals
      }
      veloroute_data <- veloroute_data |>
        select(any_of(insee_col_admin), starts_with("SNV-EV"))
    }
  }

  # Rename columns
  if (indicator_choice == "taux_realisation"){
    indicator_data <- indicator_data  |>
      rename(!!ifelse(info_to_display == "value", "value", "label") := completion_rate_current,
             !!ifelse(info_to_display == "value", "label", "value") := evolution)
  } else if (indicator_choice == "km_restant"){
    indicator_data <- veloroute_data  |>
      rename(!!ifelse(info_to_display == "value", "value", "label") := !!sym(paste0(schema_type, "_NON-REAL_", current_year)),
             !!ifelse(info_to_display == "value", "label", "value") := !!sym(paste0(schema_type, "_REAL_", current_year)))
  }


  # ---- PREPARE DATA ----

  # Join administrative data with aménagement data
  admin_sf <- admin_sf |>
    left_join(indicator_data |>
                mutate(across(all_of(insee_col_admin), as.character)) |>
                select(all_of(insee_col_admin), value, label),
              by = insee_col_admin)

  # Extract data corresponding to the territory selected by the user, according to the selected display scale
  if (tab_typology != "france"){
    if(tab_typology == "region"){
      list <- data_cat_terr |>
        filter(REG == selected_territory)
    }
    list <- list |>  pull(.data[[insee_col_cat_terr]]) |>  unique()
    admin_sf <- admin_sf |>  filter(.data[[insee_col_admin]] %in% list)
  }

  # Get territory name depending on the selected display scale
  admin_sf <- admin_sf |>
    mutate(territory_name = .data[[switch(
      display_typology,
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
  }


  # ---- LEGEND TITLE ----

  schema_type_label <- case_when(
    schema_type == "EV" ~ "schéma EuroVelo strict",
    schema_type == "SNV-STRICT" ~ "schéma national strict",
    schema_type == "SRV-STRICT" ~ "schéma régional strict",
    schema_type == "SDV-STRICT" ~ "schéma départemental strict",
    schema_type == "SNV-EV" ~ "schéma national (EuroVelo inclus)"
  )

  legend_title <- case_when(
    info_to_display == "value" & indicator_choice == "taux_realisation" ~
      paste0("Taux de réalisation des véloroutes du<br/>", schema_type_label," (%)"),
    info_to_display == "evol" & indicator_choice == "taux_realisation" ~
      paste0("Évolution du taux de réalisation des véloroutes du<br/>", schema_type_label, " par rapport à ", previous_year, " (points)"),
    info_to_display == "value" & indicator_choice == "km_restant" ~
      paste0("Nombre de kilomètres restant à réaliser<br/>sur le ", schema_type_label),
    info_to_display == "evol" & indicator_choice == "km_restant" ~
      paste0("Nouvelles réalisations sur le<br/>", schema_type_label, " en ", current_year, " (km)"),
    TRUE ~ "Légende"
  )


  # ---- COLOR PALETTE AND LEGEND ----

  if (indicator_choice == "taux_realisation" & info_to_display == "evol"){
    if ((display_typology == "departement" & schema_type == "EV") | (display_typology == "departement" & schema_type == "SNV-EV")){
      color_palette_legend <- color_palette_map_evol(0, 10,
                                                     c("#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("0 – 5", "5 – 10", "> 10"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    } else {
      color_palette_legend <- color_palette_map_evol(0, 5,
                                                     c("#F6F9ED", "#dce7a0", "#b8ce41"),
                                                     c("0 – 2,5", "2,5 – 5", "> 5"),
                                                     "#F6F9ED", "#b8ce41", "#223f82", "#e9ecf3",
                                                     admin_sf)
    }
  } else if (indicator_choice == "taux_realisation" & info_to_display == "value") {
    if (schema_type == "SNV-STRICT"){
      color_palette_legend <- color_palette_map_value(100,
                                                      c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                      c("< 40%", "40% – 60%", "60% – 80%", "80% – 100%"),
                                                      "#f4f5d9", "#9ea534",
                                                      admin_sf, 40)
    } else if (schema_type %in% c("EV", "SNV-EV")){
      if (display_typology == "departement"){
        color_palette_legend <- color_palette_map_value(100,
                                                        c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                        c("< 50%", "50% – 65%", "65% – 80%", "80% – 100%"),
                                                        "#f4f5d9", "#9ea534",
                                                        admin_sf, 50)
      } else if (display_typology == "region"){
        color_palette_legend <- color_palette_map_value(100,
                                                        c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                        c("< 60%", "60% – 75%", "75% – 90%", "90% – 100%"),
                                                        "#f4f5d9", "#9ea534",
                                                        admin_sf, 60)
      }
    } else {
      color_palette_legend <- color_palette_map_value(100,
                                                      c("#f4f5d9", "#e3e7a0", "#d1d867", "#9ea534"),
                                                      c("< 30%", "30% – 50%", "50% – 75%", "75% – 100%"),
                                                      "#f4f5d9", "#9ea534",
                                                      admin_sf, 30)
    }
  } else if (indicator_choice == "km_restant" & info_to_display == "value"){
    if (schema_type == "EV"){
      color_palette_legend <- color_palette_map_value(100,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 km – 30 km", "30 km – 60 km", "60 km – 100 km", "> 100 km"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf, 0.1)
    } else if (display_typology == "departement"){
      color_palette_legend <- color_palette_map_value(150,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 km – 50 km", "50 km – 100 km", "100 km – 150 km", "> 150 km"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf, 0.1)
    } else if (schema_type %in% c("SNV-EV", "SNV-STRICT")){
      color_palette_legend <- color_palette_map_value(500,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 km – 100 km", "100 km – 300 km", "300 km – 500 km", "> 500 km"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf, 0.1)
    } else if (schema_type == "SRV-STRICT"){
      color_palette_legend <- color_palette_map_value(800,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 km – 300 km", "300 km – 600 km", "600 km – 800 km", "> 800 km"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf, 0.1)
    } else if (schema_type == "SDV-STRICT"){
      color_palette_legend <- color_palette_map_value(300,
                                                      c("#e9ecf3", "#c1c3d6", "#6e749f", "#303876"),
                                                      c("0 km – 100 km", "100 km – 200 km", "200 km – 300 km", "> 300 km"),
                                                      "#e9ecf3", "#303876",
                                                      admin_sf, 0.1)
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
      admin_cache = NULL) |>

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
          tooltip_label_veloroute(val, lab, info_to_display, indicator_choice, current_year, schema_type), "</span>"
        )
      }, val, lab, admin_sf$territory_name) |>

        lapply(htmltools::HTML),

      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "14px", direction = "auto")
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
  if (tab_typology == "france"){
    map <- map %>%
      addControl(
        html = HTML(
          paste0(
            '<label for="', ns("territoire_select"), '" style="font-weight:bold; display:block; margin-bottom:5px;">Vue (métropole ou DROM) :</label>',
            '<select id="', ns("territoire_select"), '" ',
            'style="font-size:14px; padding:2px; border: 1px solid #ccc; width: ', 210, 'px">',
            paste0('<option value="', names(zones()), '">', names(zones()), '</option>', collapse = ''),
            '</select>'
          )
        ),
        position = "bottomleft"
      )
  }

  return(map)
}



#' @title Generate an interactive Leaflet map of cycling routes
#'
#' @description This function creates a Leaflet map displaying cycling routes (véloroutes) within a selected
#' French territory (region or department). The map distinguishes routes by their typology
#' (EuroVelo, strictly national, regional, or departmental) using colors, and by their completion
#' status using line style (filled for completed, white with borders for not completed).
#' The function also displays the administrative boundaries of the selected territory and includes
#' a combined legend for typology and completion status.
#'
#' @param veloroute_sf An \code{sf} object containing the cycling route segments. It must include
#'   columns \code{SCHEMA} (cycling route typology), \code{AVANCEMENT} (progress), and
#'   the relevant administrative codes (\code{INSEE_REG} or \code{INSEE_DEP}) for filtering.
#' @param admin_sf An \code{sf} object containing the boundaries of French administrative
#'   territories (regions or departments). It must include the same administrative code column
#'   (\code{INSEE_REG} or \code{INSEE_DEP}) as \code{veloroute_sf}.
#' @param tab_typology Character. Indicates the type of territory to display: either \code{"region"}
#'   or \code{"departement"}.
#' @param selected_territory Character. INSEE code of the territory to display.
#'
#' @return A \code{leaflet} map object with:
#'   \itemize{
#'     \item Cycling routes colored by typology.
#'     \item Line style indicating completion status (filled for completed, white with borders for not completed).
#'     \item Boundaries of the selected territory.
#'     \item A combined legend showing both typology and progress.
#'   }
#'
#' @import sf
#' @import leaflet
#' @import dplyr
#' @import leaflet.extras2
#' @noRd
map_network_progress_region_dep <- function(veloroute_sf, admin_sf,
                                            tab_typology, selected_territory){

  # Filter data according to displayed territory
  code_col <- switch(tab_typology,
                     region = "INSEE_REG",
                     departement = "INSEE_DEP")

  veloroute_filtered_sf <- veloroute_sf |>
    filter(!!sym(code_col) == selected_territory)

  boundaries <- admin_sf |>
    filter(!!sym(code_col) == selected_territory)


  # Custom color palette
  schema_levels <- c("EV", "SNV-STRICT", "SRV-STRICT", "SDV-STRICT")
  schema_colors <- c("#303876", "#E94F35", "#C6CE41", "#ECB2D2")

  veloroute_filtered_sf  <- veloroute_filtered_sf |>
    mutate(
      SCHEMA = factor(SCHEMA, levels = schema_levels),
      couleur = schema_colors[as.numeric(SCHEMA)],
      couleur_finale = ifelse(AVANCEMENT == "REAL", couleur, "white")
    )


  # Zoom and centering
  if (tab_typology == "region") {
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


  # Labels for legend
  schema_labels <- c(
    "EuroVelo",
    "Strictement nationale",
    "Strictement régionale",
    "Strictement départementale"
  )

  veloroute_filtered_sf$label_html <- lapply(veloroute_filtered_sf$LISTE_ITI, HTML)


  # Leaflet map
  map <- leaflet(veloroute_filtered_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom) |>

    # Displayed territory boundaries
    addPolygons(
      data = boundaries,
      fill = FALSE,
      color = "#444444",
      weight = 0.9,
      opacity = 1
    )

  if (nrow(veloroute_filtered_sf) != 0){

    map <- map |>

      # First layer
      addPolylines(
        data = veloroute_filtered_sf,
        color = ~couleur,
        weight = 5,
        opacity = 1,
        label = ~label_html,
        labelOptions = labelOptions(
          html = TRUE
        )
      ) |>

      # Second layer to distinguish between completed and uncompleted portions
      addPolylines(
        data = veloroute_filtered_sf,
        color = ~couleur_finale,
        weight = 2,
        opacity = 1,
        label = ~label_html,
        labelOptions = labelOptions(
          html = TRUE
        )
      )
  }

  map <- map |>

    # Legend
    addControl(html = HTML(
      paste0(
        "<div style='font-size:12px; line-height:1.2em'>",
        "<b>Véloroute</b><br/>",
        paste0(
          "<div style='display:flex; align-items:center; margin-top:5px'>",
          "<div style='width:20px; height:10px; background:", schema_colors, "; margin-right:5px;'></div>",
          schema_labels,
          "</div>", collapse = ""
        ),
        "<br/><b>Avancement</b><br/>",
        "<div style='display:flex; align-items:center; margin-top:5px'>",
        "<div style='width:20px; height:7px; background:black; margin-right:5px;'></div>",
        "Réalisé",
        "</div>",
        "<div style='display:flex; align-items:center; margin-top:5px'>",
        "<div style='width:20px; height:5px; background:white; border-top:2px solid black; border-bottom:2px solid black; margin-right:5px;'></div>",
        "Non réalisé",
        "</div>",
        "</div>"
      )
    ), position = "bottomleft") |>

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carte_amenagement",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  return(map)
}



#' @title Display a national map of EuroVelo and national cycling routes by progress level
#'
#' @description This function generates an interactive \code{leaflet} map displaying France’s
#' national and EuroVelo cycling routes (\emph{véloroutes nationales et européennes}).
#' Each route is colored according to its overall level of progress, which indicates how
#' close the route is to completion. The function also overlays the metropolitan
#' France boundary and includes a downloadable export option for the map.
#'
#' @param veloroute_sf An \code{sf} object containing cycling route segments for all of France.
#'   It must include at least the following columns:
#'   \itemize{
#'     \item \code{SCHEMA} – the typology of the route (\code{"EV"}, \code{"SNV-STRICT"}, etc.)
#'     \item \code{NIVEAU_AVANCEMENT} – a categorical variable describing the level of progress
#'       (e.g. “achevée ou proche”, “très avancée”, “avancée”, “peu avancée”)
#'     \item \code{id} and \code{nom} – used for route identification in labels
#'   }
#' @param contour_metropole An \code{sf} object representing the geographic boundary of
#'   metropolitan France (used as the background polygon layer).
#'
#' @return A \code{leaflet} map widget displaying:
#'   \itemize{
#'     \item EuroVelo and strictly national cycling routes colored by their progress level;
#'     \item Administrative boundaries of metropolitan France;
#'     \item A legend explaining the color scheme for progress levels;
#'     \item A download control allowing the user to export the map as an image or PDF.
#'   }
#'
#' @import sf
#' @import leaflet
#' @import dplyr
#' @import htmlwidgets
#' @import leaflet.extras2
#' @noRd
map_network_progress_france <- function(veloroute_sf, contour_metropole, ns){

  # Only keep EuroVelo and national cycling routes
  veloroute_national_sf <- veloroute_sf |>
    filter(SCHEMA %in% c("EV", "SNV-STRICT"))

  # Custom color palette
  avancement_levels <- c("Achevée ou proche", "Très avancée", "Avancée", "Peu avancée")
  avancement_colors <- c("#7d8221", "#c6ce3e", "#b1d7e6", "#294754")

  veloroute_national_sf  <- veloroute_national_sf |>
    mutate(
      NIVEAU_AVANCEMENT = factor(NIVEAU_AVANCEMENT, levels = avancement_levels),
      couleur = avancement_colors[as.numeric(NIVEAU_AVANCEMENT)]
    )


  # Zoom and centering
  lng <- 2.30
  lat <- 46.50
  zoom <- 6


  # Labels for legend
  avancement_labels <- c(
    "Achevée ou proche (>95%)",
    "Très avancée (75 à 95%)",
    "Avancée (50 à 75%)",
    "Peu avancée (<50%)"
  )

  veloroute_national_sf$label_html <- lapply(veloroute_national_sf$LISTE_ITI, HTML)

  # Leaflet map
  map <- leaflet(veloroute_national_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom) |>

    # Displayed territory boundaries
    addPolygons(
      data = contour_metropole,
      color = "#000000", weight = 1.5, opacity = 0.6,
      fill = FALSE
    ) |>

    # Cycling routes
    addPolylines(
      data = veloroute_national_sf,
      color = ~couleur,
      weight = 5,
      opacity = 1,
      label = ~label_html,
      labelOptions = labelOptions(
        html = TRUE
      )
    ) |>

    # Legend
    addControl(html = HTML(
      paste0(
        "<div style='font-size:14px; line-height:1.2em'>",
        "<b>Avancement de la véloroute</b><br/>",
        paste0(
          "<div style='display:flex; align-items:center; margin-top:5px'>",
          "<div style='width:20px; height:10px; background:", avancement_colors, "; margin-right:5px;'></div>",
          avancement_labels,
          "</div>", collapse = ""
        )
      )
    ), position = "bottomleft") |>

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carte_amenagement",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  select_width <- 213
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

  return(map)
}



#' @title Display a Leaflet Map of Recent Cycling Route Achievements
#'
#' @description This function generates an interactive Leaflet map that visualizes cycling route segments
#' completed or upgraded within the past five years in a selected French administrative territory
#' (region or department). Each segment is colored according to its year of completion,
#' while older segments are shown in light gray in the background.
#'
#' @param veloroute_sf An `sf` object containing cycling route segments with at least the following columns:
#'   - `INSEE_REG` or `INSEE_DEP`: administrative codes,
#'   - `ANNEE`: completion year or `NA`,
#'   - `id` and `nom`: unique segment identifiers and names.
#'   This dataset should include geometry (`LINESTRING`) for the route network.
#' @param admin_sf An `sf` object containing the administrative boundaries of France
#' (regions or departments), used to delineate the displayed area.
#' @param current_year An integer specifying the reference year for visualization.  #'
#' @param tab_typology A character string indicating the administrative level to display:
#' either `"region"` or `"departement"`.
#' @param selected_territory A character string giving the INSEE code of the region or department
#' to display.
#'
#' @return
#' A `leaflet` map object displaying cycling route achievements by year of realization,
#' ready for integration in Shiny or R Markdown applications.
#'
#' @import dplyr
#' @import sf
#' @import leaflet
#' @import leaflet.extras2
#' @importFrom rlang sym
#' @noRd
map_new_achievements <- function(veloroute_sf, admin_sf, current_year,
                                 tab_typology, selected_territory){

  # Filter data according to displayed territory
  code_col <- switch(tab_typology,
                     region = "INSEE_REG",
                     departement = "INSEE_DEP")

  veloroute_filtered_sf <- veloroute_sf |>
    filter(!!sym(code_col) == selected_territory)

  boundaries <- admin_sf |>
    filter(!!sym(code_col) == selected_territory)

  # Process dates
  veloroute_filtered_sf <- veloroute_filtered_sf |>
    mutate(ANNEE = ifelse(is.na(ANNEE), "ANTERIEUR", ANNEE))

  # Custom color palette
  date_levels <- as.character(c(seq(current_year, current_year - 4), "ANTERIEUR"))
  date_colors <- c("#E94F35", "#F3C460", "#C6CE41", "#B1D6E4", "#294754", "#bebebe")

  veloroute_filtered_sf  <- veloroute_filtered_sf |>
    mutate(
      ANNEE = factor(ANNEE, levels = date_levels),
      couleur = date_colors[ANNEE]
    )

  # Zoom and centering
  if (tab_typology == "region") {
    centroids <- get_centroids(selected_territory, tab_typology)
    lng <- centroids$lng; lat <- centroids$lat
    if (selected_territory %in% c("75")){
      zoom <- 6.9
    } else if (selected_territory %in% c("76", "84", "03", "94", "44", "27")){
      zoom <- 7.3
    } else if (selected_territory %in% c("01", "02", "04", "06", "11")){
      zoom <- 8.7
    } else if (selected_territory %in% c("32", "28", "52", "93")){
      zoom <- 7.7
    } else if (selected_territory == "24"){
        zoom <- 7.5
    } else {zoom <- 8}
  } else if (tab_typology == "departement"){
    centroids <- get_centroids(selected_territory, tab_typology)
    lng <- centroids$lng; lat <- centroids$lat
    zoom <- 8.2
  }

  # Labels for legend
  date_labels <- c(
    "2024",
    "2023",
    "2022",
    "2021",
    "2020",
    "Réalisations antérieures"
  )

  veloroute_filtered_sf$label_html <- lapply(veloroute_filtered_sf$LISTE_ITI, HTML)

  # Leaflet map
  map <- leaflet(veloroute_filtered_sf, options = leafletOptions(minZoom = 4, zoomSnap = 0.1, zoomDelta = 0.1), height = 700) |>
    addProviderTiles("CartoDB.PositronNoLabels") |>
    setView(lng = lng, lat = lat, zoom = zoom) |>

    # Displayed territory boundaries
    addPolygons(
      data = boundaries,
      fill = FALSE,
      color = "#444444",
      weight = 0.9,
      opacity = 1
    )

  if (nrow(veloroute_filtered_sf) != 0) {

    map <- map |>

      # First layer
      # addPolylines(
      #   data = veloroute_filtered_sf |> filter(ANNEE == "ANTERIEUR"),
      #   color = ~couleur,
      #   weight = 2,
      #   opacity = 1,
      #   label = ~label_html,
      #   labelOptions = labelOptions(
      #     html = TRUE
      #   ),
      #   dashArray = ~ifelse(AVANCEMENT == "NON-REAL", "3,5", "1")
      # ) |>

      # Older portions
      addPolylines(
        data = veloroute_filtered_sf |> filter(ANNEE == "ANTERIEUR"),
        color = ~couleur,
        weight = 3,
        opacity = 1,
        label = ~label_html,
        labelOptions = labelOptions(
          html = TRUE
        )
      ) |>

      # Unrealized portions
      addPolylines(
        data = veloroute_filtered_sf |> filter(ANNEE == "ANTERIEUR" & AVANCEMENT == "NON-REAL"),
        color = "white",
        weight = 1,
        opacity = 1,
        label = ~label_html,
        labelOptions = labelOptions(
          html = TRUE
        )
      ) |>

      # Recent portions
      addPolylines(
        data = veloroute_filtered_sf |> filter(ANNEE != "ANTERIEUR"),
        color = ~couleur,
        weight = 5,
        opacity = 1,
        label = ~label_html,
        labelOptions = labelOptions(
          html = TRUE
        )
      )
  }

  map <- map |>

    # Legend
    addControl(html = HTML(
      paste0(
        "<div style='font-size:12px; line-height:1.2em'>",
        "<b>Année de réalisation</b><br/>",
        paste0(
          "<div style='display:flex; align-items:center; margin-top:5px'>",
          "<div style='width:20px; height:10px; background:", date_colors, "; margin-right:5px;'></div>",
          date_labels,
          "</div>", collapse = ""
        ),
        "<div style='display:flex; align-items:center; margin-top:4px;'>",
        "<div style='width:20px; height:5px; background:white; border-top:2px solid #bebebe; border-bottom:2px solid #bebebe; margin-right:5px;'></div>",
        "<span style='font-size:12px;'>Portions non réalisées</span>",
        "</div>",
        "</div>"
      )
    ), position = "bottomleft") |>

    addEasyprint(options = easyprintOptions(title = "Télécharger la carte", position = "topleft",
                                            exportOnly = TRUE, filename = "carte_amenagement",
                                            hideControlContainer = FALSE, sizeModes = list("CurrentSize", "A4Portrait", "A4Landscape"),
                                            defaultSizeTitles = list(
                                              "A4Portrait" = "Télécharger en A4 Portrait",
                                              "A4Landscape" = "Télécharger en A4 Paysage",
                                              "CurrentSize" = "Télécharger à la taille actuelle"
                                            )))

  return(map)
}



#' @title Generate value box content for veloroute indicators
#'
#' @description This function computes key indicators related to the
#' realization and status of cycle route networks, and returns the values and labels
#' required to populate value boxes in a Shiny dashboard.
#' Depending on the `vb_type` argument, the function calculates one of several
#' possible indicators: total kilometers realized, share of site-propre sections,
#' yearly new achievements, completion rates by schema type, or kilometers remaining.
#'
#' @param veloroute_data A data frame containing veloroute metrics, including
#'   columns such as `EV_SITE-PROPRE_2024`, `SNV-STRICT_NON-REAL_2024`, etc.
#'   Each column corresponds to a different schema, type, or year.
#' @param current_year Numeric. The current year of analysis (e.g., `2024`).
#' @param tab_typology Character; the aggregation level used for indicator computation.
#'   Expected values: \code{"france"} (no territorial filter), \code{"region"} or \code{"departement"}.
#' @param selected_cog Character. The INSEE code of the selected region or department.
#'   Used to subset `veloroute_data` to the chosen territory.
#' @param vb_type Character. The type of value box to generate.
#'   Must be one of:
#'   \describe{
#'     \item{`"taux_realisation"`}{Completion rate of the selected veloroute schema.}
#'     \item{`"km_restant"`}{Kilometers remaining to be realized in the selected schema.}
#'     \item{`"total"`}{Total length of realized veloroutes in the territory.}
#'     \item{`"status"`}{Percentage of veloroutes in *site propre* vs. *site partagé*.}
#'     \item{`"new_achievements"`}{Kilometers newly realized during the current year.}
#'   }
#' @param schema_type Character. The veloroute schema type to be analyzed.
#'   Examples include `"EV"`, `"SNV-STRICT"`, `"SRV-STRICT"`, `"SDV-STRICT"`,
#'   or `"SNV-EV"` (which merges EuroVelo and SNV strict networks).
#'
#' @return A list with the following named elements:
#' \describe{
#'   \item{`title`}{The title text to display in the value box.}
#'   \item{`value`}{The main formatted numeric value (e.g., kilometers or percentages).}
#'   \item{`evol`}{The evolution compared to the previous year, formatted with sign and units.}
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @noRd
value_box_content_veloroute <- function(veloroute_data, current_year,
                                        tab_typology, selected_cog,
                                        vb_type, schema_type){

  previous_year <- current_year - 1

  ## Filter territory if needed
  if (tab_typology == "region"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_REG == selected_cog)
  } else if (tab_typology == "departement"){
    veloroute_data <- veloroute_data |>
      filter(INSEE_DEP == selected_cog)
  }

  # --- Total number of kilometers ---
  if (vb_type == "total"){

    cols <- c(paste0("TOUS-SCHEMAS_SITE-PARTAGE_", current_year),
              paste0("TOUS-SCHEMAS_SITE-PROPRE_", current_year),
              paste0("TOUS-SCHEMA_SITE-PARTAGE_", previous_year),
              paste0("TOUS-SCHEMA_SITE-PROPRE_", previous_year))


    df <- veloroute_data |> select(all_of(cols))

    total_current <- df |>
      select(ends_with(as.character(current_year))) |>
      rowSums(na.rm = TRUE)

    total_previous <- df |>
      select(ends_with(as.character(previous_year))) |>
      rowSums(na.rm = TRUE)

    evol <- total_current - total_previous

    result <- list(title = "Total véloroutes réalisées",
                   value = paste0(format(round(total_current, 0), big.mark = " "), " km"),
                   evol = paste0("+", format(round(evol, 0), big.mark = " "), " km par rapport à ", previous_year))

  } else if (vb_type == "status"){

    # --- Status percentage ---

    cols <- c(paste0("TOUS-SCHEMAS_SITE-PARTAGE_", current_year),
              paste0("TOUS-SCHEMAS_SITE-PROPRE_", current_year),
              paste0("TOUS-SCHEMA_SITE-PARTAGE_", previous_year),
              paste0("TOUS-SCHEMA_SITE-PROPRE_", previous_year))

    df <- veloroute_data |> select(all_of(cols))

    pct_site_propre_current <- df[[paste0("TOUS-SCHEMAS_SITE-PROPRE_", current_year)]] /
      (df[[paste0("TOUS-SCHEMAS_SITE-PROPRE_", current_year)]] +
         df[[paste0("TOUS-SCHEMAS_SITE-PARTAGE_", current_year)]]) * 100

    pct_site_propre_previous <- df[[paste0("TOUS-SCHEMA_SITE-PROPRE_", previous_year)]] /
      (df[[paste0("TOUS-SCHEMA_SITE-PROPRE_", previous_year)]] +
         df[[paste0("TOUS-SCHEMA_SITE-PARTAGE_", previous_year)]]) * 100

    evol <- pct_site_propre_current - pct_site_propre_previous
    unit <- ifelse(evol < 2, "point", "points")
    evol <- paste0(ifelse(evol >=0, "+", ""), format(round(evol, 1), decimal.mark = ","), " ", unit, " par rapport à ", previous_year)

    result <- list(title = "Part en site propre",
                   value = paste0(format(round(pct_site_propre_current, 1), decimal.mark = ","), "%"),
                   evol = evol)

  } else if (vb_type == "new_achievements"){

    # --- New achievements of the year ---

    cols <- c(paste0("TOUS-SCHEMA_SITE-PARTAGE_REAL_", current_year),
              paste0("TOUS-SCHEMA_SITE-PROPRE_REAL_", current_year),
              paste0("TOUS-SCHEMA_SITE-PARTAGE_REAL_", previous_year),
              paste0("TOUS-SCHEMA_SITE-PROPRE_REAL_", previous_year))


    df <- veloroute_data |> select(all_of(cols))

    real_current <- df |>
      select(ends_with(as.character(current_year))) |>
      rowSums(na.rm = TRUE)

    real_previous <- df |>
      select(ends_with(as.character(previous_year))) |>
      rowSums(na.rm = TRUE)

    evol <- real_current - real_previous

    result <- list(title = paste0("Réalisations ", current_year),
                   value = paste0(format(round(real_current, 0), big.mark = " "), " km"),
                   evol = paste0(ifelse(evol >= 0, "+", ""), format(round(evol, 0), big.mark = " "), " km par rapport à ", previous_year))

  } else if (vb_type == "taux_realisation"){

    # --- Completion rate ---

    suffixes <- c(
      paste0("_SITE-PARTAGE_", previous_year),
      paste0("_SITE-PROPRE_", previous_year),
      paste0("_SITE-PARTAGE_", current_year),
      paste0("_SITE-PROPRE_", current_year),
      paste0("_NON-REAL_", current_year)
    )

    if (schema_type != "SNV-EV"){

      cols_to_select <- c(paste0(schema_type, suffixes))

      veloroute_data <- veloroute_data |> select(all_of(cols_to_select))

    } else if (schema_type == "SNV-EV"){

      # Sum EV and SNV-STRICT to get full SNV
      for (suf in suffixes) {
        ev_col  <- paste0("EV", suf)
        snv_col <- paste0("SNV-STRICT", suf)
        new_col <- paste0("SNV-EV", suf)

        ev_vals  <- if (ev_col  %in% names(veloroute_data)) veloroute_data[[ev_col]]  else rep(0, nrow(veloroute_data))
        snv_vals <- if (snv_col %in% names(veloroute_data)) veloroute_data[[snv_col]] else rep(0, nrow(veloroute_data))

        ev_vals  <- ifelse(is.na(ev_vals), 0, ev_vals)
        snv_vals <- ifelse(is.na(snv_vals), 0, snv_vals)

        veloroute_data[[new_col]] <- ev_vals + snv_vals
      }
      veloroute_data <- veloroute_data |> select(starts_with("SNV-EV"))
    }

    indicator_data <- veloroute_data |>
      mutate(
        total = rowSums(
          across(ends_with(as.character(current_year))),
          na.rm = TRUE
        ),
        completion_rate_current = (
          rowSums(
            across(matches(paste0("(SITE-PROPRE_|SITE-PARTAGE_)", current_year))),
            na.rm = TRUE
          ) / total
        ) * 100,
        completion_rate_previous = (
          rowSums(
            across(matches(paste0("(SITE-PROPRE_|SITE-PARTAGE_)", previous_year))),
            na.rm = TRUE
          ) / total
        ) * 100,
        evolution = completion_rate_current - completion_rate_previous
      )

    unit <- ifelse(indicator_data$evolution < 2, "point", "points")
    evol <- paste0(ifelse(indicator_data$evolution >=0, "+", ""), format(round(indicator_data$evolution, 1), decimal.mark = ","), " ", unit, "  par rapport à ", previous_year)

    title <- switch(schema_type,
                    "EV"         = "Taux de réalisation EuroVelo",
                    "SNV-STRICT" = "Taux de réalisation SNV strict",
                    "SRV-STRICT" = "Taux de réalisation SRV strict",
                    "SDV-STRICT" = "Taux de réalisation SDV strict",
                    "SNV-EV"     = "Taux de réalisation SNV<br>(EuroVelo inclus)")

    result <- list(title = title,
                   value = paste0(format(round(indicator_data$completion_rate_current, 1), decimal.mark = ","), "%"),
                   evol = evol)

  } else if (vb_type == "km_restant"){

    # --- Kilometers left to achieve ---

    # Extract useful columns
    if (schema_type != "SNV-EV"){

      cols_to_select <- c(paste0(schema_type, "_NON-REAL_", current_year))

      km_restant <- veloroute_data |> pull(all_of(cols_to_select))

    } else if (schema_type == "SNV-EV"){

      # Sum EV and SNV-STRICT to get full SNV
      ev_col  <- paste0("EV", "_NON-REAL_", current_year)
      snv_col <- paste0("SNV-STRICT", "_NON-REAL_", current_year)

      ev_vals  <- if (ev_col  %in% names(veloroute_data)) veloroute_data[[ev_col]]  else rep(0, nrow(veloroute_data))
      snv_vals <- if (snv_col %in% names(veloroute_data)) veloroute_data[[snv_col]] else rep(0, nrow(veloroute_data))

      ev_vals  <- ifelse(is.na(ev_vals), 0, ev_vals)
      snv_vals <- ifelse(is.na(snv_vals), 0, snv_vals)

      km_restant <- ev_vals + snv_vals
    }

    title <- switch(schema_type,
                    "EV"         = "EuroVelo<br>Restant à réaliser",
                    "SNV-STRICT" = "SNV strict<br>Restant à réaliser",
                    "SRV-STRICT" = "SRV strict<br>Restant à réaliser",
                    "SDV-STRICT" = "SDV strict<br>Restant à réaliser",
                    "SNV-EV"     = "SNV (EuroVelo inclus)<br>Restant à réaliser")

    result <- list(title = title,
                   value = paste0(format(round(km_restant, 0), big.mark = " "), " km"),
                   evol = "")

  }

  # Case when selected territory has no cycle route
  if (nrow(veloroute_data) == 0){
    result$value <- "-"
    result$evol <- ""
  }

  return(result)
}



