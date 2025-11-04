#' @title Generate the introduction text for each module
#'
#' @description
#' Generates and returns the introduction HTML content to display at the top of each module tab.
#' This function reads a base HTML file and replaces predefined placeholders with territory-specific
#' information, including the territory name and its category. It ensures that the introductory text
#' is dynamically adapted to the selected administrative level and typology (commune, département, etc.).
#'
#' @param path Character string. The path to the pre-existing HTML file containing the base structure with placeholders.
#' @param validated_selection A `reactiveValues` object containing the territory
#' selected by the user.
#' @param data_cat_terr Data frame containing the territorial data, including INSEE codes and names.
#'
#' @return An `HTML` object ready to be rendered within the UI, containing the customized introduction card for the selected territory.
#'
#' @noRd
generate_text_intro <- function(path, validated_selection, data_cat_terr){

  # Read base HTML file
  html_lines <- readLines(path)

  # Get the territory infos
  territory_name <- get_territory_infos(validated_selection, data_cat_terr)$name
  territory_category <- get_territory_infos(validated_selection, data_cat_terr)$libcat

  # Custom TERRITORY_NAME with the actual name
  html_lines <- gsub(
    "TERRITORY_NAME",
    territory_name,
    html_lines
  )

  # Custom TERRITORY_CATEGORY with the actual category
  if (validated_selection$last_validated_typology %in% c("france", "region", "epci")){
    custom_text <- ""
  } else if (validated_selection$last_validated_typology == "commune"){
    custom_text <- paste0(
      "La commune de <strong>", territory_name, "</strong> appartient à la catégorie des <strong>",
      transform_category(territory_category), "</strong>."
    )
  } else if (validated_selection$last_validated_typology == "departement"){
    custom_text <- paste0(
      "Le département <strong>", territory_name, "</strong> appartient à la catégorie des <strong>",
      transform_category(territory_category), "</strong>.")
  }

  html_lines <- gsub(
    "TERRITORY_CATEGORY",
    custom_text,
    html_lines
  )

  # Return as HTML in the UI
  return(HTML(paste(html_lines, collapse = "\n")))
}



#' @title Generate the introduction text for Fiche Synthèse tab
#'
#' @description
#' Generates and returns the introduction HTML content to display at the top of the Fiche Synthèse tab.
#' This function reads a base HTML file and replaces predefined placeholders with territory-specific
#' information, including the territory name and its category. It ensures that the introductory text
#' is dynamically adapted to the selected administrative level and typology (commune, département, etc.).
#'
#' @param path Character string. The path to the pre-existing HTML file containing the base structure with placeholders.
#' @param validated_selection A `reactiveValues` object containing the territory
#' selected by the user.
#' @param data_cat_terr Data frame containing the territorial data, including INSEE codes and names.
#' @param admin_sf A sf object containing administrative data such as population and surface.
#' @param actions_avelo A data frame containing AVELO program data.
#'
#' @return An `HTML` object ready to be rendered within the UI, containing the customized introduction card for the selected territory.
#'
#' @noRd
generate_text_intro_fiche_synthese <- function(path, validated_selection, data_cat_terr, admin_sf, actions_avelo){

  # Read base HTML file
  html_lines <- readLines(path)

  # Get the territory infos
  territory_infos <- get_territory_infos(validated_selection, data_cat_terr)
  territory_cog <- territory_infos$cog
  territory_name <- territory_infos$name
  territory_category <- territory_infos$libcat
  territory_typology <- get_validated_typology(validated_selection)

  # Custom TERRITORY_NAME with the actual name
  html_lines <- gsub(
    "TERRITORY_NAME",
    territory_name,
    html_lines
  )

  # Custom TERRITORY_CONTEXT with contextual elements
  if (territory_typology == "france"){
    custom_text <- ""
  } else {

    # Extract selected territory from admin_sf
    col_suffix <- case_when(
      territory_typology == "region" ~ "REG",
      territory_typology == "departement" ~ "DEP",
      territory_typology == "epci" ~ "EPCI",
      territory_typology == "commune" ~ "COM"
    )
    admin_sf_filtered <- admin_sf |> filter(!!sym(paste0("INSEE_", col_suffix)) == territory_cog)

    # Typology and category within typology
    typology <- case_when(
      territory_typology == "region" ~ "Région",
      territory_typology == "departement" ~ "Département",
      territory_typology == "epci" ~ "EPCI",
      territory_typology == "commune" ~ "Commune"
    )

    category <- transform_category(territory_category)
    category_formatted <- str_c(str_to_upper(str_sub(category, 1, 1)), str_sub(category, 2))

    # Population, surface and population density
    population <- admin_sf_filtered |> pull(!!sym(paste0("POP_", col_suffix)))
    population_formatted <- format(round(population, 0), big.mark = " ")

    superficie <- admin_sf_filtered |> pull(!!sym(paste0("SUPERFICIE_", col_suffix)))
    superficie_formatted <- format(round(superficie, 0), big.mark = " ")

    densite_pop <- population / superficie
    densite_pop_formatted <- format(round(densite_pop, 1), decimal.mark = ",", big.mark = " ")

    # Check if territory is an AVELO awardee
    col_typo <- ifelse(territory_typology == "epci",
                       c("CC", "CU", "CA", "METRO", "EPT"),
                       col_suffix)

    avelo_data <- actions_avelo |>
      filter(TYPO %in% col_typo) |>
      filter(COG == territory_cog)

    avelo_text <- ifelse(nrow(avelo_data) != 0,
                         paste0("</p><p dir='auto'>Lauréat du programme <strong>AVELO ",
                                avelo_data |> distinct(N_AVELO) |> pull(),
                                "</strong> en ", avelo_data |> distinct(YEAR) |> pull()),
                         "")

    custom_text <- paste0(
      "<strong>Collectivité : </strong>", territory_name, "<br>",
      "<strong>Échelon territorial : </strong>", typology, "<br>",
      "<strong>Catégorie : </strong>", category_formatted, "<br>",
      "<strong>Population : </strong>", population_formatted, " habitants<br>",
      "<strong>Superficie : </strong>", superficie_formatted, " km²<br>",
      "<strong>Densité de population : </strong>", densite_pop_formatted, " habitants par km²",
      avelo_text
      )
  }

 html_lines <- gsub(
    "TERRITORY_CONTEXT",
    custom_text,
    html_lines
  )

  # Return as HTML in the UI
  return(HTML(paste(html_lines, collapse = "\n")))
}




