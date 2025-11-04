#' @title Generate named vector of typology levels for selectInput
#'
#' @description This function returns a named character vector mapping internal typology levels
#' to their corresponding display labels used in the UI selectInput "select_typology".
#'
#' @return Named character vector with internal levels as values and labels as names.
#' @importFrom stats setNames
#'
#' @noRd
select_list_typologies <- function(){
  setNames(c("france", "region", "departement", "epci", "commune"), c("National", "Régional", "Départemental", "EPCI", "Communal"))
}



#' @title Extract INSEE codes and display names for a given territorial typology
#'
#' @description Given a selected territorial typology and its INSEE code and label columns,
#' this function extracts a distinct list of INSEE codes and a concatenated
#' vector of codes and names formatted for UI selection.
#'
#'
#' @param typo Character string. The column name in `data_cat_terr` representing the INSEE code of the territory (e.g., "REG").
#' @param libtypo Character string. The column name representing the name/label of the territory (e.g., "LIBREG").
#' @param data_cat_terr Data frame containing the territorial data, including INSEE codes and names.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{cog}{Character vector of distinct INSEE codes for the specified typology.}
#'   \item{names}{Character vector of concatenated strings "code - name" for display in UI selectors.}
#' }
#'
#' @importFrom rlang sym
#' @import dplyr
#'
#' @noRd
extract_cog_and_names <- function(typo, libtypo, data_cat_terr){

  # Special case : discard communes that do not belong to an EPCI
  if (typo == "EPCI"){
    data_cat_terr <- data_cat_terr |> filter(EPCI != 0) |> arrange(LIBEPCI)
  } else if (typo == "REG"){
    data_cat_terr <- data_cat_terr |> arrange(LIBREG)
  } else if (typo == "COM"){
    data_cat_terr <- data_cat_terr |> arrange(LIBCOM)
  }

  list_territories <- data_cat_terr |> select(all_of(c(typo, libtypo))) |> distinct()
  cog <- list_territories |> pull(typo)
  names <- list_territories |> mutate(NAME = paste0(.data[[libtypo]], " (", .data[[typo]], ")")) |> pull(NAME)

  list(cog = cog, names = names)
}



#' @title Get named vector of INSEE codes and full territory names for selectInput choices
#'
#' @description Depending on the selected territorial typology, this function returns a named character vector
#' where names are full territory labels and values are the corresponding INSEE codes.
#' This structure is suitable for the `choices` argument of `selectInput` in Shiny.
#'
#' @param typology Character string indicating the territory typology. One of:
#'   "france", "region", "departement", "epci", or "commune".
#' @param data_cat_terr Data frame containing the territorial data, including INSEE codes and names.
#'
#' @return Named character vector with INSEE codes as values and full territory names as names.
#'
#' @noRd
extract_select_list_territories <- function(typology, data_cat_terr) {

  if (typology == "france") {
    cog <- "france"
    names <- "France entière"
  } else {
    col_map <- list(
      region = c("REG", "LIBREG"),
      departement = c("DEP", "LIBDEP"),
      epci = c("EPCI", "LIBEPCI"),
      commune = c("COM", "LIBCOM")
    )

    cols <- col_map[[typology]]

    tmp <- extract_cog_and_names(cols[1], cols[2], data_cat_terr)
    cog <- tmp$cog
    names <- tmp$names
  }

  setNames(cog, names)
}



#' @title Generate a conditionalPanel for a given typology, to allow territory selection
#' @description Returns the UI conditionalPanel for the selected typology input.
#'
#' @param ns The namespace function for the module.
#' @param typology A character string specifying the typology. Should be one of "france", "region", "departement", "epci", or "commune".
#'
#' @return A \code{conditionalPanel} containing the appropriate input for the typology.
#' @import shiny
#'
#' @noRd
generate_territory_select_input <- function(ns, typology) {
  if (!typology %in% c("france", "region", "departement", "epci", "commune")) {
    stop("Invalid typology provided.")
  }

  condition <- sprintf("input['%s'] == '%s'", ns("select_typology"), typology)

  if (typology == "france") {
    return(
      conditionalPanel(
        condition = condition,
        selectInput(ns("input_france"), "Sélection du territoire", choices = "France entière")
      )
    )
  } else {
    return(
      conditionalPanel(
        condition = condition,
        selectizeInput(
          ns(paste0("input_", typology)),
          "Sélection du territoire",
          choices = NULL
        )
      )
    )
  }
}
