# functions to create rvm color palettes. Method based on :
# https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/

#' rvm_colour
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
rvm_colour <- function(...) {

  rvm_colours <- c(
    'delft_blue' = '#303876',
    'delft_blue_50' = '#979BBA',
    'ivory' = '#F6F9ED',
    'pear' = '#C6CE41',
    'pear_50' = '#E2E7A0',
    'lavender_pink_200' = '#C03C85',
    'lavender_pink' = '#ECB2D2',
    'saffron' = '#F3C460',
    'cinnabar' = '#E94F35',
    'cinnabar_50' = '#F4A799',
    'raisin_black' = '#294754',
    'raisin_black_50' = '#94A3A9',
    'light_blue' = '#B1D6E4',
    'light_blue_50' = '#EBF5F8',
    'amethyst' = '#9185BE',
    'amethyst_50' = '#C8C2DF'
  )

  cols <- c(...)

  if (is.null(cols))
    return (rvm_colours)

  rvm_colours[cols]
}


#' rvm_palette
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
rvm_palette <- function(palette = "main", ...) {

  rvm_palettes <- list(
    'main' = rvm_colour(
      "saffron",
      'lavender_pink_200',
      'delft_blue_50',
      "light_blue",
      "amethyst",
      "cinnabar",
      "lavender_pink",
      "delft_blue",
      "raisin_black"
      ),

    'highlight' = rvm_colour("raisin_black", "pear")
  )

  rvm_palettes[[palette]]

}


#' palette_gen
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
palette_gen <- function(palette = "main", direction = 1) {

  function(n) {

    if (n > length(rvm_palette(palette)))
      warning("Not enough colours in this palette!")

    else {

      all_colours <- rvm_palette(palette)

      all_colours <- unname(unlist(all_colours))

      all_colours <- if (direction >= 0) all_colours else rev(all_colours)

      colour_list <- all_colours[1:n]

    }
  }
}

#' palette_gen_c
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom grDevices colorRamp
palette_gen_c <- function(palette = "main", direction = 1, ...) {

  pal <- rvm_palette(palette)

  pal <- if (direction >= 0) pal else rev(pal)

  colorRampPalette(pal, ...)

}

#' scale_fill_rvm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom ggplot2 discrete_scale
scale_fill_rvm <- function(palette = "main", direction = 1, ...) {

  discrete_scale(
    "fill", "rvm",
    palette_gen(palette, direction),
    ...
  )
}

#' scale_colour_rvm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom ggplot2 discrete_scale
scale_colour_rvm <- function(palette = "main", direction = 1, ...) {

  discrete_scale(
    "colour", "rvm",
    palette_gen(palette, direction),
    ...
  )
}



