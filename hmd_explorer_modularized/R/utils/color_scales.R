# Utility: Color scale helpers
# Provides consistent color scale generation across visualizations

#' Parse color scheme string into family and palette
#' @param colscheme String in format "Family:palette" (e.g., "Viridis:viridis")
#' @return Named list with family and pal elements
parse_colorscheme <- function(colscheme) {
  parts <- str_split(colscheme, ":", simplify = TRUE)
  list(family = parts[1], pal = parts[2])
}


#' Create plotly colorscale from scheme string
#' @param colscheme String in format "Family:palette"
#' @param n Number of colors (default 20)
#' @param reverse Whether to reverse the palette
#' @return List suitable for plotly colorscale parameter
make_plotly_colorscale <- function(colscheme, n = 20, reverse = FALSE) {
  parsed <- parse_colorscheme(colscheme)
  hue_posn <- seq(0, 1, length.out = n)

  if (parsed$family == "Viridis") {
    direction <- if (reverse) -1 else 1
    cols <- viridis_pal(option = parsed$pal, direction = direction)(n)
  } else if (parsed$family == "RColorBrewer") {
    cols <- create_colbrew_ramp(parsed$pal)(n)
    if (reverse) cols <- rev(cols)
  } else {
    # Default to viridis
    cols <- viridis_pal()(n)
  }

  list(hue_posn, cols)
}


#' Create RColorBrewer ramp function
#' @param palname Palette name from RColorBrewer
#' @return Color ramp function
create_colbrew_ramp <- function(palname) {
  pal_info <- brewer.pal.info %>%
    as_tibble(rownames = "pal_name") %>%
    filter(pal_name == palname)

  if (nrow(pal_info) == 0) {
    stop("Unknown palette: ", palname)
  }

  max_n <- pal_info %>% pull(maxcolors)
  colorRampPalette(brewer.pal(name = palname, n = max_n))
}


#' Get all available color palettes
#' @return Character vector of palette names in "Family:palette" format
get_all_palettes <- function() {
  viridis_pals <- c(
    "Viridis:viridis",
    "Viridis:magma",
    "Viridis:plasma",
    "Viridis:inferno",
    "Viridis:cividis"
  )

  brew_pals <- brewer.pal.info %>%
    as_tibble(rownames = "pal_name") %>%
    pull(pal_name) %>%
    paste0("RColorBrewer:", .)

  c(viridis_pals, brew_pals)
}
