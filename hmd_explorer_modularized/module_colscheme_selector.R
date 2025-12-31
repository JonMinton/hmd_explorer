# Color Scheme Selector Module
# Provides UI and server for selecting color palettes (Viridis + RColorBrewer)

# Helper function to get RColorBrewer palette info
define_colbrew_pals <- function(cat = NULL, cb = NULL) {
  out <- brewer.pal.info %>%
    as_tibble(rownames = "pal_name")

  if (!is.null(cat)) {
    out <- out %>% filter(category == cat)
  }
  if (!is.null(cb)) {
    out <- out %>% filter(colorblind == cb)
  }

  out
}

# Get all RColorBrewer palette names
all_colbrew_pals <- brewer.pal.info %>%
  as_tibble(rownames = "pal_name") %>%
  pull(pal_name) %>%
  str_c("RColorBrewer:", .)

# Create a color ramp function from a palette name
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


#' Color Scheme Selector UI
#' @param id Module namespace ID
#' @param include_reverse Whether to include a reverse palette checkbox
colscheme_selector_ui <- function(id, include_reverse = TRUE) {
  ns <- NS(id)

  choices <- c(
    "Viridis:viridis",
    "Viridis:magma",
    "Viridis:plasma",
    "Viridis:inferno",
    "Viridis:cividis",
    all_colbrew_pals
  )

  tagList(
    selectInput(
      ns("select_colourscheme"),
      label = "Select colour scheme",
      choices = choices,
      selected = "Viridis:viridis"
    ),
    if (include_reverse) {
      checkboxInput(
        ns("reverse_palette"),
        label = "Reverse palette",
        value = FALSE
      )
    }
  )
}


#' Color Scheme Selector Server
#' @param id Module namespace ID
#' @return Reactive list with palette name, family, and reverse flag
colscheme_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Parse the selected color scheme
    palette_info <- reactive({
      selected <- input$select_colourscheme
      if (is.null(selected)) {
        return(list(family = "Viridis", name = "viridis", reverse = FALSE))
      }

      parts <- str_split(selected, ":", simplify = TRUE)
      list(
        family = parts[1],
        name = parts[2],
        reverse = isTRUE(input$reverse_palette)
      )
    })

    # Return the reactive palette info
    return(palette_info)
  })
}
