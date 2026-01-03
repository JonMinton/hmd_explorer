# HMD Explorer - Modularized Version
# Main application file with Bootstrap styling via bslib

library(shiny)
source("global.R")

# Define Bootstrap theme
hmd_theme <- bs_theme(

  version = 5,
  bootswatch = "flatly",
  primary = "#2c3e50",
  secondary = "#95a5a6",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Roboto Slab")
)

ui <- page_navbar(
  # Include custom CSS and JS for resizable panels
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/resizable-panels.css"),
    tags$script(src = "js/resizable-panels.js")
  ),
  title = "Human Mortality Database Explorer",
  theme = hmd_theme,
  id = "main_tabset",
  fillable = TRUE,

  # Sidebar layout within each nav panel
  sidebar = sidebar(
    id = "main_sidebar",
    width = 300,
    uiOutput("sidebar_controls")
  ),

  # Tab 1: Individual Population Mortality
  nav_panel(
    title = "Mortality",
    value = "mortality",
    make_graphics_ui("graphics_mortality")
  ),

  # Tab 2: Population
  nav_panel(
    title = "Population",
    value = "population",
    make_graphics_ui("graphics_population")
  ),

  # Tab 3: Mortality Sex Ratios
  nav_panel(
    title = "Mortality Sex Ratios",
    value = "mort_sex_ratio",
    make_graphics_ui("graphics_mort_ratio")
  ),

  # Tab 4: Population Sex Ratios
  nav_panel(
    title = "Population Sex Ratios",
    value = "pop_sex_ratio",
    make_graphics_ui("graphics_pop_ratio")
  ),

  # Tab 5: Group Comparisons
  nav_panel(
    title = "Group Comparisons",
    value = "group_compare",
    make_graphics_ui("graphics_group_compare")
  ),

  # Tab 6: Lifetable
  nav_panel(
    title = "Lifetable",
    value = "lifetable",
    make_lifetable_ui("lifetable_module")
  ),

  # Tab 7: Correlation in trends
  nav_panel(
    title = "Correlation",
    value = "correlation",
    make_corrmaps_ui("corrmaps_module")
  )
)

server <- function(input, output, session) {

  # Get active tab reactively
  active_tab <- reactive({
    input$main_tabset
  })

  # Dynamic sidebar UI based on active tab
  output$sidebar_controls <- renderUI({
    tab <- active_tab()

    switch(
      tab,
      "mortality" = select_data_ui("data_mortality"),
      "population" = select_data_ui("data_population"),
      "mort_sex_ratio" = select_data_ui("data_mort_ratio", select_sex = FALSE),
      "pop_sex_ratio" = select_data_ui("data_pop_ratio", select_sex = FALSE),
      "group_compare" = tagList(
        h4("Population Group B"),
        select_data_ui("data_group_b", allow_multiple = TRUE),
        h4("Comparator Group A"),
        select_data_ui("data_group_a", allow_multiple = TRUE)
      ),
      "lifetable" = select_data_ui("data_lifetable", select_sex = TRUE),
      "correlation" = select_data_ui("data_correlation"),
      # Default
      select_data_ui("data_default")
    )
  })

  # Data modules - use moduleServer pattern
  data_mortality <- select_data_server("data_mortality")
  data_population <- select_data_server("data_population")
  data_mort_ratio <- select_data_server("data_mort_ratio", mode = "sex-compare")
  data_pop_ratio <- select_data_server("data_pop_ratio", mode = "sex-compare")
  data_group_b <- select_data_server("data_group_b", allow_multiple = TRUE)
  data_group_a <- select_data_server("data_group_a", allow_multiple = TRUE)
  data_lifetable <- select_data_server("data_lifetable")
  data_correlation <- select_data_server("data_correlation")

  # Graphics modules
  make_graphics_server("graphics_mortality",
                       mode = "singular",
                       data = data_mortality)

  make_graphics_server("graphics_population",
                       mode = "population",
                       data = data_population)

  make_graphics_server("graphics_mort_ratio",
                       mode = "sex-compare",
                       data = data_mort_ratio)

  make_graphics_server("graphics_pop_ratio",
                       mode = "pop-sex-compare",
                       data = data_pop_ratio)

  make_graphics_server("graphics_group_compare",
                       mode = "group-compare",
                       data = data_group_b,
                       data_comparator = data_group_a)

  # Lifetable module
  make_lifetable_server("lifetable_module",
                        data = data_lifetable)

  # Correlation module
  make_corrmaps_server("corrmaps_module",
                       data = data_correlation)
}

shinyApp(ui, server)
