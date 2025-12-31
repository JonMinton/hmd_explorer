# HMD Explorer - Modularized Version
# Main application file

library(shiny)
source("global.R")

ui <- fluidPage(

  titlePanel("Human Mortality Database Explorer"),

  sidebarLayout(
    sidebarPanel(
      # Dynamic sidebar based on active tab
      uiOutput("sidebar_controls")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        id = "main_tabset",

        # Tab 1: Individual Population Mortality
        tabPanel(
          "Mortality",
          value = "mortality",
          make_graphics_ui("graphics_mortality")
        ),

        # Tab 2: Population
        tabPanel(
          "Population",
          value = "population",
          make_graphics_ui("graphics_population")
        ),

        # Tab 3: Mortality Sex Ratios
        tabPanel(
          "Mortality Sex Ratios",
          value = "mort_sex_ratio",
          make_graphics_ui("graphics_mort_ratio")
        ),

        # Tab 4: Population Sex Ratios
        tabPanel(
          "Population Sex Ratios",
          value = "pop_sex_ratio",
          make_graphics_ui("graphics_pop_ratio")
        ),

        # Tab 5: Group Comparisons
        tabPanel(
          "Mortality Group Comparisons",
          value = "group_compare",
          make_graphics_ui("graphics_group_compare")
        ),

        # Tab 6: Lifetable
        tabPanel(
          "Lifetable",
          value = "lifetable",
          make_lifetable_ui("lifetable_module")
        ),

        # Tab 7: Correlation in trends
        tabPanel(
          "Correlation in Trends",
          value = "correlation",
          make_corrmaps_ui("corrmaps_module")
        )
      )
    )
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
  data_group_b <- select_data_server("data_group_b")
  data_group_a <- select_data_server("data_group_a")
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
