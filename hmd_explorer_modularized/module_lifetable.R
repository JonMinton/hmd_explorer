# Lifetable Module for HMD Explorer
# Generates life table visualizations (survival probability surfaces)

#' Calculate q_x (probability of dying between age x and x+1)
#' Uses HMD methodology with appropriate a_x values
#' @param age Age in years
#' @param sex Sex ("Male" or "Female")
#' @param deaths Number of deaths
#' @param exposure Person-years of exposure
#' @return q_x value
calc_qx <- function(age, sex, deaths, exposure) {
  m_x <- deaths / exposure

  # Calculate a_x (average fraction of year lived by those dying)
  a_x <- if (age == 0) {
    if (m_x > 0.107) {
      if (sex == "Female") 0.350 else 0.330
    } else {
      if (sex == "Female") {
        0.053 + 2.800 * m_x
      } else {
        0.045 + 2.684 * m_x
      }
    }
  } else {
    0.5
  }

  # Calculate q_x using the relationship with m_x

q_x <- m_x / (1 + (1 - a_x) * m_x)

  return(q_x)
}


#' Lifetable UI Module
#' @param id Module namespace ID
make_lifetable_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Display options
    selectInput(
      ns("select_measure"),
      label = "Select life table measure",
      choices = c(
        "Survival probability (p_x)" = "p_x",
        "Death probability (q_x)" = "q_x",
        "Log death probability" = "log_q_x"
      ),
      selected = "p_x"
    ),

    # Color scheme selection
    selectInput(
      ns("select_colourscheme"),
      label = "Select colour scheme",
      choices = c(
        "Viridis:viridis",
        "Viridis:magma",
        "Viridis:plasma",
        "Viridis:inferno",
        "Viridis:cividis"
      ),
      selected = "Viridis:plasma"
    ),

    # Reverse palette
    checkboxInput(
      ns("reverse_palette"),
      label = "Reverse palette",
      value = FALSE
    ),

    # Plot button
    actionButton(
      ns("generate_plot"),
      label = "Generate Life Table",
      class = "btn-primary"
    ),

    tags$hr(),

    # Main plot output
    plotOutput(ns("lifetable_surface"), height = "500px"),

    # Info text
    tags$small(
      textOutput(ns("plot_info"))
    )
  )
}


#' Lifetable Server Module
#' @param id Module namespace ID
#' @param data Reactive data from select_data_server
make_lifetable_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate life table measures
    calc_lifetable <- function(data_df) {
      data_df %>%
        mutate(
          q_x = pmap_dbl(
            .l = list(
              age = age,
              sex = gender,
              deaths = num_deaths,
              exposure = exposure
            ),
            .f = possibly(calc_qx, otherwise = NA_real_)
          ),
          p_x = 1 - q_x,
          log_q_x = log(q_x, 10)
        )
    }

    # Processed life table data
    lifetable_data <- eventReactive(input$generate_plot, {
      req(data())
      calc_lifetable(data())
    })

    # Selected measure
    selected_measure <- eventReactive(input$generate_plot, {
      input$select_measure
    })

    # Selected color scheme
    selected_colscheme <- eventReactive(input$generate_plot, {
      input$select_colourscheme
    })

    # Selected palette direction
    selected_reverse <- eventReactive(input$generate_plot, {
      input$reverse_palette
    })

    # Render life table surface
    output$lifetable_surface <- renderPlot({
      req(lifetable_data())

      lt_data <- lifetable_data()
      measure <- selected_measure()
      colscheme <- selected_colscheme()
      reverse <- selected_reverse()

      # Parse color scheme
      parts <- str_split(colscheme, ":", simplify = TRUE)
      pal <- parts[2]

      # Create title based on measure
      measure_label <- switch(
        measure,
        "p_x" = "Survival probability (p_x)",
        "q_x" = "Death probability (q_x)",
        "log_q_x" = "Log death probability"
      )

      # Base plot
      p <- lt_data %>%
        ggplot(aes(x = year, y = age, fill = .data[[measure]])) +
        geom_tile() +
        labs(
          x = "Year",
          y = "Age",
          fill = measure_label,
          title = paste0("Life table: ", measure_label)
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          legend.position = "right"
        )

      # Apply color scale
      direction <- if (reverse) -1 else 1

      if (measure == "p_x") {
        # Survival probability: 0 to 1
        p <- p + scale_fill_viridis_c(
          option = pal,
          direction = direction,
          limits = c(0, 1)
        )
      } else if (measure == "q_x") {
        # Death probability: 0 to 1
        p <- p + scale_fill_viridis_c(
          option = pal,
          direction = direction,
          limits = c(0, 1)
        )
      } else {
        # Log death probability: variable range
        p <- p + scale_fill_viridis_c(
          option = pal,
          direction = direction
        )
      }

      return(p)
    })

    # Plot info
    output$plot_info <- renderText({
      req(lifetable_data())
      data <- lifetable_data()

      # Calculate some summary stats
      years <- range(data$year, na.rm = TRUE)
      ages <- range(data$age, na.rm = TRUE)
      gender <- unique(data$gender)[1]

      paste0(
        "Data: ", nrow(data), " observations | ",
        "Years: ", years[1], "-", years[2], " | ",
        "Ages: ", ages[1], "-", ages[2], " | ",
        "Gender: ", gender
      )
    })

    return(NULL)
  })
}
