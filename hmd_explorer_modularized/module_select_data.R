# Data Selection Module for HMD Explorer
# Provides UI and server for selecting country, gender, age, and year ranges

#' Data Selection UI
#' @param id Module namespace ID
#' @param allow_multiple Whether to allow multiple country selection
#' @param select_sex Whether to show gender selection
select_data_ui <- function(id, allow_multiple = FALSE, select_sex = TRUE) {
  ns <- NS(id)

  sex_select_element <- if (select_sex) {
    selectInput(
      ns("sex_select"),
      "Select sex of interest",
      choices = c("Total", "Male", "Female"),
      selected = "Total"
    )
  } else {
    NULL
  }

  tagList(
    # Country selection
    selectInput(
      ns("code_select"),
      "Select country of interest",
      choices = codes_named,
      selected = "GBR_SCO",
      multiple = allow_multiple
    ),

    # Gender selection (conditional)
    sex_select_element,

    # Year range options
    checkboxInput(
      ns("snap_period"),
      "Snap years to available range",
      value = FALSE
    ),

    uiOutput(ns("dynamic_year_slider")),

    # Age range
    sliderInput(
      ns("age_limits"),
      "Select range of ages",
      min = 0, max = 110, step = 1,
      value = c(0, 90),
      sep = ""
    ),

    # Load button
    actionButton(
      ns("confirm_selection"),
      "Load Data",
      class = "btn-primary"
    ),

    # Debug outputs (can be hidden in production)
    tags$hr(),
    tags$small(
      textOutput(ns("selection_summary"))
    )
  )
}


#' Data Selection Server
#' @param id Module namespace ID
#' @param mode Selection mode: "standard", "sex-compare", "population"
#' @return Reactive containing the selected and filtered data
select_data_server <- function(id, mode = "standard") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get min/max year for selected countries
    get_year_range <- function(codes) {
      if (is.null(codes) || length(codes) == 0) {
        return(c(1750, 2020))
      }

      full_data %>%
        filter(code %in% codes) %>%
        summarise(
          min_year = min(year, na.rm = TRUE),
          max_year = max(year, na.rm = TRUE)
        ) %>%
        {c(.$min_year, .$max_year)}
    }

    # Dynamic year slider based on snap setting
    output$dynamic_year_slider <- renderUI({
      snap <- input$snap_period
      codes <- input$code_select

      if (isTRUE(snap) && !is.null(codes)) {
        year_range <- get_year_range(codes)
        sliderInput(
          ns("period_limits"),
          "Select range of years",
          min = year_range[1], max = year_range[2],
          step = 1,
          value = year_range,
          sep = ""
        )
      } else {
        sliderInput(
          ns("period_limits"),
          "Select range of years",
          min = 1750, max = 2020,
          step = 1,
          value = c(1900, 2020),
          sep = ""
        )
      }
    })

    # Reactive values triggered by button click
    selected_codes <- eventReactive(input$confirm_selection, {
      input$code_select
    }, ignoreNULL = FALSE)

    selected_sex <- eventReactive(input$confirm_selection, {
      input$sex_select %||% "Total"
    }, ignoreNULL = FALSE)

    selected_ages <- eventReactive(input$confirm_selection, {
      input$age_limits %||% c(0, 110)
    }, ignoreNULL = FALSE)

    selected_years <- eventReactive(input$confirm_selection, {
      input$period_limits %||% c(1900, 2020)
    }, ignoreNULL = FALSE)

    # Load and filter data
    filtered_data <- eventReactive(input$confirm_selection, {
      req(selected_codes())

      codes <- selected_codes()
      sex <- selected_sex()
      ages <- selected_ages()
      years <- selected_years()

      # Start with full data filtered by codes
      data <- full_data %>%
        filter(code %in% codes) %>%
        filter(age >= ages[1], age <= ages[2]) %>%
        filter(year >= years[1], year <= years[2])

      # Handle different modes
      if (mode == "sex-compare") {
        # Return both Male and Female for comparison
        data <- data %>%
          filter(gender %in% c("Male", "Female"))
      } else if (mode == "population") {
        # For population mode, return data for selected sex
        data <- data %>%
          filter(gender == sex)
      } else {
        # Standard mode - filter by selected sex
        data <- data %>%
          filter(gender == sex)
      }

      return(data)
    }, ignoreNULL = FALSE)

    # Selection summary
    output$selection_summary <- renderText({
      req(input$confirm_selection)

      codes <- selected_codes()
      sex <- selected_sex()
      ages <- selected_ages()
      years <- selected_years()

      paste0(
        "Selected: ",
        paste(codes, collapse = ", "), " | ",
        sex, " | ",
        "Ages ", ages[1], "-", ages[2], " | ",
        "Years ", years[1], "-", years[2]
      )
    })

    # Return the reactive data
    return(filtered_data)
  })
}
