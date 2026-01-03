# Data Selection Module for HMD Explorer
# Provides UI and server for selecting country, gender, age, and year ranges
# Implements dynamic cascading dropdowns (type -> place -> sex pattern)

#' Data Selection UI
#' @param id Module namespace ID
#' @param allow_multiple Whether to allow multiple country selection
#' @param select_sex Whether to show gender selection
select_data_ui <- function(id, allow_multiple = FALSE, select_sex = TRUE) {

  ns <- NS(id)

  tagList(
    # Population type selection (cascading dropdown level 1)
    selectInput(
      ns("type_select"),
      "Select population type",
      choices = c(
        "Countries" = "country",
        "Regions/Subdivisions" = "region",
        "All" = "all"
      ),
      selected = "country"
    ),

    # Country/place selection (cascading dropdown level 2 - dynamic)
    uiOutput(ns("dynamic_place_select")),

    # Gender selection (cascading dropdown level 3 - dynamic)
    if (select_sex) uiOutput(ns("dynamic_sex_select")) else NULL,

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
#' @param allow_multiple Whether to allow multiple country selection
#' @return Reactive containing the selected and filtered data
select_data_server <- function(id, mode = "standard", allow_multiple = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: Classify codes as country or region
    # Regions typically have underscores (e.g., GBR_SCO, GBR_ENG)
    classify_code <- function(code) {
      if (grepl("_", code)) "region" else "country"
    }

    # Get available places based on type selection
    get_places_by_type <- function(type) {
      if (type == "all") {
        return(codes_named)
      }

      # Filter codes_named based on type
      code_types <- sapply(codes_named, classify_code)

      if (type == "country") {
        codes_named[code_types == "country"]
      } else {
        codes_named[code_types == "region"]
      }
    }

    # Get available sexes for selected place(s)
    get_available_sexes <- function(codes) {
      if (is.null(codes) || length(codes) == 0) {
        return(c("Total", "Male", "Female"))
      }

      full_data %>%
        filter(code %in% codes) %>%
        pull(gender) %>%
        unique() %>%
        sort()
    }

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

    # Dynamic place selection (cascading from type)
    output$dynamic_place_select <- renderUI({
      type <- input$type_select %||% "country"
      places <- get_places_by_type(type)

      # Default selection based on type
      default_sel <- if (type == "region" && "GBR_SCO" %in% places) {
        "GBR_SCO"
      } else if (length(places) > 0) {
        places[1]
      } else {
        NULL
      }

      selectInput(
        ns("code_select"),
        "Select population",
        choices = places,
        selected = default_sel,
        multiple = allow_multiple
      )
    })

    # Dynamic sex selection (cascading from place)
    output$dynamic_sex_select <- renderUI({
      codes <- input$code_select
      available_sexes <- get_available_sexes(codes)

      # Default to Total if available, otherwise first available
      default_sex <- if ("Total" %in% available_sexes) {
        "Total"
      } else if (length(available_sexes) > 0) {
        available_sexes[1]
      } else {
        NULL
      }

      selectInput(
        ns("sex_select"),
        "Select sex",
        choices = available_sexes,
        selected = default_sex
      )
    })

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
