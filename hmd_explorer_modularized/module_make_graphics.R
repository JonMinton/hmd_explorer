# Graphics Module for HMD Explorer
# Generates Lexis surfaces and 3D plots for mortality/population data
# Supports click-to-drill-down subplots via plotly crosstalk
# Supports multiple layout arrangements (main-only, main+col, main+row)

#' Graphics UI Module
#' @param id Module namespace ID
#' @param show_subplot Whether to show subplot output area
make_graphics_ui <- function(id, show_subplot = TRUE) {
  ns <- NS(id)

  tagList(
    # Outcome selection
    selectInput(
      ns("select_outcome"),
      label = "Select outcome to display",
      choices = c("Population", "Mortality", "Log mortality"),
      selected = "Log mortality"
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
        "Viridis:cividis",
        all_colbrew_pals
      ),
      selected = "Viridis:viridis"
    ),

    # Palette options
    checkboxInput(
      ns("reverse_pal_direction"),
      value = FALSE,
      label = "Reverse palette direction"
    ),

    # Continuity correction
    checkboxInput(
      ns("add_continuity"),
      value = FALSE,
      label = "Add continuity correction"
    ),
    uiOutput(ns("dynamic_continuity")),

    # Visualization type
    selectInput(
      ns("select_devicevis"),
      label = "Select visualization type",
      choices = c(
        `ggplot2: Lexis surface` = "ggplot2:lexis",
        `Plotly: Lexis heatmap` = "plotly:lexis",
        `Plotly: 3D surface` = "plotly:3d_surface"
      ),
      selected = "plotly:3d_surface"
    ),

    # Dynamic controls that appear based on mode (rendered server-side)
    uiOutput(ns("mode_specific_controls")),

    # Layout arrangement selector (like Dash app) with icon images
    if (show_subplot) {
      radioButtons(
        ns("layout_arrangement"),
        label = "Layout arrangement",
        choiceNames = list(
          tags$span(
            tags$img(src = "images/main-only.svg", height = "24px", style = "vertical-align: middle;"),
            " Surface only"
          ),
          tags$span(
            tags$img(src = "images/main-3colRight.svg", height = "24px", style = "vertical-align: middle;"),
            " + subplots (column)"
          ),
          tags$span(
            tags$img(src = "images/main-3rowBottom.svg", height = "24px", style = "vertical-align: middle;"),
            " + subplots (row)"
          )
        ),
        choiceValues = c("main-only", "main-3col", "main-3row"),
        selected = "main-3row",
        inline = TRUE
      )
    } else {
      NULL
    },

    # Plot button
    actionButton(
      ns("plot_graphs"),
      label = "Generate Plot",
      class = "btn-primary"
    ),

    tags$hr(),

    # Normal (non-maximized) layout container
    uiOutput(ns("normal_layout")),

    # Fullscreen overlay (appears when maximized)
    uiOutput(ns("fullscreen_overlay"))
  )
}


#' Graphics Server Module
#' @param id Module namespace ID
#' @param mode Visualization mode: "singular", "population", "sex-compare",
#'             "pop-sex-compare", "group-compare"
#' @param data Reactive data from select_data_server
#' @param data_comparator Optional second dataset for group comparisons
#' @param country_name Optional reactive for country name display in subplots
make_graphics_server <- function(id, mode = "singular", data, data_comparator = NULL, country_name = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create unique source ID for crosstalk linking
    surface_source <- paste0(id, "_surface")

    # Dynamic continuity correction input
    output$dynamic_continuity <- renderUI({
      if (isTRUE(input$add_continuity)) {
        numericInput(
          ns("continuity_value"),
          label = "Continuity correction value",
          value = 0.5,
          min = 0, max = 500
        )
      }
    })

    # Mode-specific controls (for sex-compare and group-compare modes)
    output$mode_specific_controls <- renderUI({
      # Only show for comparison modes
      if (mode %in% c("sex-compare", "group-compare")) {
        tagList(
          tags$hr(),
          # Display mode selector
          selectInput(
            ns("compare_display_mode"),
            label = if (mode == "sex-compare") "Compare display mode" else "Group display mode",
            choices = c(
              "Show ratio/difference" = "ratio",
              "Show both surfaces" = "both"
            ),
            selected = "ratio"
          ),

          # Alpha sliders (only show when 'both' mode is selected)
          conditionalPanel(
            condition = sprintf("input['%s'] == 'both'", ns("compare_display_mode")),
            ns = ns,
            sliderInput(
              ns("alpha_surface_1"),
              label = if (mode == "sex-compare") "Male surface transparency" else "Population B transparency",
              min = 0, max = 1, step = 0.1, value = 0.7
            ),
            sliderInput(
              ns("alpha_surface_2"),
              label = if (mode == "sex-compare") "Female surface transparency" else "Population A transparency",
              min = 0, max = 1, step = 0.1, value = 0.7
            )
          )
        )
      } else {
        NULL
      }
    })

    # Selected values (triggered by plot button)
    # Use ignoreNULL = FALSE with default values to prevent errors before button click
    selected_outcome <- eventReactive(input$plot_graphs, {
      input$select_outcome %||% "Log mortality"
    }, ignoreNULL = FALSE)

    selected_colourscheme <- eventReactive(input$plot_graphs, {
      input$select_colourscheme %||% "Viridis:viridis"
    }, ignoreNULL = FALSE)

    selected_devicevis <- eventReactive(input$plot_graphs, {
      input$select_devicevis %||% "plotly:3d_surface"
    }, ignoreNULL = FALSE)

    selected_add_continuity <- eventReactive(input$plot_graphs, {
      input$add_continuity %||% FALSE
    }, ignoreNULL = FALSE)

    selected_continuityvalue <- eventReactive(input$plot_graphs, {
      if (isTRUE(input$add_continuity)) {
        input$continuity_value %||% 0.5
      } else {
        0.5  # Default small correction to avoid log(0)
      }
    }, ignoreNULL = FALSE)

    selected_pal_reverse <- eventReactive(input$plot_graphs, {
      input$reverse_pal_direction %||% FALSE
    }, ignoreNULL = FALSE)

    # Compare display mode (for sex-compare and group-compare)
    selected_compare_mode <- eventReactive(input$plot_graphs, {
      input$compare_display_mode %||% "ratio"
    }, ignoreNULL = FALSE)

    # Alpha values for dual surfaces
    selected_alpha_1 <- eventReactive(input$plot_graphs, {
      input$alpha_surface_1 %||% 0.7
    }, ignoreNULL = FALSE)

    selected_alpha_2 <- eventReactive(input$plot_graphs, {
      input$alpha_surface_2 %||% 0.7
    }, ignoreNULL = FALSE)

    # Track whether plot has been generated
    plot_generated <- reactiveVal(FALSE)

    # Track maximized state - defaults to TRUE (maximized on first load)
    is_maximized <- reactiveVal(FALSE)

    # Store selection info for title bar
    selection_info <- reactiveVal("")

    # Update states when button is clicked
    observeEvent(input$plot_graphs, {
      plot_generated(TRUE)
      is_maximized(TRUE)  # Auto-maximize on generate
    })

    # Toggle maximize state
    observeEvent(input$toggle_maximize, {
      is_maximized(!is_maximized())
    })

    # Exit fullscreen
    observeEvent(input$exit_fullscreen, {
      is_maximized(FALSE)
    })

    # Derive outcome data based on mode
    derive_outcome <- function(data_reactive, outcome, correction) {
      req(data_reactive())

      correction <- correction %||% 0.5

      if (mode == "sex-compare") {
        # Sex comparison: calculate ratio between male and female mortality
        processed <- data_reactive() %>%
          filter(gender %in% c("Male", "Female")) %>%
          group_by(year, age, gender) %>%
          summarise(
            num_deaths = sum(num_deaths, na.rm = TRUE),
            num_population = sum(num_population, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          pivot_wider(
            names_from = gender,
            values_from = c(num_deaths, num_population, exposure)
          ) %>%
          mutate(
            mr_male = (num_deaths_Male + correction) / (exposure_Male + correction),
            mr_female = (num_deaths_Female + correction) / (exposure_Female + correction),
            lmr_male = log(mr_male, 10),
            lmr_female = log(mr_female, 10),
            ratio = mr_male / mr_female,
            excess = (mr_male - mr_female) / mr_female
          )

        if (outcome == "Log mortality") {
          processed <- processed %>%
            mutate(z_var = log(ratio, 10)) %>%
            select(year, age, z_var, ratio, excess, lmr_male, lmr_female)
        } else {
          processed <- processed %>%
            mutate(z_var = ratio) %>%
            select(year, age, z_var, ratio, excess, lmr_male, lmr_female)
        }

      } else if (mode == "pop-sex-compare") {
        # Population sex ratio
        processed <- data_reactive() %>%
          filter(gender %in% c("Male", "Female")) %>%
          group_by(year, age, gender) %>%
          summarise(
            num_population = sum(num_population, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          pivot_wider(
            names_from = gender,
            values_from = num_population
          ) %>%
          mutate(
            ratio = (Male + correction) / (Female + correction),
            excess = (Male - Female) / Female
          ) %>%
          mutate(z_var = ratio) %>%
          select(year, age, z_var, ratio, excess, Male, Female)

      } else if (mode == "group-compare" && !is.null(data_comparator)) {
        # Group comparison: difference between two population groups
        data_b <- data_reactive() %>%
          group_by(year, age) %>%
          summarise(
            deaths_b = sum(num_deaths, na.rm = TRUE),
            exposure_b = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(lmr_b = log((deaths_b + correction) / (exposure_b + correction), 10))

        data_a <- data_comparator() %>%
          group_by(year, age) %>%
          summarise(
            deaths_a = sum(num_deaths, na.rm = TRUE),
            exposure_a = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(lmr_a = log((deaths_a + correction) / (exposure_a + correction), 10))

        processed <- data_b %>%
          inner_join(data_a, by = c("year", "age")) %>%
          mutate(
            z_var = lmr_b - lmr_a,
            lmr_A = lmr_a,
            lmr_B = lmr_b
          ) %>%
          select(year, age, z_var, lmr_A, lmr_B)

      } else if (mode == "population") {
        # Population mode - handle stacked male/female for Total
        processed <- data_reactive() %>%
          group_by(year, age) %>%
          summarise(
            num_population = sum(num_population, na.rm = TRUE),
            num_deaths = sum(num_deaths, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(z_var = num_population) %>%
          select(year, age, z_var, num_population)

      } else {
        # Standard mortality mode - aggregate by year/age
        processed <- data_reactive() %>%
          group_by(year, age) %>%
          summarise(
            num_deaths = sum(num_deaths, na.rm = TRUE),
            num_population = sum(num_population, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          )

        if (outcome == "Log mortality") {
          processed <- processed %>%
            mutate(
              mr = (num_deaths + correction) / (exposure + correction),
              z_var = log(mr, 10)
            ) %>%
            select(year, age, z_var, mr, num_deaths, exposure)
        } else if (outcome == "Mortality") {
          processed <- processed %>%
            mutate(
              mr = (num_deaths + correction) / (exposure + correction),
              z_var = mr
            ) %>%
            select(year, age, z_var, mr, num_deaths, exposure)
        } else if (outcome == "Population") {
          processed <- processed %>%
            mutate(z_var = num_population) %>%
            select(year, age, z_var, num_population)
        }
      }

      return(processed)
    }

    # Store processed data for subplot access
    processed_data <- reactive({
      req(data())
      derive_outcome(data, selected_outcome(), selected_continuityvalue())
    })

    # Create ggplot surface
    produce_surface_ggplot <- function(data, colscheme, devicetype) {
      if (devicetype != "ggplot2:lexis" || is.null(data)) {
        return(NULL)
      }

      # Ensure colscheme has a valid default
      if (is.null(colscheme) || is.na(colscheme) || colscheme == "") {
        colscheme <- "Viridis:viridis"
      }

      parts <- str_split(colscheme, ":", simplify = TRUE)
      family <- parts[1]
      pal <- parts[2]

      # Validate parsed values
      if (is.na(family) || is.na(pal)) {
        family <- "Viridis"
        pal <- "viridis"
      }

      p <- data %>%
        ggplot(aes(x = year, y = age, fill = z_var)) +
        geom_tile() +
        labs(x = "Year", y = "Age", fill = selected_outcome()) +
        theme_minimal()

      if (family == "Viridis") {
        p <- p + scale_fill_viridis_c(
          option = pal,
          direction = ifelse(selected_pal_reverse(), -1, 1)
        )
      } else if (family == "RColorBrewer") {
        p <- p + scale_fill_distiller(
          palette = pal,
          direction = ifelse(selected_pal_reverse(), -1, 1)
        )
      }

      return(p)
    }

    # Create plotly surface with crosstalk source
    # Supports dual translucent surfaces for sex-compare and group-compare modes
    produce_surface_plotly <- function(data, colscheme, devicetype) {
      if (!devicetype %in% c("plotly:lexis", "plotly:3d_surface") || is.null(data)) {
        return(NULL)
      }

      # Check if we should render dual surfaces (both mode for comparison types)
      compare_mode <- selected_compare_mode()
      show_dual_surfaces <- (mode %in% c("sex-compare", "group-compare")) &&
                            compare_mode == "both" &&
                            devicetype == "plotly:3d_surface"

      # Ensure colscheme has a valid default
      if (is.null(colscheme) || is.na(colscheme) || colscheme == "") {
        colscheme <- "Viridis:viridis"
      }

      parts <- str_split(colscheme, ":", simplify = TRUE)
      family <- parts[1]
      pal <- parts[2]

      # Validate parsed values
      if (is.na(family) || is.na(pal)) {
        family <- "Viridis"
        pal <- "viridis"
      }

      # Build color scale for single surface mode
      hue_posn <- seq(0, 1, length.out = 20)
      if (family == "Viridis") {
        cols <- viridis_pal(option = pal, direction = ifelse(selected_pal_reverse(), -1, 1))(20)
      } else {
        cols <- create_colbrew_ramp(pal)(20)
        if (selected_pal_reverse()) cols <- rev(cols)
      }
      colscale_list <- list(hue_posn, cols)

      # Get ages and years from data
      data_wide <- data %>%
        select(year, age, z_var) %>%
        pivot_wider(names_from = age, values_from = z_var)

      years <- data_wide$year
      data_wide$year <- NULL
      ages <- as.numeric(names(data_wide))
      val_mtrx <- as.matrix(data_wide)

      n_ages <- length(ages)
      n_years <- length(years)

      # Handle dual translucent surfaces mode
      if (show_dual_surfaces) {
        # Get alpha values
        alpha_1 <- selected_alpha_1()
        alpha_2 <- selected_alpha_2()

        # Determine which columns contain the dual surface data
        if (mode == "sex-compare") {
          # Use lmr_male and lmr_female
          data_wide_1 <- data %>%
            select(year, age, lmr_male) %>%
            pivot_wider(names_from = age, values_from = lmr_male)
          data_wide_1$year <- NULL
          mtrx_1 <- as.matrix(data_wide_1)

          data_wide_2 <- data %>%
            select(year, age, lmr_female) %>%
            pivot_wider(names_from = age, values_from = lmr_female)
          data_wide_2$year <- NULL
          mtrx_2 <- as.matrix(data_wide_2)

          name_1 <- "Male mortality"
          name_2 <- "Female mortality"
          color_1 <- "blue"
          color_2 <- "red"
          z_title <- "Log mortality rate"

        } else if (mode == "group-compare") {
          # Use lmr_B and lmr_A
          data_wide_1 <- data %>%
            select(year, age, lmr_B) %>%
            pivot_wider(names_from = age, values_from = lmr_B)
          data_wide_1$year <- NULL
          mtrx_1 <- as.matrix(data_wide_1)

          data_wide_2 <- data %>%
            select(year, age, lmr_A) %>%
            pivot_wider(names_from = age, values_from = lmr_A)
          data_wide_2$year <- NULL
          mtrx_2 <- as.matrix(data_wide_2)

          name_1 <- "Population B"
          name_2 <- "Population A"
          color_1 <- "blue"
          color_2 <- "red"
          z_title <- "Log mortality rate"
        }

        # Build custom hover text for dual surfaces - with richer information
        yearvec <- rep(years, times = n_ages)
        agevec <- rep(ages, each = n_years)
        cohortvec <- yearvec - agevec
        diff_vec <- as.vector(mtrx_1 - mtrx_2)
        ratio_vec <- 10 ^ diff_vec  # Convert log diff to mortality ratio

        if (mode == "sex-compare") {
          custom_text <- paste0(
            "In ", yearvec,
            ", at age ", agevec,
            " (", cohortvec, " birth cohort)\n",
            "Male LMR: ", round(as.vector(mtrx_1), 3),
            "\nFemale LMR: ", round(as.vector(mtrx_2), 3),
            "\nDiff: ", round(diff_vec, 3),
            "\nRatio: ", round(ratio_vec, 2),
            " (", round(1 / ratio_vec, 2), ")"
          ) %>%
            matrix(n_years, n_ages)
        } else {
          # group-compare
          custom_text <- paste0(
            "In ", yearvec,
            ", at age ", agevec,
            " (", cohortvec, " birth cohort)\n",
            "Pop B LMR: ", round(as.vector(mtrx_1), 3),
            "\nPop A LMR: ", round(as.vector(mtrx_2), 3),
            "\nDiff (B-A): ", round(diff_vec, 3),
            "\nRatio: ", round(ratio_vec, 2),
            " (", round(1 / ratio_vec, 2), ")"
          ) %>%
            matrix(n_years, n_ages)
        }

        # Create dual surface plot
        p <- plot_ly(
          showscale = FALSE,
          source = surface_source
        ) %>%
          add_surface(
            name = name_1,
            x = ~ages,
            y = ~years,
            z = ~mtrx_1,
            opacity = alpha_1,
            colorscale = list(c(0, 1), c(color_1, color_1)),
            hoverinfo = "text",
            text = ~custom_text
          ) %>%
          add_surface(
            name = name_2,
            x = ~ages,
            y = ~years,
            z = ~mtrx_2,
            opacity = alpha_2,
            colorscale = list(c(0, 1), c(color_2, color_2)),
            hoverinfo = "text",
            text = ~custom_text
          ) %>%
          layout(scene = list(
            zaxis = list(title = z_title),
            xaxis = list(title = "Age in years"),
            yaxis = list(title = "Year"),
            aspectratio = list(
              x = n_ages / n_years,
              y = 1,
              z = 0.5
            )
          )) %>%
          event_register("plotly_click")

        return(p)
      }

      # Standard single surface mode
      # Build custom hover text - mode-specific for richer information
      yearvec <- rep(years, times = n_ages)
      agevec <- rep(ages, each = n_years)
      cohortvec <- yearvec - agevec

      if (mode == "sex-compare") {
        # Sex comparison: show ratio, inverse ratio, and excess
        ratio_wide <- data %>%
          select(year, age, ratio) %>%
          pivot_wider(names_from = age, values_from = ratio)
        ratio_wide$year <- NULL
        ratio_mtrx <- as.matrix(ratio_wide)

        excess_wide <- data %>%
          select(year, age, excess) %>%
          pivot_wider(names_from = age, values_from = excess)
        excess_wide$year <- NULL
        excess_mtrx <- as.matrix(excess_wide)

        custom_text <- paste0(
          "In ", yearvec,
          ", at age ", agevec,
          " (", cohortvec, " birth cohort)\n",
          ifelse(
            as.vector(ratio_mtrx) > 1,
            paste0(round(as.vector(ratio_mtrx), 2), " male deaths per female death"),
            paste0(round(1 / as.vector(ratio_mtrx), 2), " female deaths per male death")
          ),
          "\n(",
          ifelse(
            as.vector(excess_mtrx) > 0,
            paste0(round(as.vector(excess_mtrx) * 1000, 0), " excess male deaths/1000 females"),
            paste0(-round(as.vector(excess_mtrx) * 1000, 0), " excess female deaths/1000 males")
          ),
          ")"
        ) %>%
          matrix(n_years, n_ages)

      } else if (mode == "pop-sex-compare") {
        # Population sex ratio: show ratio and excess
        ratio_wide <- data %>%
          select(year, age, ratio) %>%
          pivot_wider(names_from = age, values_from = ratio)
        ratio_wide$year <- NULL
        ratio_mtrx <- as.matrix(ratio_wide)

        excess_wide <- data %>%
          select(year, age, excess) %>%
          pivot_wider(names_from = age, values_from = excess)
        excess_wide$year <- NULL
        excess_mtrx <- as.matrix(excess_wide)

        custom_text <- paste0(
          "In ", yearvec,
          ", at age ", agevec,
          " (", cohortvec, " birth cohort)\n",
          ifelse(
            as.vector(ratio_mtrx) > 1,
            paste0(round(as.vector(ratio_mtrx), 2), " males per female"),
            paste0(round(1 / as.vector(ratio_mtrx), 2), " females per male")
          ),
          "\n(",
          ifelse(
            as.vector(excess_mtrx) > 0,
            paste0(round(as.vector(excess_mtrx) * 1000, 0), " more males/1000 females"),
            paste0(-round(as.vector(excess_mtrx) * 1000, 0), " more females/1000 males")
          ),
          ")"
        ) %>%
          matrix(n_years, n_ages)

      } else if (mode == "group-compare") {
        # Group comparison: show diff, ratio, and individual values
        lmr_A_wide <- data %>%
          select(year, age, lmr_A) %>%
          pivot_wider(names_from = age, values_from = lmr_A)
        lmr_A_wide$year <- NULL
        lmr_A_mtrx <- as.matrix(lmr_A_wide)

        lmr_B_wide <- data %>%
          select(year, age, lmr_B) %>%
          pivot_wider(names_from = age, values_from = lmr_B)
        lmr_B_wide$year <- NULL
        lmr_B_mtrx <- as.matrix(lmr_B_wide)

        diff_mtrx <- lmr_B_mtrx - lmr_A_mtrx
        ratio_mtrx <- 10 ^ diff_mtrx  # Convert log diff to ratio

        custom_text <- paste0(
          "In ", yearvec,
          ", at age ", agevec,
          " (", cohortvec, " birth cohort)\n",
          "Diff in log mortality: ", round(as.vector(diff_mtrx), 2),
          "\nRatio: ", round(as.vector(ratio_mtrx), 2),
          " (", round(1 / as.vector(ratio_mtrx), 2), ")",
          "\nB: ", round(as.vector(lmr_B_mtrx), 2),
          "  A: ", round(as.vector(lmr_A_mtrx), 2)
        ) %>%
          matrix(n_years, n_ages)

      } else {
        # Default: standard hover text
        custom_text <- paste0(
          "Year: ", yearvec,
          "\nAge: ", agevec,
          " (", cohortvec, " birth cohort)",
          "\n", selected_outcome(), ": ", round(as.vector(val_mtrx), 3)
        ) %>%
          matrix(n_years, n_ages)
      }

      if (devicetype == "plotly:lexis") {
        p <- plot_ly(
          z = ~t(val_mtrx),
          x = ~years,
          y = ~ages,
          source = surface_source
        ) %>%
          add_heatmap(
            colorscale = colscale_list,
            hoverinfo = "text",
            text = ~t(custom_text)
          ) %>%
          layout(
            yaxis = list(title = "Age in years"),
            xaxis = list(title = "Year")
          ) %>%
          event_register("plotly_click")
      } else {
        # 3D surface with source for crosstalk
        p <- plot_ly(
          x = ~ages,
          y = ~years,
          z = ~val_mtrx,
          surfacecolor = ~val_mtrx,
          source = surface_source
        ) %>%
          add_surface(
            colorscale = colscale_list,
            colorbar = list(title = selected_outcome()),
            hoverinfo = "text",
            text = ~custom_text
          ) %>%
          layout(scene = list(
            zaxis = list(title = selected_outcome()),
            xaxis = list(title = "Age in years"),
            yaxis = list(title = "Year"),
            aspectratio = list(
              x = n_ages / n_years,
              y = 1,
              z = 0.5
            )
          )) %>%
          event_register("plotly_click")
      }

      return(p)
    }

    # Create subplot based on click event (crosstalk drill-down)
    # @param arrangement "row" for horizontal (1 row, 3 cols) or "col" for vertical (3 rows, 1 col)
    produce_subplot <- function(click_data, full_data, arrangement = "row") {
      if (is.null(click_data) || length(click_data) == 0) {
        return(NULL)
      }

      this_age <- click_data$x[1]
      this_year <- click_data$y[1]
      this_cohort <- this_year - this_age

      raw_data <- data()
      req(raw_data)
      correction <- selected_continuityvalue()

      # Get country name for title - extract from data or use provided reactive
      ctry_name <- if (!is.null(country_name)) {
        country_name()
      } else {
        # Extract country code(s) from data and look up names
        codes <- unique(raw_data$code)
        if (length(codes) == 0) {
          "Selected population"
        } else if (length(codes) == 1) {
          # Single country - look up the name from codes_named
          name_match <- names(codes_named)[codes_named == codes[1]]
          if (length(name_match) > 0) name_match[1] else codes[1]
        } else {
          # Multiple countries - show count
          paste0(length(codes), " populations")
        }
      }

      if (mode %in% c("singular", "population")) {
        # Prepare data for standard mortality/population subplot
        plot_data <- raw_data %>%
          filter(age <= 100) %>%
          group_by(year, age) %>%
          summarise(
            num_deaths = sum(num_deaths, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            num_population = sum(num_population, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            mr = (num_deaths + correction) / (exposure + correction),
            birth_cohort = year - age
          )

        y_var <- if (selected_outcome() == "Population") "num_population" else "mr"
        y_title <- if (selected_outcome() == "Population") "Population" else "Mortality rate"
        y_log <- selected_outcome() != "Population"

        # Age slice (varying year)
        p1 <- plot_data %>%
          filter(age == this_age) %>%
          plot_ly(x = ~year, y = as.formula(paste0("~", y_var))) %>%
          add_lines(
            hoverinfo = "text",
            text = ~paste0(
              "Year: ", year,
              "\nAge: ", age,
              if (y_var == "mr") paste0("\nLog mortality: ", round(log(mr, 10), 3)) else "",
              if (y_var == "mr") paste0("\nDeaths per 10,000: ", round(10000 * mr, 0)) else paste0("\nPopulation: ", scales::comma(num_population))
            )
          )

        # Year slice (varying age)
        p2 <- plot_data %>%
          filter(year == this_year) %>%
          plot_ly(x = ~age, y = as.formula(paste0("~", y_var))) %>%
          add_lines(
            hoverinfo = "text",
            text = ~paste0(
              "Age: ", age,
              "\nYear: ", year,
              if (y_var == "mr") paste0("\nLog mortality: ", round(log(mr, 10), 3)) else "",
              if (y_var == "mr") paste0("\nDeaths per 10,000: ", round(10000 * mr, 0)) else paste0("\nPopulation: ", scales::comma(num_population))
            )
          )

        # Cohort slice (varying age along cohort)
        p3 <- plot_data %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age, y = as.formula(paste0("~", y_var))) %>%
          add_lines(
            hoverinfo = "text",
            text = ~paste0(
              "Age: ", age,
              "\nCohort: ", birth_cohort,
              if (y_var == "mr") paste0("\nLog mortality: ", round(log(mr, 10), 3)) else "",
              if (y_var == "mr") paste0("\nDeaths per 10,000: ", round(10000 * mr, 0)) else paste0("\nPopulation: ", scales::comma(num_population))
            )
          )

        # Use nrows based on arrangement
        nrows <- if (arrangement == "col") 3 else 1
        p <- subplot(list(p1, p2, p3), nrows = nrows, shareY = TRUE, titleX = TRUE) %>%
          layout(
            yaxis = list(
              title = y_title,
              type = if (y_log) "log" else "linear"
            ),
            xaxis = list(title = paste0("Year (age ", this_age, ")")),
            xaxis2 = list(title = paste0("Age (year ", this_year, ")"), range = c(0, 100)),
            xaxis3 = list(title = paste0("Age (", this_cohort, " cohort)"), range = c(0, 100)),
            showlegend = FALSE,
            title = paste0(y_title, " for ", ctry_name, " at age ", this_age, ", year ", this_year)
          )

      } else if (mode == "sex-compare") {
        # Sex ratio subplot
        plot_data <- raw_data %>%
          filter(age <= 100) %>%
          filter(gender %in% c("Male", "Female")) %>%
          group_by(year, age, gender) %>%
          summarise(
            num_deaths = sum(num_deaths, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          pivot_wider(names_from = gender, values_from = c(num_deaths, exposure)) %>%
          mutate(
            mr_male = (num_deaths_Male + correction) / (exposure_Male + correction),
            mr_female = (num_deaths_Female + correction) / (exposure_Female + correction),
            ratio = mr_male / mr_female,
            difference = (mr_male - mr_female) / mr_female,
            birth_cohort = year - age
          )

        p1 <- plot_data %>%
          filter(age == this_age) %>%
          plot_ly(x = ~year, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Year: ", year, "\nM:F ratio: ", round(ratio, 2),
            "\nDifference: ", round(1000 * difference, 1), " per 1000"
          ))

        p2 <- plot_data %>%
          filter(year == this_year) %>%
          plot_ly(x = ~age, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age, "\nM:F ratio: ", round(ratio, 2),
            "\nDifference: ", round(1000 * difference, 1), " per 1000"
          ))

        p3 <- plot_data %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age, "\nCohort: ", birth_cohort,
            "\nM:F ratio: ", round(ratio, 2)
          ))

        nrows <- if (arrangement == "col") 3 else 1
        p <- subplot(list(p1, p2, p3), nrows = nrows, shareY = TRUE, titleX = TRUE) %>%
          layout(
            yaxis = list(title = "Sex difference in mortality"),
            xaxis = list(title = paste0("Year (age ", this_age, ")")),
            xaxis2 = list(title = paste0("Age (year ", this_year, ")"), range = c(0, 100)),
            xaxis3 = list(title = paste0("Age (", this_cohort, " cohort)"), range = c(0, 100)),
            showlegend = FALSE,
            title = paste0("Sex mortality ratios for ", ctry_name)
          )

      } else if (mode == "pop-sex-compare") {
        # Population sex ratio subplot
        plot_data <- raw_data %>%
          filter(age <= 100) %>%
          filter(gender %in% c("Male", "Female")) %>%
          group_by(year, age, gender) %>%
          summarise(num_population = sum(num_population, na.rm = TRUE), .groups = "drop") %>%
          pivot_wider(names_from = gender, values_from = num_population) %>%
          mutate(
            ratio = Male / Female,
            difference = (Male - Female) / Female,
            birth_cohort = year - age
          )

        p1 <- plot_data %>%
          filter(age == this_age) %>%
          plot_ly(x = ~year, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Year: ", year, "\nM:F ratio: ", round(ratio, 3),
            "\n", abs(round(1000 * difference, 0)), ifelse(difference > 0, " more", " fewer"), " males/1000 females"
          ))

        p2 <- plot_data %>%
          filter(year == this_year) %>%
          plot_ly(x = ~age, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age, "\nM:F ratio: ", round(ratio, 3)
          ))

        p3 <- plot_data %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age, y = ~difference) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age, "\nCohort: ", birth_cohort
          ))

        nrows <- if (arrangement == "col") 3 else 1
        p <- subplot(list(p1, p2, p3), nrows = nrows, shareY = TRUE, titleX = TRUE) %>%
          layout(
            yaxis = list(title = "Sex population difference"),
            xaxis = list(title = paste0("Year (age ", this_age, ")")),
            xaxis2 = list(title = paste0("Age (year ", this_year, ")"), range = c(0, 100)),
            xaxis3 = list(title = paste0("Age (", this_cohort, " cohort)"), range = c(0, 100)),
            showlegend = FALSE,
            title = paste0("Population sex ratios for ", ctry_name)
          )

      } else if (mode == "group-compare" && !is.null(data_comparator)) {
        # Group comparison subplot
        data_b <- raw_data %>%
          filter(age <= 100) %>%
          group_by(year, age) %>%
          summarise(
            deaths = sum(num_deaths, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(lmr_B = log((deaths + correction) / (exposure + correction), 10))

        data_a <- data_comparator() %>%
          filter(age <= 100) %>%
          group_by(year, age) %>%
          summarise(
            deaths = sum(num_deaths, na.rm = TRUE),
            exposure = sum(exposure, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(lmr_A = log((deaths + correction) / (exposure + correction), 10))

        plot_data <- data_b %>%
          inner_join(data_a, by = c("year", "age")) %>%
          mutate(
            diff_lmr = lmr_B - lmr_A,
            birth_cohort = year - age
          )

        absmax <- max(abs(plot_data$diff_lmr), na.rm = TRUE)

        p1 <- plot_data %>%
          filter(age == this_age) %>%
          plot_ly(x = ~year, y = ~diff_lmr) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Year: ", year,
            "\nDiff: ", round(diff_lmr, 3),
            "\nB: ", round(lmr_B, 3), " A: ", round(lmr_A, 3)
          ))

        p2 <- plot_data %>%
          filter(year == this_year) %>%
          plot_ly(x = ~age, y = ~diff_lmr) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age,
            "\nDiff: ", round(diff_lmr, 3)
          ))

        p3 <- plot_data %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age, y = ~diff_lmr) %>%
          add_lines(hoverinfo = "text", text = ~paste0(
            "Age: ", age, " (Cohort: ", birth_cohort, ")",
            "\nDiff: ", round(diff_lmr, 3)
          ))

        nrows <- if (arrangement == "col") 3 else 1
        p <- subplot(list(p1, p2, p3), nrows = nrows, shareY = TRUE, titleX = TRUE) %>%
          layout(
            yaxis = list(title = "Difference in log mortality", range = c(-absmax, absmax)),
            xaxis = list(title = paste0("Year (age ", this_age, ")")),
            xaxis2 = list(title = paste0("Age (year ", this_year, ")"), range = c(0, 100)),
            xaxis3 = list(title = paste0("Age (", this_cohort, " cohort)"), range = c(0, 100)),
            showlegend = FALSE,
            title = paste0("Group comparison at age ", this_age, ", year ", this_year)
          )

      } else {
        return(NULL)
      }

      return(p)
    }

    # Render ggplot output
    output$main_ggplot <- renderPlot({
      req(data())
      req(input$plot_graphs)  # Wait for button click
      outcome_data <- derive_outcome(data, selected_outcome(), selected_continuityvalue())
      produce_surface_ggplot(outcome_data, selected_colourscheme(), selected_devicevis())
    })

    # Render plotly output
    output$main_plotly <- renderPlotly({
      req(data())
      req(input$plot_graphs)  # Wait for button click
      outcome_data <- derive_outcome(data, selected_outcome(), selected_continuityvalue())
      produce_surface_plotly(outcome_data, selected_colourscheme(), selected_devicevis())
    })

    # Render subplot based on click (crosstalk) - for row arrangement
    output$subplot_row <- renderPlotly({
      click_data <- event_data("plotly_click", source = surface_source)
      produce_subplot(click_data, data(), arrangement = "row")
    })

    # Render subplot for column arrangement (stacked vertically)
    output$subplot_col <- renderPlotly({
      click_data <- event_data("plotly_click", source = surface_source)
      produce_subplot(click_data, data(), arrangement = "col")
    })

    # Get main plot UI based on device type
    get_main_plot_ui <- function(dv, height = "500px") {
      switch(
        dv,
        "ggplot2:lexis" = plotOutput(ns("main_ggplot"), height = height),
        "plotly:lexis" = plotlyOutput(ns("main_plotly"), height = height),
        "plotly:3d_surface" = plotlyOutput(ns("main_plotly"), height = height)
      )
    }

    # Build title text for fullscreen header
    get_title_text <- function() {
      outcome <- selected_outcome()
      paste0(outcome, " | ", mode)
    }

    # Normal (non-maximized) layout - compact view with maximize button
    output$normal_layout <- renderUI({
      req(plot_generated())

      # Only show when NOT maximized
      if (is_maximized()) {
        return(NULL)
      }

      arrangement <- input$layout_arrangement %||% "main-3row"
      dv <- selected_devicevis()

      # Compact heights for normal view
      main_height_only <- "400px"
      main_height_col <- "400px"
      subplot_height_col <- "380px"
      main_height_row <- "300px"
      subplot_height_row <- "180px"

      content <- if (arrangement == "main-only") {
        get_main_plot_ui(dv, height = main_height_only)
      } else if (arrangement == "main-3col") {
        fluidRow(
          column(width = 8, get_main_plot_ui(dv, height = main_height_col)),
          column(width = 4,
            tags$small("Click surface for slices"),
            plotlyOutput(ns("subplot_col"), height = subplot_height_col)
          )
        )
      } else {
        div(
          get_main_plot_ui(dv, height = main_height_row),
          tags$small("Click surface for slices"),
          plotlyOutput(ns("subplot_row"), height = subplot_height_row)
        )
      }

      div(
        # Maximize button
        div(
          style = "margin-bottom: 10px;",
          actionButton(
            ns("toggle_maximize"),
            label = tagList(icon("expand"), " Maximize"),
            class = "btn-outline-primary btn-sm"
          )
        ),
        content
      )
    })

    # Fullscreen overlay - covers entire viewport with title bar and resizable panels
    output$fullscreen_overlay <- renderUI({
      req(plot_generated())

      # Only show when maximized
      if (!is_maximized()) {
        return(NULL)
      }

      arrangement <- input$layout_arrangement %||% "main-3row"
      dv <- selected_devicevis()

      # Generate unique IDs for resizable elements
      container_id <- ns("resizable_container")
      main_panel_id <- ns("main_panel")
      subplot_panel_id <- ns("subplot_panel")
      splitter_id <- ns("splitter")

      # Build the visualization content based on arrangement
      viz_content <- if (arrangement == "main-only") {
        # No resizing needed for main-only
        div(
          style = "height: calc(100vh - 60px); padding: 10px;",
          get_main_plot_ui(dv, height = "100%")
        )
      } else if (arrangement == "main-3col") {
        # Column layout with vertical splitter (drag left/right)
        div(
          id = container_id,
          class = "resizable-col-container",
          style = "height: calc(100vh - 60px); padding: 10px;",

          # Main panel (left)
          div(
            id = main_panel_id,
            class = "main-panel",
            get_main_plot_ui(dv, height = "100%")
          ),

          # Vertical splitter (drag handle)
          div(
            id = splitter_id,
            class = "splitter-vertical",
            title = "Drag to resize"
          ),

          # Subplot panel (right)
          div(
            id = subplot_panel_id,
            class = "subplot-panel",
            tags$small("Click on surface to see slices", style = "color: #666; margin-bottom: 5px;"),
            plotlyOutput(ns("subplot_col"), height = "calc(100% - 25px)")
          ),

          # Initialize the resizer via JavaScript
          tags$script(HTML(sprintf(
            "setTimeout(function() { initColResizer('%s', '%s', '%s', '%s'); }, 300);",
            container_id, main_panel_id, subplot_panel_id, splitter_id
          )))
        )
      } else {
        # Row layout with horizontal splitter (drag up/down)
        div(
          id = container_id,
          class = "resizable-row-container",
          style = "height: calc(100vh - 60px); padding: 10px;",

          # Main panel (top)
          div(
            id = main_panel_id,
            class = "main-panel",
            style = "height: 60%;",
            get_main_plot_ui(dv, height = "100%")
          ),

          # Horizontal splitter (drag handle)
          div(
            id = splitter_id,
            class = "splitter-horizontal",
            title = "Drag to resize"
          ),

          # Subplot panel (bottom)
          div(
            id = subplot_panel_id,
            class = "subplot-panel",
            style = "height: calc(40% - 16px);",
            tags$small("Click on surface to see slices", style = "color: #666;"),
            plotlyOutput(ns("subplot_row"), height = "calc(100% - 20px)")
          ),

          # Initialize the resizer via JavaScript
          tags$script(HTML(sprintf(
            "setTimeout(function() { initRowResizer('%s', '%s', '%s', '%s'); }, 300);",
            container_id, main_panel_id, subplot_panel_id, splitter_id
          )))
        )
      }

      # Fullscreen overlay container
      div(
        id = ns("fullscreen_container"),
        style = paste0(
          "position: fixed;",
          "top: 0;",
          "left: 0;",
          "width: 100vw;",
          "height: 100vh;",
          "background-color: white;",
          "z-index: 9999;",
          "overflow: hidden;"
        ),

        # Title bar
        div(
          style = paste0(
            "height: 50px;",
            "background-color: #2c3e50;",
            "color: white;",
            "display: flex;",
            "align-items: center;",
            "justify-content: space-between;",
            "padding: 0 20px;",
            "box-shadow: 0 2px 4px rgba(0,0,0,0.2);"
          ),

          # Title text (left side)
          tags$span(
            style = "font-size: 16px; font-weight: 500;",
            textOutput(ns("fullscreen_title"), inline = TRUE)
          ),

          # Right side controls
          div(
            style = "display: flex; align-items: center; gap: 15px;",

            # Drag hint
            if (arrangement != "main-only") {
              tags$small(
                style = "color: #95a5a6;",
                icon("arrows-alt-h" , style = if(arrangement == "main-3col") "" else "display:none;"),
                icon("arrows-alt-v", style = if(arrangement == "main-3row") "" else "display:none;"),
                " Drag divider to resize"
              )
            },

            # Exit button
            actionButton(
              ns("exit_fullscreen"),
              label = tagList(icon("times"), " Exit"),
              class = "btn btn-outline-light btn-sm"
            )
          )
        ),

        # Visualization content
        viz_content
      )
    })

    # Title text for fullscreen mode
    output$fullscreen_title <- renderText({
      req(data())

      # Get population name from data
      raw_data <- data()
      codes <- unique(raw_data$code)
      pop_name <- if (length(codes) == 0) {
        ""
      } else if (length(codes) == 1) {
        name_match <- names(codes_named)[codes_named == codes[1]]
        if (length(name_match) > 0) name_match[1] else codes[1]
      } else {
        paste0(length(codes), " populations")
      }

      outcome <- selected_outcome()
      arrangement <- input$layout_arrangement %||% "main-3row"
      arr_label <- switch(
        arrangement,
        "main-only" = "Surface Only",
        "main-3col" = "Surface + Column Subplots",
        "main-3row" = "Surface + Row Subplots"
      )

      # Add dual surfaces indicator for comparison modes
      compare_mode <- selected_compare_mode()
      display_mode_label <- ""
      if (mode %in% c("sex-compare", "group-compare") && compare_mode == "both") {
        display_mode_label <- " (Dual Surfaces)"
      }

      paste0(pop_name, " | ", outcome, display_mode_label, " | ", arr_label)
    })

    return(NULL)
  })
}
