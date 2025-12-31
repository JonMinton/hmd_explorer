# Graphics Module for HMD Explorer
# Generates Lexis surfaces and 3D plots for mortality/population data
# Supports click-to-drill-down subplots via plotly crosstalk

#' Graphics UI Module
#' @param id Module namespace ID
#' @param show_subplot Whether to show subplot output area
make_graphics_ui <- function(id, show_subplot = TRUE) {
  ns <- NS(id)

  subplot_ui <- if (show_subplot) {
    tagList(
      tags$hr(),
      tags$h5("Click on surface to see slices"),
      plotlyOutput(ns("subplot"), height = "300px")
    )
  } else {
    NULL
  }

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

    # Plot button
    actionButton(
      ns("plot_graphs"),
      label = "Generate Plot",
      class = "btn-primary"
    ),

    tags$hr(),

    # Main plot output
    uiOutput(ns("mainplot")),

    # Subplot output (for crosstalk drill-down)
    subplot_ui,

    # Debug info (hidden in production)
    tags$small(
      textOutput(ns("plot_info"))
    )
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

    # Selected values (triggered by plot button)
    selected_outcome <- eventReactive(input$plot_graphs, {
      input$select_outcome
    })

    selected_colourscheme <- eventReactive(input$plot_graphs, {
      input$select_colourscheme
    })

    selected_devicevis <- eventReactive(input$plot_graphs, {
      input$select_devicevis
    })

    selected_add_continuity <- eventReactive(input$plot_graphs, {
      input$add_continuity
    })

    selected_continuityvalue <- eventReactive(input$plot_graphs, {
      if (isTRUE(input$add_continuity)) {
        input$continuity_value %||% 0.5
      } else {
        0.5  # Default small correction to avoid log(0)
      }
    })

    selected_pal_reverse <- eventReactive(input$plot_graphs, {
      input$reverse_pal_direction
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
            ratio = mr_male / mr_female,
            excess = (mr_male - mr_female) / mr_female
          )

        if (outcome == "Log mortality") {
          processed <- processed %>%
            mutate(z_var = log(ratio, 10)) %>%
            select(year, age, z_var, ratio, excess)
        } else {
          processed <- processed %>%
            mutate(z_var = ratio) %>%
            select(year, age, z_var, ratio, excess)
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

      parts <- str_split(colscheme, ":", simplify = TRUE)
      family <- parts[1]
      pal <- parts[2]

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
    produce_surface_plotly <- function(data, colscheme, devicetype) {
      if (!devicetype %in% c("plotly:lexis", "plotly:3d_surface") || is.null(data)) {
        return(NULL)
      }

      parts <- str_split(colscheme, ":", simplify = TRUE)
      family <- parts[1]
      pal <- parts[2]

      # Build color scale
      hue_posn <- seq(0, 1, length.out = 20)
      if (family == "Viridis") {
        cols <- viridis_pal(option = pal, direction = ifelse(selected_pal_reverse(), -1, 1))(20)
      } else {
        cols <- create_colbrew_ramp(pal)(20)
        if (selected_pal_reverse()) cols <- rev(cols)
      }
      colscale_list <- list(hue_posn, cols)

      # Convert to matrix format
      data_wide <- data %>%
        select(year, age, z_var) %>%
        pivot_wider(names_from = age, values_from = z_var)

      years <- data_wide$year
      data_wide$year <- NULL
      ages <- as.numeric(names(data_wide))
      val_mtrx <- as.matrix(data_wide)

      n_ages <- length(ages)
      n_years <- length(years)

      # Build custom hover text
      custom_text <- paste0(
        "Year: ", rep(years, times = n_ages),
        "\nAge: ", rep(ages, each = n_years),
        "\n", selected_outcome(), ": ", round(as.vector(val_mtrx), 3)
      ) %>%
        matrix(n_years, n_ages)

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
    produce_subplot <- function(click_data, full_data) {
      if (is.null(click_data) || length(click_data) == 0) {
        return(NULL)
      }

      this_age <- click_data$x[1]
      this_year <- click_data$y[1]
      this_cohort <- this_year - this_age

      raw_data <- data()
      req(raw_data)
      correction <- selected_continuityvalue()

      # Get country name for title
      ctry_name <- if (!is.null(country_name)) country_name() else "Selected country"

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

        p <- subplot(list(p1, p2, p3), shareY = TRUE, titleX = TRUE) %>%
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

        p <- subplot(list(p1, p2, p3), shareY = TRUE, titleX = TRUE) %>%
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

        p <- subplot(list(p1, p2, p3), shareY = TRUE, titleX = TRUE) %>%
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

        p <- subplot(list(p1, p2, p3), shareY = TRUE, titleX = TRUE) %>%
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
      outcome_data <- derive_outcome(data, selected_outcome(), selected_continuityvalue())
      produce_surface_ggplot(outcome_data, selected_colourscheme(), selected_devicevis())
    })

    # Render plotly output
    output$main_plotly <- renderPlotly({
      req(data())
      outcome_data <- derive_outcome(data, selected_outcome(), selected_continuityvalue())
      produce_surface_plotly(outcome_data, selected_colourscheme(), selected_devicevis())
    })

    # Render subplot based on click (crosstalk)
    output$subplot <- renderPlotly({
      click_data <- event_data("plotly_click", source = surface_source)
      produce_subplot(click_data, data())
    })

    # Dynamic main plot output
    output$mainplot <- renderUI({
      dv <- selected_devicevis()
      switch(
        dv,
        "ggplot2:lexis" = plotOutput(ns("main_ggplot"), height = "500px"),
        "plotly:lexis" = plotlyOutput(ns("main_plotly"), height = "500px"),
        "plotly:3d_surface" = plotlyOutput(ns("main_plotly"), height = "600px")
      )
    })

    # Plot info
    output$plot_info <- renderText({
      req(data())
      paste("Data:", nrow(data()), "rows |", "Mode:", mode)
    })

    return(NULL)
  })
}
