# Correlation Maps Module for HMD Explorer
# Generates correlation heatmaps showing age-to-age mortality correlations over time

#' Correlation Maps UI Module
#' @param id Module namespace ID
make_corrmaps_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Correlation type selection
    selectInput(
      ns("select_corrtype"),
      label = "Select correlation type",
      choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
      selected = "pearson"
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
      selected = "Viridis:viridis"
    ),

    # Continuity correction
    checkboxInput(
      ns("add_continuity"),
      value = FALSE,
      label = "Add continuity correction"
    ),
    uiOutput(ns("dynamic_continuity")),

    # Scale limits
    checkboxInput(
      ns("fix_scalelimits"),
      value = FALSE,
      label = "Set custom scale limits"
    ),
    uiOutput(ns("selected_scalelimits")),

    # Visualization type
    selectInput(
      ns("select_devicevis"),
      label = "Select visualization type",
      choices = c(
        `ggplot2: Correlation heatmap` = "ggplot2:lexis",
        `plotly: Correlation heatmap` = "plotly:lexis"
      ),
      selected = "plotly:lexis"
    ),

    # Plot button
    actionButton(
      ns("generate_plot"),
      label = "Generate Correlation Map",
      class = "btn-primary"
    ),

    tags$hr(),

    # Main correlation plot output
    uiOutput(ns("mainplot")),

    # Subplot for age trend comparison (crosstalk)
    tags$hr(),
    tags$h5("Click on heatmap to compare age trends"),
    plotlyOutput(ns("show_age_trends"), height = "250px")
  )
}


#' Correlation Maps Server Module
#' @param id Module namespace ID
#' @param data Reactive data from select_data_server
make_corrmaps_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive inputs
    selected_add_continuity <- reactive({ input$add_continuity })
    selected_continuityvalue <- reactive({ input$continuity_value %||% 0 })
    selected_corrtype <- reactive({ input$select_corrtype })
    selected_colscheme <- reactive({ input$select_colourscheme })
    selected_devicevis <- reactive({ input$select_devicevis })
    selected_setscale <- reactive({ input$fix_scalelimits })
    selected_scalelimits <- reactive({ input$corr_scalelimits %||% c(-1, 1) })

    # Dynamic continuity correction UI
    output$dynamic_continuity <- renderUI({
      if (!isTRUE(selected_add_continuity())) {
        return(NULL)
      }
      numericInput(
        ns("continuity_value"),
        label = "Continuity correction value",
        value = 5,
        min = 0, max = 500
      )
    })

    # Dynamic scale limits UI
    output$selected_scalelimits <- renderUI({
      if (!isTRUE(selected_setscale())) {
        return(NULL)
      }
      sliderInput(
        ns("corr_scalelimits"),
        label = "Correlation scale limits",
        value = c(-1, 1),
        min = -1, max = 1, step = 0.1
      )
    })

    # Calculate correlation data
    calc_corrdata <- function(data_df, corrtype, correction, mode = "ggplot") {
      if (is.null(correction)) correction <- 0

      dta_trnd <- data_df %>%
        mutate(Mx_approx = (num_deaths + correction) / (num_population + correction)) %>%
        group_by(year, age) %>%
        summarise(mean_Mx = mean(Mx_approx, na.rm = TRUE), .groups = "drop") %>%
        mutate(log_mean_Mx = log(mean_Mx, 10))

      tmp <- dta_trnd %>%
        select(-mean_Mx) %>%
        pivot_wider(names_from = age, values_from = log_mean_Mx) %>%
        select(-year)

      cor_matrix <- cor(tmp, method = corrtype, use = "pairwise.complete.obs")

      if (mode == "plotly") {
        return(cor_matrix)
      }

      # Convert to long format for ggplot
      cor_df <- cor_matrix %>%
        as_tibble() %>%
        mutate(from_age = rownames(cor_matrix)) %>%
        pivot_longer(
          cols = -from_age,
          names_to = "to_age",
          values_to = "value"
        ) %>%
        mutate(
          from_age = as.numeric(from_age),
          to_age = as.numeric(to_age)
        )

      return(cor_df)
    }

    # Calculate two-age comparison data
    calc_twoage_data <- function(data_df, first_age, second_age, correction) {
      if (is.null(correction)) correction <- 0

      dta_age1 <- data_df %>%
        filter(age == first_age) %>%
        mutate(Mx_approx = (num_deaths + correction) / (num_population + correction)) %>%
        mutate(log_mean_Mx = log(Mx_approx, 10)) %>%
        select(year, age, log_mean_Mx)

      dta_age2 <- data_df %>%
        filter(age == second_age) %>%
        mutate(Mx_approx = (num_deaths + correction) / (num_population + correction)) %>%
        mutate(log_mean_Mx = log(Mx_approx, 10)) %>%
        select(year, age, log_mean_Mx)

      bind_rows(dta_age1, dta_age2)
    }

    # Create ggplot correlation map
    plot_corrmap <- function(data_df, colscheme, scalelimit) {
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

      p <- data_df %>%
        ggplot(aes(x = from_age, y = to_age, fill = value)) +
        geom_tile() +
        scale_x_continuous(breaks = seq(0, 100, by = 10)) +
        scale_y_continuous(breaks = seq(0, 100, by = 10)) +
        coord_equal() +
        labs(x = "Age", y = "Age", fill = "Correlation") +
        theme_minimal()

      if (family == "Viridis") {
        if (is.null(scalelimit)) {
          p <- p + scale_fill_viridis_c(option = pal)
        } else {
          p <- p + scale_fill_viridis_c(option = pal, limits = scalelimit)
        }
      }

      return(p)
    }

    # Create plotly correlation map
    plotly_corrmap <- function(cor_matrix, colscheme, scalelimit) {
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

      # Build color scale
      if (family == "Viridis") {
        hue_posn <- seq(0, 1, length.out = 20)
        colscale_list <- list(hue_posn, viridis_pal(option = pal)(20))
      } else {
        hue_posn <- seq(0, 1, length.out = 20)
        colscale_list <- list(hue_posn, viridis_pal()(20))
      }

      # Create hover text matrix
      hovertext_matrix <- paste0(
        "Age ", rep(rownames(cor_matrix), each = length(colnames(cor_matrix))),
        " vs Age ", rep(colnames(cor_matrix), times = length(rownames(cor_matrix))),
        "\nCorrelation: ", round(cor_matrix, 3)
      ) %>%
        matrix(nrow = length(rownames(cor_matrix)))

      p <- plot_ly(
        source = "corrmap_main",
        x = ~rownames(cor_matrix),
        y = ~colnames(cor_matrix),
        z = ~cor_matrix,
        hoverinfo = "text",
        text = hovertext_matrix
      ) %>%
        add_heatmap(
          colorscale = colscale_list,
          zmin = scalelimit[1],
          zmax = scalelimit[2]
        ) %>%
        layout(
          yaxis = list(title = "Age", scaleanchor = "x"),
          xaxis = list(title = "Age")
        ) %>%
        event_register("plotly_click")

      return(p)
    }

    # Render ggplot correlation map
    output$corrmap <- renderPlot({
      req(data())
      req(input$generate_plot)  # Wait for button click

      isolate({
        data() %>%
          calc_corrdata(
            corrtype = selected_corrtype(),
            correction = selected_continuityvalue()
          ) %>%
          plot_corrmap(
            colscheme = selected_colscheme(),
            scalelimit = if (selected_setscale()) selected_scalelimits() else NULL
          )
      })
    })

    # Render plotly correlation map
    output$corrmap_plotly <- renderPlotly({
      req(data())
      req(input$generate_plot)  # Wait for button click

      isolate({
        data() %>%
          calc_corrdata(
            corrtype = selected_corrtype(),
            correction = selected_continuityvalue(),
            mode = "plotly"
          ) %>%
          plotly_corrmap(
            colscheme = selected_colscheme(),
            scalelimit = selected_scalelimits()
          )
      })
    })

    # Render age trend subplot (crosstalk)
    output$show_age_trends <- renderPlotly({
      s <- event_data("plotly_click", source = "corrmap_main")

      if (is.null(s)) {
        return(NULL)
      }

      first_age <- as.numeric(s$x[1])
      second_age <- as.numeric(s$y[1])

      data_ss <- data() %>%
        calc_twoage_data(
          first_age = first_age,
          second_age = second_age,
          correction = selected_continuityvalue()
        )

      p <- data_ss %>%
        plot_ly(
          x = ~year,
          y = ~log_mean_Mx,
          color = ~factor(age),
          colors = c("red", "darkgreen")
        ) %>%
        add_lines() %>%
        layout(
          yaxis = list(title = "Log mortality rate"),
          xaxis = list(title = "Year"),
          title = paste0("Mortality trends: Age ", first_age, " vs Age ", second_age),
          showlegend = TRUE
        )

      return(p)
    })

    # Dynamic main plot output
    output$mainplot <- renderUI({
      dv_selection <- selected_devicevis()
      switch(
        dv_selection,
        "ggplot2:lexis" = plotOutput(ns("corrmap"), height = "500px"),
        "plotly:lexis" = plotlyOutput(ns("corrmap_plotly"), height = "500px")
      )
    })

    return(NULL)
  })
}
