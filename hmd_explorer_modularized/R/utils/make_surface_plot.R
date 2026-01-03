# Utility: Create plotly surface plots
# Analogous to Make3dLexisSurface.py in Python version

#' Create a 3D Lexis surface plot
#' @param z_list List from make_z_list with age, year, vals
#' @param colorscale Plotly colorscale (list of position/color pairs)
#' @param z_title Title for z-axis
#' @param source Source ID for crosstalk events
#' @param log_z Whether to use log scale for z-axis
#' @return Plotly figure object
make_3d_surface <- function(z_list, colorscale, z_title = "Value",
                             source = NULL, log_z = FALSE) {
  ages <- z_list$age
  years <- z_list$year
  val_mtrx <- z_list$vals

  n_ages <- length(ages)
  n_years <- length(years)

  # Build hover text
  custom_text <- paste0(
    "Year: ", rep(years, times = n_ages),
    "\nAge: ", rep(ages, each = n_years),
    "\n", z_title, ": ", round(as.vector(val_mtrx), 3)
  ) %>%
    matrix(n_years, n_ages)

  p <- plot_ly(
    x = ~ages,
    y = ~years,
    z = ~val_mtrx,
    surfacecolor = ~val_mtrx,
    source = source
  ) %>%
    add_surface(
      colorscale = colorscale,
      colorbar = list(title = z_title),
      hoverinfo = "text",
      text = ~custom_text
    ) %>%
    layout(scene = list(
      zaxis = list(
        title = z_title,
        type = if (log_z) "log" else "linear"
      ),
      xaxis = list(title = "Age in years"),
      yaxis = list(title = "Year"),
      aspectratio = list(
        x = n_ages / n_years,
        y = 1,
        z = 0.5
      )
    ))

  if (!is.null(source)) {
    p <- p %>% event_register("plotly_click")
  }

  p
}


#' Create a 2D Lexis heatmap
#' @param z_list List from make_z_list with age, year, vals
#' @param colorscale Plotly colorscale
#' @param z_title Title for colorbar
#' @param source Source ID for crosstalk events
#' @return Plotly figure object
make_2d_heatmap <- function(z_list, colorscale, z_title = "Value",
                             source = NULL) {
  ages <- z_list$age
  years <- z_list$year
  val_mtrx <- z_list$vals

  p <- plot_ly(
    z = ~t(val_mtrx),
    x = ~years,
    y = ~ages,
    source = source
  ) %>%
    add_heatmap(
      colorscale = colorscale,
      colorbar = list(title = z_title)
    ) %>%
    layout(
      yaxis = list(title = "Age in years"),
      xaxis = list(title = "Year")
    )

  if (!is.null(source)) {
    p <- p %>% event_register("plotly_click")
  }

  p
}


#' Create APC (Age-Period-Cohort) subplots
#' Analogous to MakeApcSubplots.py
#' @param subplot_data List from extract_subplot_data
#' @param y_var Variable name for y-axis
#' @param y_title Title for y-axis
#' @param log_y Whether to use log scale
#' @param arrangement "row" for horizontal, "col" for vertical
#' @return Plotly subplot figure
make_apc_subplots <- function(subplot_data, y_var = "mr", y_title = "Mortality rate",
                               log_y = TRUE, arrangement = "row") {
  click_age <- subplot_data$click_age
  click_year <- subplot_data$click_year
  click_cohort <- subplot_data$click_cohort

  # Age slice plot (by year)
  p1 <- subplot_data$age_slice %>%
    plot_ly(x = ~year, y = as.formula(paste0("~", y_var))) %>%
    add_lines(hoverinfo = "text", text = ~paste0("Year: ", year, "\n", y_title, ": ", round(.data[[y_var]], 4)))

  # Period slice plot (by age)
  p2 <- subplot_data$period_slice %>%
    plot_ly(x = ~age, y = as.formula(paste0("~", y_var))) %>%
    add_lines(hoverinfo = "text", text = ~paste0("Age: ", age, "\n", y_title, ": ", round(.data[[y_var]], 4)))

  # Cohort slice plot (by age)
  p3 <- subplot_data$cohort_slice %>%
    plot_ly(x = ~age, y = as.formula(paste0("~", y_var))) %>%
    add_lines(hoverinfo = "text", text = ~paste0("Age: ", age, "\nCohort: ", birth_cohort))

  nrows <- if (arrangement == "col") 3 else 1

  subplot(list(p1, p2, p3), nrows = nrows, shareY = TRUE, titleX = TRUE) %>%
    layout(
      yaxis = list(
        title = y_title,
        type = if (log_y) "log" else "linear"
      ),
      xaxis = list(title = paste0("Year (age ", click_age, ")")),
      xaxis2 = list(title = paste0("Age (year ", click_year, ")"), range = c(0, 100)),
      xaxis3 = list(title = paste0("Age (", click_cohort, " cohort)"), range = c(0, 100)),
      showlegend = FALSE
    )
}
