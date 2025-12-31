# Data Transformation Functions for HMD Explorer
# These functions convert long-format data to matrix format for 3D plotting

#' Convert population data from long to wide format for 3D plotting
#'
#' @param X data frame with year, age, and value columns
#' @param what column name to extract (default: "num_population")
#' @return List with age, year, and vals (matrix)
make_z_list_pop <- function(X, what = "num_population") {
  out_df <- X %>%
    select(
      year = year,
      age = age,
      val = !!sym(what)
    )

  tmp <- out_df %>%
    select(year, age, val) %>%
    pivot_wider(names_from = age, values_from = val)

  years <- tmp$year
  tmp$year <- NULL
  ages <- as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)

  list(age = ages, year = years, vals = val_mtrx)
}


#' Convert mortality data to matrix form with optional logarithmic transformation
#'
#' @param X data frame with year, age, num_deaths, exposure
#' @param what "lmr_k" (log mortality rate) or "mr" (mortality rate)
#' @param adjust pseudocount added to numerator and denominator (prevents division by zero)
#' @param k logarithm base (default: 10)
#' @return List with age, year, and vals (matrix)
make_z_list <- function(X, what = "lmr_k", adjust = 0, k = 10) {
  tmp <- X %>%
    select(
      year = year,
      age = age,
      n = num_deaths,
      N = exposure
    )

  if (what == "lmr_k") {
    out_df <- tmp %>%
      mutate(
        mr = (n + adjust) / (N + adjust),
        val = log(mr, k)
      ) %>%
      mutate(val = ifelse(is.nan(val), NA, val))
  } else if (what == "mr") {
    out_df <- tmp %>%
      mutate(val = (n + adjust) / (N + adjust))
  }

  tmp2 <- out_df %>%
    select(year, age, val) %>%
    pivot_wider(names_from = age, values_from = val)

  years <- tmp2$year
  tmp2$year <- NULL
  ages <- as.numeric(names(tmp2))
  val_mtrx <- as.matrix(tmp2)

  list(age = ages, year = years, vals = val_mtrx)
}


#' Filter and aggregate mortality data for group comparisons
#'
#' @param full_data the full HMD dataset
#' @param codes_a vector of country codes for group A
#' @param codes_b vector of country codes for group B
#' @param gender_select selected gender ("Male", "Female", or "Total")
#' @param limit_age whether to filter by age
#' @param age_limits vector of c(min, max) ages
#' @param limit_period whether to filter by year
#' @param period_limits vector of c(min, max) years
#' @param small_n_correction value to add to numerator/denominator
#' @return Aggregated data frame with lmr by pop_group, year, age
get_selected_data <- function(full_data,
                               codes_a,
                               codes_b,
                               gender_select = "Total",
                               limit_age = FALSE,
                               age_limits = c(0, 110),
                               limit_period = FALSE,
                               period_limits = c(1750, 2020),
                               small_n_correction = 50) {

  tmp <- full_data %>%
    filter(gender == gender_select) %>%
    mutate(
      pop_group = case_when(
        code %in% codes_b ~ "B",
        code %in% codes_a ~ "A",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(pop_group))

  if (limit_age) {
    tmp <- tmp %>%
      filter(age >= age_limits[1], age <= age_limits[2])
  }

  if (limit_period) {
    tmp <- tmp %>%
      filter(year >= period_limits[1], year <= period_limits[2])
  }

  tmp <- tmp %>%
    group_by(pop_group, year, age) %>%
    summarise(
      exposure = sum(exposure, na.rm = TRUE),
      num_deaths = sum(num_deaths, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(
      lmr = log(
        (num_deaths + small_n_correction) / (exposure + small_n_correction),
        10
      )
    )

  return(tmp)
}


#' Calculate sex ratios from population or mortality data
#'
#' @param data data frame with male and female data
#' @param value_col column name containing values to compare
#' @param correction small value to add to avoid division issues
#' @param ratio_limit maximum ratio value (for clamping extreme values)
#' @return data frame with ratio and excess columns
calculate_sex_ratios <- function(data,
                                  value_col = "mr",
                                  correction = 50,
                                  ratio_limit = 3) {

  data %>%
    select(age, year, gender, value = !!sym(value_col)) %>%
    pivot_wider(names_from = gender, values_from = value) %>%
    mutate(
      ratio = Male / Female,
      excess = (Male - Female) / Female,
      ratio = case_when(
        ratio < -ratio_limit ~ -ratio_limit,
        ratio > ratio_limit ~ ratio_limit,
        TRUE ~ ratio
      )
    )
}
