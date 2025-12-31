# Test setup file for HMD Explorer
# This file is sourced before all tests run

library(testthat)
library(tidyverse)

# Path to the app directory
app_dir <- file.path(dirname(dirname(dirname(getwd()))), "hmd_explorer")

# Source the functions we need to test (without running the server)
# We extract standalone functions from server.R

# ============================================================================
# EXTRACTED FUNCTIONS FOR TESTING
# These are the pure functions from server.R that can be unit tested
# ============================================================================

#' Convert population data from long to wide format for 3D plotting
#' @param X data frame with year, age, and value columns
#' @param what column name to extract (default: "num_population")
#' @return List with age, year, and vals (matrix)
make_z_list_pop <- function(X, what = "num_population") {
  out_df <- X %>%
    select(year = year,
           age = age,
           val = !!what)

  out_df %>%
    select(year, age, val) %>%
    spread(age, val) -> tmp

  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}

#' Convert mortality data to matrix form with optional logarithmic transformation
#' @param X data frame with year, age, num_deaths, exposure
#' @param what "lmr_k" (log mortality rate) or "mr" (mortality rate)
#' @param adjust pseudocount added to numerator and denominator
#' @param k logarithm base (default: 10)
#' @return List with age, year, and vals (matrix)
make_z_list <- function(X,
                        what = "lmr_k",
                        adjust = 0,
                        k = 10) {
  tmp <- X %>%
    select(year = year,
           age = age,
           n = num_deaths,
           N = exposure)

  if (what == "lmr_k") {
    out_df <- tmp %>%
      mutate(mr = (n + adjust) / (N + adjust), val = log(mr, k)) %>%
      mutate(val = ifelse(is.nan(val), NA, val))
  } else if (what == "mr") {
    out_df <- tmp %>%
      mutate(val = (n + adjust) / (N + adjust))
  }

  out_df %>%
    select(year, age, val) %>%
    spread(age, val) -> tmp

  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}

#' Filter and aggregate mortality data for group comparisons
#' @param full_data the full HMD dataset
#' @param input list containing filter parameters
#' @return Aggregated data frame with lmr by pop_group, year, age
get_selected_data <- function(full_data, input) {
  tmp <- full_data %>%
    filter(gender == input$gender_select) %>%
    mutate(
      pop_group = case_when(
        code %in% input$multi_code_select_B ~ "B",
        code %in% input$multi_code_select_A ~ "A",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(pop_group))

  if (input$limit_age) {
    tmp <- tmp %>%
      filter(age >= input$age_limits[1], age <= input$age_limits[2])
  }

  if (input$limit_period) {
    tmp <- tmp %>%
      filter(year >= input$period_limits[1],
             year <= input$period_limits[2])
  }

  tmp <- tmp %>%
    group_by(pop_group, year, age) %>%
    summarise(
      exposure = sum(exposure, na.rm = T),
      num_deaths = sum(num_deaths, na.rm = T),
      .groups = "drop"
    ) %>%
    ungroup() %>%
    mutate(lmr = log((num_deaths + input$small_n_correction) /
                       (exposure + input$small_n_correction), 10))

  return(tmp)
}

# ============================================================================
# LIFE TABLE FUNCTIONS (from scripts/calc_qx.R)
# ============================================================================

init_cohort_size <- 50000

#' Calculate survivor numbers from survival probabilities
#' @param x data frame with p_x column
#' @param init_size initial cohort size (default: 50,000)
#' @return data frame with cohort_size column appended
calculate_survivors_properly <- function(x, init_size = init_cohort_size) {
  k <- dim(x)[1]
  cohort_size <- rep(NA, k)
  cohort_size[1] <- init_size * x$p_x[1]
  for (i in 2:k) {
    cohort_size[i] <- cohort_size[i - 1] * x$p_x[i]
  }
  output <- data.frame(x, cohort_size = cohort_size)
}

#' Calculate probability of death (qx) from mortality rate
#' @param age age in years
#' @param sex "Male" or "Female"
#' @param deaths number of deaths
#' @param exposure person-years of exposure
#' @return qx (probability of death)
calc_qx <- function(age, sex, deaths, exposure) {
  m_x <- deaths / exposure
  a_x <- if (age == 0) {
    if (m_x > 0.107) {
      if (sex == "Female") {
        0.350
      } else {
        0.330
      }
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

  q_x <- m_x / (1 + (1 - a_x) * m_x)
  return(q_x)
}

# ============================================================================
# TEST DATA GENERATORS
# ============================================================================

#' Create synthetic test data mimicking HMD structure
#' @param n_years number of years
#' @param n_ages number of ages
#' @param codes vector of country codes
#' @param genders vector of genders
#' @return data frame with HMD-like structure
create_test_data <- function(n_years = 10,
                              n_ages = 5,
                              codes = c("TEST1", "TEST2"),
                              genders = c("Male", "Female", "Total")) {
  expand.grid(
    year = 2000:(2000 + n_years - 1),
    age = 0:(n_ages - 1),
    code = codes,
    gender = genders,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      # Simulate realistic mortality patterns
      base_rate = case_when(
        age == 0 ~ 0.005,  # infant mortality
        age < 5 ~ 0.0001,
        age < 20 ~ 0.0005,
        TRUE ~ 0.001 * exp(0.1 * age)  # exponential increase with age
      ),
      # Males have higher mortality
      gender_factor = ifelse(gender == "Male", 1.3,
                              ifelse(gender == "Female", 1.0, 1.15)),
      # Base population
      num_population = round(100000 * exp(-0.02 * age)),
      exposure = num_population * 1.0,  # assume 1 year exposure per person
      num_deaths = round(exposure * base_rate * gender_factor)
    ) %>%
    select(code, year, age, gender, num_deaths, num_population, exposure)
}

#' Create mock input list for testing
#' @return list mimicking Shiny input object
create_mock_input <- function(
    code_select = "TEST1",
    gender_select = "Total",
    limit_age = FALSE,
    age_limits = c(0, 110),
    limit_period = FALSE,
    period_limits = c(1750, 2020),
    multi_code_select_A = "TEST1",
    multi_code_select_B = "TEST2",
    small_n_correction = 50,
    ratio_limiter = 3,
    group_mode = "diff",
    limit_diffz = FALSE,
    diffz_limits = c(-3, 3),
    group_A_alpha = 0.7,
    group_B_alpha = 0.7
) {
  list(
    code_select = code_select,
    gender_select = gender_select,
    limit_age = limit_age,
    age_limits = age_limits,
    limit_period = limit_period,
    period_limits = period_limits,
    multi_code_select_A = multi_code_select_A,
    multi_code_select_B = multi_code_select_B,
    small_n_correction = small_n_correction,
    ratio_limiter = ratio_limiter,
    group_mode = group_mode,
    limit_diffz = limit_diffz,
    diffz_limits = diffz_limits,
    group_A_alpha = group_A_alpha,
    group_B_alpha = group_B_alpha
  )
}
