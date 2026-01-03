# Utility: Extract data for age/period/cohort subplots
# Analogous to ExtractSubplotDataSeries.py in Python version

#' Extract subplot data series for a given click point
#' @param data Data frame with mortality/population data
#' @param click_age Age value from click event
#' @param click_year Year value from click event
#' @param correction Continuity correction value
#' @param outcome Type of outcome ("mortality", "population", "ratio")
#' @return List with age_slice, period_slice, cohort_slice data frames
extract_subplot_data <- function(data, click_age, click_year, correction = 0.5,
                                  outcome = "mortality") {
  click_cohort <- click_year - click_age

  # Prepare base data with mortality rate
  base_data <- data %>%
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
      log_mr = log(mr, 10),
      birth_cohort = year - age

)

  # Age slice: all years for the clicked age
  age_slice <- base_data %>%
    filter(age == click_age) %>%
    arrange(year)

  # Period slice: all ages for the clicked year
  period_slice <- base_data %>%
    filter(year == click_year) %>%
    arrange(age)

  # Cohort slice: all ages for the birth cohort
  cohort_slice <- base_data %>%
    filter(birth_cohort == click_cohort) %>%
    arrange(age)

  list(
    age_slice = age_slice,
    period_slice = period_slice,
    cohort_slice = cohort_slice,
    click_age = click_age,
    click_year = click_year,
    click_cohort = click_cohort
  )
}


#' Extract sex ratio subplot data
#' @param data Data frame with both Male and Female records
#' @param click_age Age value from click event
#' @param click_year Year value from click event
#' @param correction Continuity correction value
#' @return List with age_slice, period_slice, cohort_slice for sex ratios
extract_sex_ratio_subplot_data <- function(data, click_age, click_year,
                                            correction = 0.5) {
  click_cohort <- click_year - click_age

  base_data <- data %>%
    filter(age <= 100) %>%
    filter(gender %in% c("Male", "Female")) %>%
    group_by(year, age, gender) %>%
    summarise(
      num_deaths = sum(num_deaths, na.rm = TRUE),
      exposure = sum(exposure, na.rm = TRUE),
      num_population = sum(num_population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = gender,
      values_from = c(num_deaths, exposure, num_population)
    ) %>%
    mutate(
      mr_male = (num_deaths_Male + correction) / (exposure_Male + correction),
      mr_female = (num_deaths_Female + correction) / (exposure_Female + correction),
      ratio = mr_male / mr_female,
      difference = (mr_male - mr_female) / mr_female,
      pop_ratio = num_population_Male / num_population_Female,
      birth_cohort = year - age
    )

  list(
    age_slice = base_data %>% filter(age == click_age) %>% arrange(year),
    period_slice = base_data %>% filter(year == click_year) %>% arrange(age),
    cohort_slice = base_data %>% filter(birth_cohort == click_cohort) %>% arrange(age),
    click_age = click_age,
    click_year = click_year,
    click_cohort = click_cohort
  )
}


#' Extract group comparison subplot data
#' @param data_a Data frame for group A
#' @param data_b Data frame for group B
#' @param click_age Age value from click event
#' @param click_year Year value from click event
#' @param correction Continuity correction value
#' @return List with age_slice, period_slice, cohort_slice for group differences
extract_group_comparison_subplot_data <- function(data_a, data_b, click_age,
                                                   click_year, correction = 0.5) {
  click_cohort <- click_year - click_age

  prep_group <- function(data, group_name) {
    data %>%
      filter(age <= 100) %>%
      group_by(year, age) %>%
      summarise(
        deaths = sum(num_deaths, na.rm = TRUE),
        exposure = sum(exposure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        lmr = log((deaths + correction) / (exposure + correction), 10),
        group = group_name
      )
  }

  group_a <- prep_group(data_a, "A")
  group_b <- prep_group(data_b, "B")

  base_data <- group_b %>%
    inner_join(
      group_a %>% select(year, age, lmr_a = lmr),
      by = c("year", "age")
    ) %>%
    rename(lmr_b = lmr) %>%
    mutate(
      diff_lmr = lmr_b - lmr_a,
      birth_cohort = year - age
    )

  list(
    age_slice = base_data %>% filter(age == click_age) %>% arrange(year),
    period_slice = base_data %>% filter(year == click_year) %>% arrange(age),
    cohort_slice = base_data %>% filter(birth_cohort == click_cohort) %>% arrange(age),
    click_age = click_age,
    click_year = click_year,
    click_cohort = click_cohort
  )
}
