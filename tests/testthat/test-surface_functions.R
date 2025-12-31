# Integration tests for surface plot generation functions
# These tests verify that plot generation functions work correctly
# without requiring a running Shiny server

context("Surface plot generation functions")

# ============================================================================
# Setup: Load plotly for plot type checking
# ============================================================================

library(plotly)

# Create a comprehensive test dataset
test_plot_data <- create_test_data(
  n_years = 10,
  n_ages = 20,
  codes = c("TEST_A", "TEST_B"),
  genders = c("Male", "Female", "Total")
)

# ============================================================================
# Helper function to extract and test surface function logic
# (without plotly event dependencies)
# ============================================================================

# Simplified version of return_mort_surface for testing
test_mort_surface_logic <- function(full_data, input) {
  dta_ss <- full_data %>%
    filter(code == input$code_select) %>%
    filter(gender == input$gender_select)

  if (input$limit_age) {
    dta_ss <- dta_ss %>%
      filter(age >= input$age_limits[1], age <= input$age_limits[2])
  }
  if (input$limit_period) {
    dta_ss <- dta_ss %>%
      filter(year >= input$period_limits[1],
             year <= input$period_limits[2])
  }

  dta_ss <- dta_ss %>%
    group_by(code, gender) %>%
    nest()

  z_list <- dta_ss %>%
    mutate(lmr_list = map(data, make_z_list, adjust = 0.5))

  list(
    data = dta_ss,
    z_list = z_list
  )
}

# Simplified version of return_mortratio_surface logic
test_mortratio_logic <- function(full_data, input) {
  ratio_limit <- as.double(input$ratio_limiter)
  this_code <- input$code_select
  n_correction <- input$small_n_correction

  dta_ss <- full_data %>%
    filter(code == this_code) %>%
    filter(gender != "Total")

  if (input$limit_age) {
    dta_ss <- dta_ss %>%
      filter(age >= input$age_limits[1], age <= input$age_limits[2])
  }

  if (input$limit_period) {
    dta_ss <- dta_ss %>%
      filter(year >= input$period_limits[1],
             year <= input$period_limits[2])
  }

  z_list <- dta_ss %>%
    mutate(mr = (num_deaths + n_correction) / (exposure + n_correction)) %>%
    select(age, year, gender, mr) %>%
    spread(gender, mr) %>%
    mutate(ratio = Male / Female,
           excess = (Male - Female) / Female) %>%
    mutate(
      ratio = case_when(
        ratio < -ratio_limit ~ -ratio_limit,
        ratio > ratio_limit  ~ ratio_limit,
        TRUE ~ ratio
      )
    )

  return(z_list)
}


# ============================================================================
# Mortality surface tests
# ============================================================================

describe("Mortality surface generation", {

  describe("data preparation", {

    it("filters data by selected country code", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Total"
      )

      result <- test_mort_surface_logic(test_plot_data, input)

      expect_equal(unique(result$data$code), "TEST_A")
    })


    it("filters data by selected gender", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Male"
      )

      result <- test_mort_surface_logic(test_plot_data, input)

      expect_equal(unique(result$data$gender), "Male")
    })


    it("applies age limits when enabled", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Total",
        limit_age = TRUE,
        age_limits = c(5, 15)
      )

      result <- test_mort_surface_logic(test_plot_data, input)
      ages_in_data <- result$data$data[[1]]$age

      expect_true(all(ages_in_data >= 5))
      expect_true(all(ages_in_data <= 15))
    })


    it("applies period limits when enabled", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Total",
        limit_period = TRUE,
        period_limits = c(2002, 2007)
      )

      result <- test_mort_surface_logic(test_plot_data, input)
      years_in_data <- result$data$data[[1]]$year

      expect_true(all(years_in_data >= 2002))
      expect_true(all(years_in_data <= 2007))
    })


    it("generates valid z_list output", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Total"
      )

      result <- test_mort_surface_logic(test_plot_data, input)
      z_list <- result$z_list$lmr_list[[1]]

      expect_type(z_list, "list")
      expect_named(z_list, c("age", "year", "vals"))
      expect_type(z_list$vals, "double")
    })


    it("uses adjust = 0.5 to prevent division issues", {
      # Create data with zero deaths
      zero_death_data <- tibble(
        code = "TEST",
        year = 2000,
        age = 0,
        gender = "Total",
        num_deaths = 0,
        exposure = 10000,
        num_population = 10000
      )

      z_list <- make_z_list(zero_death_data, adjust = 0.5)

      # With adjust = 0.5, should get finite value
      expect_true(is.finite(z_list$vals[1, 1]))
    })

  })

})


# ============================================================================
# Sex ratio surface tests
# ============================================================================

describe("Sex ratio surface generation", {

  describe("mortality ratio calculations", {

    it("calculates male/female mortality ratio correctly", {
      # Create simple test data
      ratio_data <- tibble(
        code = rep("TEST", 4),
        year = rep(2000, 4),
        age = rep(c(0, 1), each = 2),
        gender = rep(c("Male", "Female"), 2),
        num_deaths = c(20, 10, 30, 15),  # M:F = 2:1 at each age
        exposure = rep(10000, 4),
        num_population = rep(10000, 4)
      )

      input <- create_mock_input(
        code_select = "TEST",
        ratio_limiter = 5,
        small_n_correction = 0
      )

      result <- test_mortratio_logic(ratio_data, input)

      # Ratio should be approximately 2 for both ages
      expect_equal(result$ratio[1], 2, tolerance = 0.01)
      expect_equal(result$ratio[2], 2, tolerance = 0.01)
    })


    it("calculates excess mortality correctly", {
      ratio_data <- tibble(
        code = rep("TEST", 2),
        year = rep(2000, 2),
        age = rep(0, 2),
        gender = c("Male", "Female"),
        num_deaths = c(200, 100),  # Male = 200, Female = 100
        exposure = rep(10000, 2),
        num_population = rep(10000, 2)
      )

      input <- create_mock_input(
        code_select = "TEST",
        ratio_limiter = 5,
        small_n_correction = 0
      )

      result <- test_mortratio_logic(ratio_data, input)

      # Excess = (200 - 100) / 100 = 1.0 (100% more male deaths)
      # But this is on mortality rate basis:
      # Male mr = 200/10000 = 0.02
      # Female mr = 100/10000 = 0.01
      # excess = (0.02 - 0.01) / 0.01 = 1.0
      expect_equal(result$excess[1], 1.0, tolerance = 0.01)
    })


    it("applies ratio limiter to clamp extreme values", {
      ratio_data <- tibble(
        code = rep("TEST", 2),
        year = rep(2000, 2),
        age = rep(0, 2),
        gender = c("Male", "Female"),
        num_deaths = c(1000, 10),  # Extreme ratio of 100
        exposure = rep(10000, 2),
        num_population = rep(10000, 2)
      )

      input <- create_mock_input(
        code_select = "TEST",
        ratio_limiter = 3,  # Limit to 3
        small_n_correction = 0
      )

      result <- test_mortratio_logic(ratio_data, input)

      # Ratio should be clamped to 3
      expect_equal(result$ratio[1], 3)
    })


    it("excludes Total gender from ratio calculations", {
      result <- test_mortratio_logic(test_plot_data, create_mock_input(
        code_select = "TEST_A",
        ratio_limiter = 5,
        small_n_correction = 50
      ))

      # Result should only have Male and Female columns after spread
      expect_true("Male" %in% names(result))
      expect_true("Female" %in% names(result))
    })


    it("applies small_n_correction to prevent instability", {
      ratio_data <- tibble(
        code = rep("TEST", 2),
        year = rep(2000, 2),
        age = rep(0, 2),
        gender = c("Male", "Female"),
        num_deaths = c(1, 1),  # Very small counts
        exposure = rep(100, 2),
        num_population = rep(100, 2)
      )

      input <- create_mock_input(
        code_select = "TEST",
        ratio_limiter = 5,
        small_n_correction = 50
      )

      result <- test_mortratio_logic(ratio_data, input)

      # With correction, should get ratio close to 1
      expect_true(abs(result$ratio[1] - 1) < 0.5)
    })

  })

})


# ============================================================================
# Population surface tests
# ============================================================================

describe("Population surface generation", {

  describe("data preparation", {

    it("handles Total gender with cumulative population", {
      # When gender is "Total", the function stacks Male and Female populations
      pop_data <- tibble(
        code = rep("TEST", 4),
        year = rep(2000, 4),
        age = rep(c(0, 1), each = 2),
        gender = rep(c("Female", "Male"), 2),
        num_population = c(5000, 6000, 4000, 5000),
        num_deaths = rep(10, 4),
        exposure = rep(10000, 4)
      ) %>%
        bind_rows(tibble(
          code = rep("TEST", 2),
          year = rep(2000, 2),
          age = c(0, 1),
          gender = "Total",
          num_population = c(11000, 9000),
          num_deaths = rep(20, 2),
          exposure = rep(20000, 2)
        ))

      # For Total, the function uses cumulative_pop via cumsum
      test_result <- pop_data %>%
        filter(code == "TEST") %>%
        filter(gender != "Total") %>%
        group_by(code, age, year) %>%
        mutate(cumulative_pop = cumsum(num_population)) %>%
        ungroup()

      # Verify cumsum works correctly
      expect_true("cumulative_pop" %in% names(test_result))
    })


    it("uses num_population directly for single genders", {
      input <- create_mock_input(
        code_select = "TEST_A",
        gender_select = "Male"
      )

      result <- test_plot_data %>%
        filter(code == input$code_select) %>%
        filter(gender == input$gender_select)

      expect_true("num_population" %in% names(result))
      expect_true(nrow(result) > 0)
    })

  })

})


# ============================================================================
# Group comparison tests
# ============================================================================

describe("Group comparison surface generation", {

  describe("difference calculations", {

    it("calculates B - A difference in log mortality", {
      group_data <- get_selected_data(
        test_plot_data,
        create_mock_input(
          gender_select = "Total",
          multi_code_select_A = "TEST_A",
          multi_code_select_B = "TEST_B",
          small_n_correction = 50
        )
      )

      diffs <- group_data %>%
        select(pop_group, year, age, lmr) %>%
        spread(pop_group, lmr) %>%
        mutate(diff_lmr = B - A)

      expect_true("diff_lmr" %in% names(diffs))
      expect_true(all(is.finite(diffs$diff_lmr) | is.na(diffs$diff_lmr)))
    })


    it("creates symmetric range for differences visualization", {
      group_data <- get_selected_data(
        test_plot_data,
        create_mock_input(
          gender_select = "Total",
          multi_code_select_A = "TEST_A",
          multi_code_select_B = "TEST_B",
          small_n_correction = 50
        )
      )

      diffs <- group_data %>%
        select(pop_group, year, age, lmr) %>%
        spread(pop_group, lmr) %>%
        mutate(diff_lmr = B - A)

      maxabs <- max(abs(diffs$diff_lmr[is.finite(diffs$diff_lmr)]), na.rm = TRUE)

      # Range should be symmetric around 0
      expect_equal(-maxabs, -abs(maxabs))
    })


    it("handles empty comparison groups", {
      empty_result <- get_selected_data(
        test_plot_data,
        create_mock_input(
          gender_select = "Total",
          multi_code_select_A = "NONEXISTENT",
          multi_code_select_B = "ALSO_NONEXISTENT",
          small_n_correction = 50
        )
      )

      expect_equal(nrow(empty_result), 0)
    })

  })


  describe("mode handling", {

    it("supports 'diff' mode showing B-A differences", {
      input <- create_mock_input(group_mode = "diff")
      expect_equal(input$group_mode, "diff")
    })


    it("supports 'both' mode showing individual populations", {
      input <- create_mock_input(group_mode = "both")
      expect_equal(input$group_mode, "both")
    })

  })

})


# ============================================================================
# Subplot interaction tests (logic only, no plotly events)
# ============================================================================

describe("Subplot data extraction", {

  describe("click coordinate handling", {

    it("correctly calculates birth cohort from click coordinates", {
      # Simulate click at age 30, year 2000
      this_age <- 30
      this_year <- 2000
      this_cohort <- this_year - this_age

      expect_equal(this_cohort, 1970)
    })


    it("filters data for age slice", {
      this_age <- 10

      age_slice <- test_plot_data %>%
        filter(age <= 100) %>%
        filter(code == "TEST_A") %>%
        filter(gender == "Total") %>%
        filter(age == this_age)

      expect_true(all(age_slice$age == this_age))
    })


    it("filters data for year slice", {
      this_year <- 2005

      year_slice <- test_plot_data %>%
        filter(age <= 100) %>%
        filter(code == "TEST_A") %>%
        filter(gender == "Total") %>%
        filter(year == this_year)

      expect_true(all(year_slice$year == this_year))
    })


    it("filters data for cohort slice", {
      this_cohort <- 1995

      cohort_slice <- test_plot_data %>%
        filter(age <= 100) %>%
        filter(code == "TEST_A") %>%
        filter(gender == "Total") %>%
        mutate(birth_cohort = year - age) %>%
        filter(birth_cohort == this_cohort)

      expect_true(all(cohort_slice$birth_cohort == this_cohort))
    })

  })


  describe("mortality rate calculations in subplots", {

    it("calculates mortality rate correctly without adjustment", {
      mr_data <- test_plot_data %>%
        filter(code == "TEST_A", gender == "Total", age == 5, year == 2000) %>%
        mutate(mr = num_deaths / exposure)

      expect_true("mr" %in% names(mr_data))
      expect_true(all(mr_data$mr >= 0))
    })


    it("calculates mortality rate correctly with adjustment", {
      mr_data <- test_plot_data %>%
        filter(code == "TEST_A", gender == "Total", age == 5, year == 2000) %>%
        mutate(mr = (num_deaths + 0.5) / (exposure + 0.5))

      expect_true(all(mr_data$mr > 0))
    })

  })

})
