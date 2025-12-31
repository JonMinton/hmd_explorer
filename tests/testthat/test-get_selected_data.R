# Tests for get_selected_data function
# Filters and aggregates mortality data for group comparisons

context("Data filtering and aggregation")

# ============================================================================
# Setup test data
# ============================================================================

# Create standard test dataset
test_full_data <- tibble(
  code = rep(c("GBR_SCO", "GBRTENW", "USA", "FRA"), each = 20),
  year = rep(rep(2000:2004, each = 4), 4),
  age = rep(rep(0:3, times = 5), 4),
  gender = rep(c("Male", "Female", "Male", "Female"), each = 5, times = 4),
  num_deaths = round(runif(80, 10, 100)),
  exposure = round(runif(80, 10000, 50000))
) %>%
  # Add Total gender rows
  bind_rows(
    tibble(
      code = rep(c("GBR_SCO", "GBRTENW", "USA", "FRA"), each = 20),
      year = rep(rep(2000:2004, each = 4), 4),
      age = rep(rep(0:3, times = 5), 4),
      gender = "Total",
      num_deaths = round(runif(80, 20, 200)),
      exposure = round(runif(80, 20000, 100000))
    )
  )


# ============================================================================
# get_selected_data tests
# ============================================================================

describe("get_selected_data", {

  describe("basic functionality", {

    it("returns a tibble", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_age = FALSE,
        limit_period = FALSE,
        small_n_correction = 50
      )

      result <- get_selected_data(test_full_data, input)

      expect_s3_class(result, "tbl_df")
    })


    it("returns expected columns", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      expect_true("pop_group" %in% names(result))
      expect_true("year" %in% names(result))
      expect_true("age" %in% names(result))
      expect_true("exposure" %in% names(result))
      expect_true("num_deaths" %in% names(result))
      expect_true("lmr" %in% names(result))
    })


    it("correctly assigns pop_group A and B", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      expect_true("A" %in% result$pop_group)
      expect_true("B" %in% result$pop_group)
    })

  })


  describe("gender filtering", {

    it("filters by Male gender correctly", {
      input <- create_mock_input(
        gender_select = "Male",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      # Result should only contain aggregated data, check it's not empty
      expect_true(nrow(result) > 0)
    })


    it("filters by Female gender correctly", {
      input <- create_mock_input(
        gender_select = "Female",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(nrow(result) > 0)
    })


    it("filters by Total gender correctly", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(nrow(result) > 0)
    })

  })


  describe("age limiting", {

    it("does not filter ages when limit_age is FALSE", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_age = FALSE
      )

      result <- get_selected_data(test_full_data, input)

      # Should have all ages 0-3
      expect_equal(sort(unique(result$age)), c(0, 1, 2, 3))
    })


    it("filters ages correctly when limit_age is TRUE", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_age = TRUE,
        age_limits = c(1, 2)
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(all(result$age >= 1))
      expect_true(all(result$age <= 2))
    })


    it("includes boundary ages in filter", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_age = TRUE,
        age_limits = c(0, 3)
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(0 %in% result$age)
      expect_true(3 %in% result$age)
    })

  })


  describe("period limiting", {

    it("does not filter years when limit_period is FALSE", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_period = FALSE
      )

      result <- get_selected_data(test_full_data, input)

      expect_equal(sort(unique(result$year)), 2000:2004)
    })


    it("filters years correctly when limit_period is TRUE", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_period = TRUE,
        period_limits = c(2001, 2003)
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(all(result$year >= 2001))
      expect_true(all(result$year <= 2003))
    })


    it("includes boundary years in filter", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_period = TRUE,
        period_limits = c(2000, 2004)
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(2000 %in% result$year)
      expect_true(2004 %in% result$year)
    })

  })


  describe("population group selection", {

    it("handles single code in each group", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(nrow(result) > 0)
      expect_equal(sort(unique(result$pop_group)), c("A", "B"))
    })


    it("handles multiple codes in group B", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = c("GBRTENW", "USA")
      )

      result <- get_selected_data(test_full_data, input)

      # Group B should have aggregated data from GBRTENW and USA
      group_b <- result %>% filter(pop_group == "B")
      expect_true(nrow(group_b) > 0)
    })


    it("handles multiple codes in group A", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = c("GBR_SCO", "FRA"),
        multi_code_select_B = "USA"
      )

      result <- get_selected_data(test_full_data, input)

      group_a <- result %>% filter(pop_group == "A")
      expect_true(nrow(group_a) > 0)
    })


    it("excludes codes not in either group", {
      # Only include GBR_SCO and GBRTENW, exclude USA and FRA
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW"
      )

      result <- get_selected_data(test_full_data, input)

      # The result is aggregated by pop_group, so we can't directly check codes
      # But we can verify the aggregation includes only 2 groups
      expect_equal(length(unique(result$pop_group)), 2)
    })


    it("returns empty tibble when no codes match", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "NONEXISTENT1",
        multi_code_select_B = "NONEXISTENT2"
      )

      result <- get_selected_data(test_full_data, input)

      expect_equal(nrow(result), 0)
    })

  })


  describe("aggregation", {

    it("aggregates exposure and deaths by pop_group, year, age", {
      # Create data with known values for verification
      known_data <- tibble(
        code = c("A1", "A2", "B1", "B2"),
        year = rep(2000, 4),
        age = rep(0, 4),
        gender = rep("Total", 4),
        num_deaths = c(10, 20, 30, 40),
        exposure = c(1000, 2000, 3000, 4000)
      )

      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = c("A1", "A2"),
        multi_code_select_B = c("B1", "B2"),
        small_n_correction = 0
      )

      result <- get_selected_data(known_data, input)

      # Group A should have 10+20=30 deaths, 1000+2000=3000 exposure
      group_a <- result %>% filter(pop_group == "A", year == 2000, age == 0)
      expect_equal(group_a$num_deaths, 30)
      expect_equal(group_a$exposure, 3000)

      # Group B should have 30+40=70 deaths, 3000+4000=7000 exposure
      group_b <- result %>% filter(pop_group == "B", year == 2000, age == 0)
      expect_equal(group_b$num_deaths, 70)
      expect_equal(group_b$exposure, 7000)
    })


    it("handles NA values in aggregation", {
      data_with_na <- tibble(
        code = c("A1", "A2"),
        year = rep(2000, 2),
        age = rep(0, 2),
        gender = rep("Total", 2),
        num_deaths = c(10, NA),
        exposure = c(1000, 2000)
      )

      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = c("A1", "A2"),
        multi_code_select_B = "NONEXISTENT",
        small_n_correction = 0
      )

      result <- get_selected_data(data_with_na, input)

      # With na.rm = T, should sum to 10 (ignoring NA)
      group_a <- result %>% filter(pop_group == "A")
      expect_equal(group_a$num_deaths, 10)
    })

  })


  describe("log mortality rate calculation", {

    it("calculates lmr correctly with small_n_correction = 0", {
      known_data <- tibble(
        code = "A1",
        year = 2000,
        age = 0,
        gender = "Total",
        num_deaths = 100,
        exposure = 10000
      )

      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "A1",
        multi_code_select_B = "NONEXISTENT",
        small_n_correction = 0
      )

      result <- get_selected_data(known_data, input)

      # lmr = log10((100 + 0) / (10000 + 0)) = log10(0.01) = -2
      expected_lmr <- log(100 / 10000, 10)
      expect_equal(result$lmr[1], expected_lmr, tolerance = 1e-10)
    })


    it("applies small_n_correction to lmr calculation", {
      known_data <- tibble(
        code = "A1",
        year = 2000,
        age = 0,
        gender = "Total",
        num_deaths = 100,
        exposure = 10000
      )

      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "A1",
        multi_code_select_B = "NONEXISTENT",
        small_n_correction = 50
      )

      result <- get_selected_data(known_data, input)

      # lmr = log10((100 + 50) / (10000 + 50))
      expected_lmr <- log((100 + 50) / (10000 + 50), 10)
      expect_equal(result$lmr[1], expected_lmr, tolerance = 1e-10)
    })


    it("prevents log(0) with small_n_correction", {
      known_data <- tibble(
        code = "A1",
        year = 2000,
        age = 0,
        gender = "Total",
        num_deaths = 0,
        exposure = 10000
      )

      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "A1",
        multi_code_select_B = "NONEXISTENT",
        small_n_correction = 50
      )

      result <- get_selected_data(known_data, input)

      # With correction: log10((0 + 50) / (10000 + 50)) should be finite
      expect_true(is.finite(result$lmr[1]))
    })

  })


  describe("combined filtering", {

    it("applies both age and period limits together", {
      input <- create_mock_input(
        gender_select = "Total",
        multi_code_select_A = "GBR_SCO",
        multi_code_select_B = "GBRTENW",
        limit_age = TRUE,
        age_limits = c(1, 2),
        limit_period = TRUE,
        period_limits = c(2001, 2003)
      )

      result <- get_selected_data(test_full_data, input)

      expect_true(all(result$age >= 1 & result$age <= 2))
      expect_true(all(result$year >= 2001 & result$year <= 2003))
    })

  })

})
