# Tests for life table calculation functions
# calc_qx: calculates probability of death from mortality rate
# calculate_survivors_properly: calculates cohort survivors from survival probabilities

context("Life table calculations")

# ============================================================================
# calc_qx tests
# ============================================================================

describe("calc_qx", {

  # Basic functionality tests
  describe("basic calculations", {

    it("returns a numeric value", {
      result <- calc_qx(age = 30, sex = "Male", deaths = 100, exposure = 10000)
      expect_type(result, "double")
    })


    it("calculates qx correctly for ages > 0 (a_x = 0.5)", {
      # For age > 0, a_x = 0.5
      # Formula: q_x = m_x / (1 + (1 - a_x) * m_x)
      # With a_x = 0.5: q_x = m_x / (1 + 0.5 * m_x)

      deaths <- 100
      exposure <- 10000
      m_x <- deaths / exposure  # 0.01

      expected_qx <- m_x / (1 + 0.5 * m_x)

      result <- calc_qx(age = 30, sex = "Male", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("returns qx values between 0 and 1 for normal inputs", {
      result <- calc_qx(age = 50, sex = "Female", deaths = 50, exposure = 1000)

      expect_gte(result, 0)
      expect_lte(result, 1)
    })


    it("returns 0 when deaths = 0", {
      result <- calc_qx(age = 30, sex = "Male", deaths = 0, exposure = 10000)

      expect_equal(result, 0)
    })

  })


  # Infant mortality (age 0) tests
  describe("infant mortality calculations (age 0)", {

    it("uses high infant mortality formula for males when m_x > 0.107", {
      # When m_x > 0.107 and Male: a_x = 0.330
      deaths <- 200
      exposure <- 1000  # m_x = 0.2 > 0.107

      m_x <- deaths / exposure
      a_x <- 0.330
      expected_qx <- m_x / (1 + (1 - a_x) * m_x)

      result <- calc_qx(age = 0, sex = "Male", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("uses high infant mortality formula for females when m_x > 0.107", {
      # When m_x > 0.107 and Female: a_x = 0.350
      deaths <- 200
      exposure <- 1000  # m_x = 0.2 > 0.107

      m_x <- deaths / exposure
      a_x <- 0.350
      expected_qx <- m_x / (1 + (1 - a_x) * m_x)

      result <- calc_qx(age = 0, sex = "Female", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("uses low infant mortality formula for males when m_x <= 0.107", {
      # When m_x <= 0.107 and Male: a_x = 0.045 + 2.684 * m_x
      deaths <- 50
      exposure <- 1000  # m_x = 0.05 <= 0.107

      m_x <- deaths / exposure
      a_x <- 0.045 + 2.684 * m_x
      expected_qx <- m_x / (1 + (1 - a_x) * m_x)

      result <- calc_qx(age = 0, sex = "Male", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("uses low infant mortality formula for females when m_x <= 0.107", {
      # When m_x <= 0.107 and Female: a_x = 0.053 + 2.800 * m_x
      deaths <- 50
      exposure <- 1000  # m_x = 0.05 <= 0.107

      m_x <- deaths / exposure
      a_x <- 0.053 + 2.800 * m_x
      expected_qx <- m_x / (1 + (1 - a_x) * m_x)

      result <- calc_qx(age = 0, sex = "Female", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("correctly handles boundary case at m_x = 0.107", {
      # At exactly 0.107, should use the low mortality formula (m_x <= 0.107 is false)
      deaths <- 107
      exposure <- 1000  # m_x = 0.107

      # At boundary, m_x > 0.107 is FALSE, so use low mortality formula
      m_x <- deaths / exposure
      a_x <- 0.045 + 2.684 * m_x  # Male formula
      expected_qx <- m_x / (1 + (1 - a_x) * m_x)

      result <- calc_qx(age = 0, sex = "Male", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })

  })


  # Sex differences tests
  describe("sex differences", {

    it("produces different qx for males vs females at age 0 with low mortality", {
      deaths <- 50
      exposure <- 1000

      result_male <- calc_qx(age = 0, sex = "Male", deaths = deaths, exposure = exposure)
      result_female <- calc_qx(age = 0, sex = "Female", deaths = deaths, exposure = exposure)

      expect_false(result_male == result_female)
    })


    it("produces different qx for males vs females at age 0 with high mortality", {
      deaths <- 200
      exposure <- 1000

      result_male <- calc_qx(age = 0, sex = "Male", deaths = deaths, exposure = exposure)
      result_female <- calc_qx(age = 0, sex = "Female", deaths = deaths, exposure = exposure)

      expect_false(result_male == result_female)
    })


    it("produces same qx for males and females at ages > 0", {
      deaths <- 100
      exposure <- 10000

      result_male <- calc_qx(age = 30, sex = "Male", deaths = deaths, exposure = exposure)
      result_female <- calc_qx(age = 30, sex = "Female", deaths = deaths, exposure = exposure)

      expect_equal(result_male, result_female)
    })

  })


  # Edge cases
  describe("edge cases", {

    it("handles very small mortality rates", {
      result <- calc_qx(age = 10, sex = "Male", deaths = 1, exposure = 1e7)

      expect_true(result > 0)
      expect_true(result < 0.001)
    })


    it("handles very high mortality rates approaching 1", {
      # With 90% mortality rate
      result <- calc_qx(age = 100, sex = "Female", deaths = 900, exposure = 1000)

      expect_true(result > 0.8)
      expect_true(result <= 1)
    })


    it("handles age 1 (uses a_x = 0.5, not infant formula)", {
      deaths <- 10
      exposure <- 10000
      m_x <- deaths / exposure

      # Age 1 uses a_x = 0.5, not infant formula
      expected_qx <- m_x / (1 + 0.5 * m_x)

      result <- calc_qx(age = 1, sex = "Male", deaths = deaths, exposure = exposure)

      expect_equal(result, expected_qx, tolerance = 1e-10)
    })


    it("handles old ages (e.g., 110)", {
      result <- calc_qx(age = 110, sex = "Female", deaths = 800, exposure = 1000)

      expect_true(result > 0)
      expect_true(result <= 1)
    })

  })

})


# ============================================================================
# calculate_survivors_properly tests
# ============================================================================

describe("calculate_survivors_properly", {

  it("returns a data frame with cohort_size column", {
    test_data <- data.frame(
      age = 0:4,
      p_x = c(0.99, 0.999, 0.9995, 0.9997, 0.9998)
    )

    result <- calculate_survivors_properly(test_data)

    expect_true("cohort_size" %in% names(result))
    expect_s3_class(result, "data.frame")
  })


  it("uses default initial cohort size of 50000", {
    test_data <- data.frame(
      age = 0:2,
      p_x = c(1, 1, 1)  # No deaths
    )

    result <- calculate_survivors_properly(test_data)

    expect_equal(result$cohort_size[1], 50000)
  })


  it("uses custom initial cohort size when specified", {
    test_data <- data.frame(
      age = 0:2,
      p_x = c(1, 1, 1)
    )

    result <- calculate_survivors_properly(test_data, init_size = 100000)

    expect_equal(result$cohort_size[1], 100000)
  })


  it("correctly calculates cumulative survivors", {
    test_data <- data.frame(
      age = 0:3,
      p_x = c(0.99, 0.98, 0.97, 0.96)
    )
    init_size <- 100000

    result <- calculate_survivors_properly(test_data, init_size = init_size)

    # Manual calculation
    expected <- numeric(4)
    expected[1] <- init_size * 0.99
    expected[2] <- expected[1] * 0.98
    expected[3] <- expected[2] * 0.97
    expected[4] <- expected[3] * 0.96

    expect_equal(result$cohort_size, expected, tolerance = 1e-10)
  })


  it("returns zero cohort size when p_x = 0 at any age", {
    test_data <- data.frame(
      age = 0:4,
      p_x = c(0.99, 0.99, 0, 0.99, 0.99)  # Everyone dies at age 2
    )

    result <- calculate_survivors_properly(test_data, init_size = 10000)

    expect_equal(result$cohort_size[3], 0)
    expect_equal(result$cohort_size[4], 0)
    expect_equal(result$cohort_size[5], 0)
  })


  it("maintains constant cohort size when p_x = 1 (no deaths)", {
    test_data <- data.frame(
      age = 0:4,
      p_x = rep(1, 5)
    )

    result <- calculate_survivors_properly(test_data, init_size = 10000)

    expect_true(all(result$cohort_size == 10000))
  })


  it("preserves original columns in output", {
    test_data <- data.frame(
      age = 0:2,
      p_x = c(0.99, 0.99, 0.99),
      extra_col = c("a", "b", "c")
    )

    result <- calculate_survivors_properly(test_data)

    expect_true("age" %in% names(result))
    expect_true("p_x" %in% names(result))
    expect_true("extra_col" %in% names(result))
  })


  it("handles single row input", {
    test_data <- data.frame(
      age = 0,
      p_x = 0.99
    )

    result <- calculate_survivors_properly(test_data, init_size = 10000)

    expect_equal(nrow(result), 1)
    expect_equal(result$cohort_size[1], 9900)
  })

})


# ============================================================================
# Integration tests: calc_qx and calculate_survivors_properly together
# ============================================================================

describe("Life table calculation pipeline", {

  it("produces declining cohort sizes with realistic mortality", {
    # Simulate typical mortality pattern
    mortality_data <- tibble(
      age = 0:100,
      sex = "Female"
    ) %>%
      mutate(
        # Simplified Gompertz-like mortality
        m_x = case_when(
          age == 0 ~ 0.005,
          age < 5 ~ 0.0001,
          TRUE ~ 0.00005 * exp(0.08 * age)
        ),
        deaths = round(m_x * 10000),
        exposure = 10000
      )

    # Calculate q_x for each age
    mortality_data <- mortality_data %>%
      rowwise() %>%
      mutate(
        q_x = calc_qx(age = age, sex = sex, deaths = deaths, exposure = exposure),
        p_x = 1 - q_x
      ) %>%
      ungroup()

    # Calculate survivors
    result <- calculate_survivors_properly(as.data.frame(mortality_data), init_size = 100000)

    # Cohort should decline over age
    expect_true(all(diff(result$cohort_size) <= 0))

    # Most should survive to age 50
    expect_true(result$cohort_size[51] > 50000)

    # Few should survive to age 100
    expect_true(result$cohort_size[101] < 10000)
  })

})
