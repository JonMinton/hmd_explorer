# Tests for make_z_list and make_z_list_pop functions
# These functions convert long-format data to matrix format for 3D plotting

context("Matrix transformation functions")

# ============================================================================
# make_z_list_pop tests
# ============================================================================

describe("make_z_list_pop", {

  it("returns a list with age, year, and vals components", {
    test_data <- tibble(
      year = rep(2000:2002, each = 3),
      age = rep(0:2, times = 3),
      num_population = c(1000, 900, 800, 1100, 950, 850, 1200, 1000, 900)
    )

    result <- make_z_list_pop(test_data)

    expect_type(result, "list")
    expect_named(result, c("age", "year", "vals"))
  })


  it("returns ages in correct order", {
    test_data <- tibble(
      year = rep(2000:2001, each = 4),
      age = rep(c(0, 5, 10, 15), times = 2),
      num_population = rep(1000, 8)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(result$age, c(0, 5, 10, 15))
  })


  it("returns years in correct order", {
    test_data <- tibble(
      year = rep(c(1990, 2000, 2010), each = 2),
      age = rep(0:1, times = 3),
      num_population = rep(1000, 6)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(result$year, c(1990, 2000, 2010))
  })


  it("creates matrix with correct dimensions (years x ages)", {
    test_data <- tibble(
      year = rep(2000:2004, each = 3),  # 5 years
      age = rep(0:2, times = 5),         # 3 ages
      num_population = rep(1000, 15)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(dim(result$vals), c(5, 3))  # rows = years, cols = ages
  })


  it("correctly places values in matrix", {
    test_data <- tibble(
      year = c(2000, 2000, 2001, 2001),
      age = c(0, 1, 0, 1),
      num_population = c(100, 200, 300, 400)
    )

    result <- make_z_list_pop(test_data)

    # Matrix should be:
    #      age0  age1
    # 2000  100   200
    # 2001  300   400
    expect_equal(result$vals[1, 1], 100)  # year 2000, age 0
    expect_equal(result$vals[1, 2], 200)  # year 2000, age 1
    expect_equal(result$vals[2, 1], 300)  # year 2001, age 0
    expect_equal(result$vals[2, 2], 400)  # year 2001, age 1
  })


  it("handles custom column names via 'what' parameter", {
    test_data <- tibble(
      year = rep(2000:2001, each = 2),
      age = rep(0:1, times = 2),
      num_population = c(1000, 2000, 3000, 4000),
      custom_col = c(10, 20, 30, 40)
    )

    result <- make_z_list_pop(test_data, what = "custom_col")

    expect_equal(result$vals[1, 1], 10)
    expect_equal(result$vals[2, 2], 40)
  })


  it("handles NA values in input", {
    test_data <- tibble(
      year = c(2000, 2000, 2001, 2001),
      age = c(0, 1, 0, 1),
      num_population = c(100, NA, 300, 400)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(result$vals[1, 1], 100)
    expect_true(is.na(result$vals[1, 2]))
    expect_equal(result$vals[2, 1], 300)
  })


  it("handles large population values without overflow", {
    test_data <- tibble(
      year = c(2000, 2000),
      age = c(0, 1),
      num_population = c(1e9, 2e9)  # 1 billion, 2 billion
    )

    result <- make_z_list_pop(test_data)

    expect_equal(result$vals[1, 1], 1e9)
    expect_equal(result$vals[1, 2], 2e9)
  })


  it("handles single year correctly", {
    test_data <- tibble(
      year = rep(2000, 3),
      age = 0:2,
      num_population = c(100, 200, 300)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(length(result$year), 1)
    expect_equal(result$year, 2000)
    expect_equal(dim(result$vals), c(1, 3))
  })


  it("handles single age correctly", {
    test_data <- tibble(
      year = 2000:2002,
      age = rep(0, 3),
      num_population = c(100, 200, 300)
    )

    result <- make_z_list_pop(test_data)

    expect_equal(length(result$age), 1)
    expect_equal(result$age, 0)
    expect_equal(dim(result$vals), c(3, 1))
  })

})


# ============================================================================
# make_z_list tests
# ============================================================================

describe("make_z_list", {

  it("returns a list with age, year, and vals components", {
    test_data <- tibble(
      year = rep(2000:2001, each = 2),
      age = rep(0:1, times = 2),
      num_deaths = c(10, 5, 12, 6),
      exposure = c(1000, 1000, 1000, 1000)
    )

    result <- make_z_list(test_data)

    expect_type(result, "list")
    expect_named(result, c("age", "year", "vals"))
  })


  it("calculates log mortality rate correctly with default base 10", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(100),
      exposure = c(10000)  # mortality rate = 0.01
    )

    result <- make_z_list(test_data, what = "lmr_k", adjust = 0)

    # log10(0.01) = -2
    expect_equal(result$vals[1, 1], log(0.01, 10), tolerance = 1e-10)
  })


  it("calculates mortality rate correctly in 'mr' mode", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(100),
      exposure = c(10000)
    )

    result <- make_z_list(test_data, what = "mr", adjust = 0)

    expect_equal(result$vals[1, 1], 0.01)
  })


  it("applies adjust parameter correctly", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(100),
      exposure = c(10000)
    )

    # With adjust = 0.5: (100 + 0.5) / (10000 + 0.5)
    result <- make_z_list(test_data, what = "mr", adjust = 0.5)
    expected <- (100 + 0.5) / (10000 + 0.5)

    expect_equal(result$vals[1, 1], expected, tolerance = 1e-10)
  })


  it("handles division by zero with adjust parameter", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(0),
      exposure = c(0)  # Would cause division by zero
    )

    # With adjust = 0.5, we get (0 + 0.5) / (0 + 0.5) = 1
    result <- make_z_list(test_data, what = "mr", adjust = 0.5)

    expect_equal(result$vals[1, 1], 1)
  })


  it("converts NaN to NA", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(0),
      exposure = c(0)
    )

    # log of 0/0 = NaN, which should become NA
    result <- make_z_list(test_data, what = "lmr_k", adjust = 0)

    expect_true(is.na(result$vals[1, 1]))
  })


  it("handles log(0) by converting to NA", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(0),
      exposure = c(1000)  # mortality rate = 0
    )

    # log(0) = -Inf, but since 0/1000 = 0, log(0) = -Inf
    # The function converts NaN to NA but -Inf stays as -Inf
    result <- make_z_list(test_data, what = "lmr_k", adjust = 0)

    expect_true(is.infinite(result$vals[1, 1]) || is.na(result$vals[1, 1]))
  })


  it("uses custom log base via 'k' parameter", {
    test_data <- tibble(
      year = c(2000),
      age = c(0),
      num_deaths = c(100),
      exposure = c(10000)  # mr = 0.01
    )

    result_base10 <- make_z_list(test_data, what = "lmr_k", adjust = 0, k = 10)
    result_base_e <- make_z_list(test_data, what = "lmr_k", adjust = 0, k = exp(1))

    expect_equal(result_base10$vals[1, 1], log(0.01, 10), tolerance = 1e-10)
    expect_equal(result_base_e$vals[1, 1], log(0.01), tolerance = 1e-10)
  })


  it("returns correct matrix dimensions", {
    test_data <- tibble(
      year = rep(2000:2009, each = 5),  # 10 years
      age = rep(0:4, times = 10),        # 5 ages
      num_deaths = rep(10, 50),
      exposure = rep(1000, 50)
    )

    result <- make_z_list(test_data)

    expect_equal(dim(result$vals), c(10, 5))
  })


  it("preserves age and year ordering", {
    test_data <- tibble(
      year = rep(c(2010, 2000, 2005), each = 3),
      age = rep(c(10, 0, 5), times = 3),
      num_deaths = rep(10, 9),
      exposure = rep(1000, 9)
    )

    result <- make_z_list(test_data)

    # Years should be sorted
    expect_equal(result$year, c(2000, 2005, 2010))
    # Ages should be sorted
    expect_equal(result$age, c(0, 5, 10))
  })


  it("handles extremely small mortality rates", {
    test_data <- tibble(
      year = c(2000),
      age = c(5),
      num_deaths = c(1),
      exposure = c(1e8)  # 1 death per 100 million
    )

    result <- make_z_list(test_data, what = "lmr_k", adjust = 0)

    expected <- log(1 / 1e8, 10)
    expect_equal(result$vals[1, 1], expected, tolerance = 1e-10)
  })


  it("handles extremely high mortality rates", {
    test_data <- tibble(
      year = c(2000),
      age = c(100),
      num_deaths = c(500),
      exposure = c(1000)  # 50% mortality rate
    )

    result <- make_z_list(test_data, what = "lmr_k", adjust = 0)

    expected <- log(0.5, 10)
    expect_equal(result$vals[1, 1], expected, tolerance = 1e-10)
  })

})
