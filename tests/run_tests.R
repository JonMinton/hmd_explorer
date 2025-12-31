# HMD Explorer Test Runner
# Run this script to execute all tests
#
# Usage:
#   source("tests/run_tests.R")
#
# Or from command line:
#   Rscript tests/run_tests.R

# Set working directory to project root if needed
if (!file.exists("tests/testthat")) {
  if (file.exists("testthat")) {
    setwd("..")
  }
}

# Load required packages
if (!require("testthat", quietly = TRUE)) {
  install.packages("testthat")
  library(testthat)
}

if (!require("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Source the setup file to get helper functions
source("tests/testthat/setup.R")

# Run all tests
cat("\n========================================\n")
cat("Running HMD Explorer Test Suite\n")
cat("========================================\n\n")

# Run tests with reporter
results <- test_dir(
  "tests/testthat",
  reporter = "summary",
  stop_on_failure = FALSE
)

# Print summary
cat("\n========================================\n")
cat("Test Summary\n")
cat("========================================\n")

# Return results for programmatic use
invisible(results)
