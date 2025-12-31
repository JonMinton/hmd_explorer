# Test runner for HMD Explorer
# Run with: testthat::test_dir("tests/testthat")

library(testthat)
library(tidyverse)

# Source the functions to test
test_check("hmd_explorer")
