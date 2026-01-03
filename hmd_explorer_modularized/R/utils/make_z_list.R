# Utility: Convert data frame to matrix format for plotly surfaces
# Analogous to Extract3dDataArrays.py in Python version

#' Convert mortality/population data to matrix format for 3D plotting
#' @param X Data frame with year, age, num_deaths, exposure columns
#' @param what Output type: "lmr_k" for log mortality, "mr" for mortality rate
#' @param adjust Continuity correction value (default 0)
#' @param k Log base (default 10)
#' @return List with age vector, year vector, and value matrix
make_z_list <- function(X, what = "lmr_k", adjust = 0, k = 10) {
  tmp <- X %>%
    select(year = year, age = age, n = num_deaths, N = exposure)

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
    pivot_wider(names_from = age, values_from = val) -> tmp_wide

  years <- tmp_wide$year
  tmp_wide$year <- NULL
  ages <- as.numeric(names(tmp_wide))
  val_mtrx <- as.matrix(tmp_wide)

  list(age = ages, year = years, vals = val_mtrx)
}


#' Convert population data to matrix format for 3D plotting
#' @param X Data frame with year, age columns and value column
#' @param what Column name to extract (default "num_population")
#' @return List with age vector, year vector, and value matrix
make_z_list_pop <- function(X, what = "num_population") {
  out_df <- X %>%
    select(year = year, age = age, val = !!sym(what))

  out_df %>%
    select(year, age, val) %>%
    pivot_wider(names_from = age, values_from = val) -> tmp_wide

  years <- tmp_wide$year
  tmp_wide$year <- NULL
  ages <- as.numeric(names(tmp_wide))
  val_mtrx <- as.matrix(tmp_wide)

  list(age = ages, year = years, vals = val_mtrx)
}
