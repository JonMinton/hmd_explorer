# Utility: Life table calculations
# Implements HMD methodology for q_x calculation

#' Calculate q_x (probability of dying between age x and x+1)
#' Uses HMD methodology with appropriate a_x values
#' @param age Age in years
#' @param sex Sex ("Male" or "Female")
#' @param deaths Number of deaths
#' @param exposure Person-years of exposure
#' @return q_x value
calc_qx <- function(age, sex, deaths, exposure) {
  m_x <- deaths / exposure

  # Calculate a_x (average fraction of year lived by those dying)
  a_x <- if (age == 0) {
    if (m_x > 0.107) {
      if (sex == "Female") 0.350 else 0.330
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

  # Calculate q_x using the relationship with m_x
  q_x <- m_x / (1 + (1 - a_x) * m_x)

  return(q_x)
}


#' Calculate survivors from initial cohort
#' @param x Data frame with p_x column
#' @param init_size Initial cohort size (default 50000)
#' @return Data frame with cohort_size column added
calculate_survivors <- function(x, init_size = 50000) {
  k <- nrow(x)
  cohort_size <- numeric(k)
  cohort_size[1] <- init_size * x$p_x[1]

  for (i in 2:k) {
    cohort_size[i] <- cohort_size[i - 1] * x$p_x[i]
  }

  x %>%
    mutate(cohort_size = cohort_size)
}


#' Calculate full life table from mortality data
#' @param data Data frame with age, gender, num_deaths, exposure columns
#' @return Data frame with life table columns (q_x, p_x, l_x, etc.)
calc_lifetable <- function(data) {
  data %>%
    mutate(
      q_x = pmap_dbl(
        .l = list(
          age = age,
          sex = gender,
          deaths = num_deaths,
          exposure = exposure
        ),
        .f = possibly(calc_qx, otherwise = NA_real_)
      ),
      p_x = 1 - q_x,
      log_q_x = log(q_x, 10)
    )
}
