
init_cohort_size <- 50000 # 50 000 as males and females
# need to be combined
calculate_survivors_properly <- function(x, init_size = init_cohort_size){
  k <- dim(x)[1]
  cohort_size <- rep(NA, k)
  cohort_size[1] <- init_size * x$p_x[1]
  for (i in 2:k){
    cohort_size[i] <- cohort_size[i - 1] * x$p_x[i]
  }
  output <- data.frame(x, cohort_size = cohort_size)
}





calc_qx <- function(age, sex, deaths, exposure){
  m_x <-  deaths / exposure
  a_x <-  if(age == 0)
  {
    if(m_x > 0.107) 
    {
      if (sex == "Female")
      {
        0.350
      } else 
      {
        0.330  
      }
      
    } else 
    {
      if (sex == "Female") 
      {
        0.053 + 2.800 * m_x
      } else 
      {
        0.045 + 2.684 * m_x
      }
    }
  } else 
  {
    0.5
  }
  
  q_x = m_x / (1 + (1 - a_x) * m_x)
  
}



# 
# dta_synth <- dta_both %>% 
#   mutate(
#     q_x = pmap_dbl(
#       .l = list(
#         age = age,
#         sex = sex,
#         deaths = deaths, 
#         exposure = exposure
#       ), 
#       .f = possibly(calc_qx, otherwise = NA)
#     ),
#     p_x = 1 - q_x
#   ) %>% 
#   arrange(country, year, sex, age) %>%
#   group_by(country, year, sex) %>% 
#   do(calculate_survivors_properly(.)) %>% 
#   mutate(cumulative_deaths = init_cohort_size - cohort_size)
# 
# 

# proper_counts <- proper_counts %>% 
#   mutate(
#     q_x = pmap_dbl(
#       .l = list(
#         age = age,
#         sex = sex,
#         deaths = deaths, 
#         exposure = exposure
#       ), 
#       .f = possibly(calc_qx, otherwise = NA)
#     ),
#     p_x = 1 - q_x
#   )
# 

