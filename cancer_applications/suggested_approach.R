# Here's some suggested code for turning data where the age is grouped in (say) 5 year categories, but the year is in individual years

# Basic idea: create an age_group -> age lookup table, then perform a join on the data 

# Example: (using tidyverse packages)

lookup_table <- tibble(
  age = 0:90
  ) %>%
  mutate(
    age_group = case_when(
       age == 0  ~ '0yrs', # replacement is after the ~ sign
       age %in% 1:4 ~ "1to4yrs", # make sure labels match those in the cancer data exactly (no typos)
       age %in% 5:9 ~ "5to9yrs",
       
       # etc etc
       
       TRUE ~ NA_character_ # This catches any values that haven't been matched to earlier conditions
    )
  )
  
table_1x1 <- data_table %>% 
  right_join(lookup_table, by = c("agegrp" = "age_group")) # left is left table join column , right is right table join column 
