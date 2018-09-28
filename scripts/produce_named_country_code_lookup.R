require(tidyverse)

codes_hmd_available <- HMDHFDplus::getHMDcountries()

code_lookup <- read_csv("hmd_explorer/data/country_codes_lookup.csv")

codes_hmd_available

code_lookup <- code_lookup %>% 
  filter(code %in% codes_hmd_available) 
  
codes_named <- code_lookup %>% pull(code)
names(codes_named) <- code_lookup %>% pull(name)

write_rds(codes_named, "hmd_explorer/data/codes_named.rds")
