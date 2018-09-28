

make_z_list <- function(X, what = "lmr_k", adjust = 0, k = 10){
  tmp <- X %>% 
    select(year = Year, age = Age, n = num_deaths, N = exposure) 
  
  if(what == "lmr_k"){
    out_df <- tmp %>% 
    mutate(
      mr = (n + adjust) / (N+adjust), val = log(mr, k)
    ) %>% 
      mutate(val = ifelse(is.nan(val), NA, val))
  } else if (what == "mr") {
    out_df <- tmp %>% 
      mutate(val = (n + adjust) / (N+adjust))
  }
    
  out_df %>% 
    select(year, age, val) %>% 
    spread(age, val) -> tmp
  
  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}

joined_data %>% 
  group_by(code, gender) %>% 
  nest() %>% 
  mutate(lmr_list = map(data, make_z_list, adjust = 0.5)) %>% 
  select(code, gender, lmr_list) -> lmr_data


joined_data %>% 
  filter(code == "GBR_SCO") %>% 
  mutate(mr = num_deaths / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  plot_ly() %>% 
  add_heatmap(x = ~Age, y = ~Year, z = ~lmr)