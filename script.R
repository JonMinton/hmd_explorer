library(tidyverse)
library(plotly)
library(HMDHFDplus)

getHMDcountries()
getHMDcountries()

username <- "jon.will.minton@gmail.com"
password <- "passphrase"

hmd_dta <- data_frame(
  code = getHMDcountries()
) %>% 
  mutate(deaths = map(code, readHMDweb, item = "Deaths_1x1", username = username, password = password)) %>% 
  mutate(population = map(code, readHMDweb, item = "Population", username = username, password = password)) %>% 
  mutate(exposures = map(code, readHMDweb, item = "Exposures_1x1", username = username, password = password))

hmd_dta %>% 
  mutate(deaths = map(
    deaths, 
    function(x) {
      x %>% 
        select(-OpenInterval) %>% 
        gather(Female:Total, key = "gender", value = "num_deaths")
      }
    )
  ) %>% 
  mutate(population = map(
    population,
    function(x) {
      x %>% 
        mutate(Female = ( Female1 + Female2 ) / 2) %>% 
        mutate(Male = (Male1 + Male2) / 2) %>% 
        mutate(Total = (Total1 + Total2) / 2) %>% 
        select(Year, Age, Female, Male, Total) %>% 
        gather(Female:Total, key = "gender", value = "num_population")
    }
    )
  ) %>% 
  mutate(exposures = map(
    exposures,
    function(x) {
      x %>% 
        select(-OpenInterval) %>% 
        gather(Female:Total, key = "gender", value = "exposure")
    }
  )) %>% 
  mutate(
    dta_joined = pmap(
      list(deaths, population, exposures), 
      function(a, b, c) {
        reduce(list(a, b, c), inner_join)
      })
  ) %>% 
  select(code, dta_joined) %>% 
  unnest() -> joined_data

joined_data %>% write_csv("hmd_explorer/data/hmd_data.csv")


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
  filter(Age <= 90) %>% 
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

# from https://plot.ly/r/heatmaps/ 

vals <- unique(scales::rescale(c(volcano)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
p <- plot_ly(z = volcano, colorscale = colz, type = "heatmap")

p

plot_ly(z = volcano, colorscale = colz) %>% add_surface()

lmr_data %>% 
  filter(code == "GBR_SCO") %>% 
  filter(gender == "Total") %>% 
  .[["lmr_list"]] %>% 
  pluck(1) %>% 
  pluck("vals") -> vals 


o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)

colz <- setNames(data.frame(vals[o], cols[o]), NULL)
plot_ly(z = vals, colorscale = colz) %>% 
  add_surface()

cols <- rainbow(100)
breakpoints <- scales::rescale(10^seq(0, 1, length = 100))

plot_ly(z = 10^vals, surfacecolor = vals) %>% 
  add_surface(
#    surfaceaxis = 2
    #colors = ~scales::rescale(vals)
#    surfacecolor = rgb(0.5, 0.5, 0.5)
    # colorscale = list(
    #   # c(0, 1),
    #   # c(rgb(0,0,0), rgb(1,1,1))
    #   breakpoints,
    #   cols 
    # ),
    # cauto = F,
    # cmin = 0, cmax = 1
    
    ) %>% 
  layout(
    scene = list(
      zaxis = list(
        title = "Mort",
        type = "log"
      )
    )
  ) -> p

plotly_json(p)


#####
x <- seq(-3, 3, by = 0.1)
y <- 10^x

plot_ly(x = ~x, y = ~y) %>% 
  add_lines(
  ) %>% 
  layout(
    yaxis = list(
      type = "log",
      range = c(1, 2)
                 )
  )
  

