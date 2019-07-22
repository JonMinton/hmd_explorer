# Global file 

pacman::p_load(
  magrittr, 
  tidyverse,
  plotly,
  shiny,
  RColorBrewer,
  viridis
)

# Sources

source("module_make_corrmaps.R")
source("module_colscheme_selector.R")
source("module_make_graphics.R")
source("module_select_data.R")

# Data
full_data <- read_csv("data/hmd_data.csv")

codes_named <- read_rds("data/codes_named.rds")

all_colbrew_pals <- brewer.pal.info %>% 
  as_tibble(rownames = "pal_name") %>% 
  pull(pal_name) %>% 
  str_c("RColorBrewer", ":", .)


# Functions


define_colbrew_pals <- function(category, colorblind){
  cat(file=stderr(), "define_colbrew_pals()\n")
  out <- RColorBrewer %>% 
    as_tibble(rownames = "pal_name") 
  
  if (!is.null(category)) {
    out <- out %>% filter(category == category)
  }
  if (!is.null(colorblind)) {
    out <- out %>% filter(colorblind == colorblind)
  }
  
  out
}



create_colbrew_ramp <- function(palname){
  max_n <- brewer.pal.info %>% 
    as_tibble(rownames = "pal_name") %>% 
    filter(pal_name == palname) %>% 
    pull(maxcolors)
  
  colorRampPalette(brewer.pal(name = palname, n = max_n))
}
