# Global file 

pacman::p_load(
  tidyverse,
  plotly,
  shiny,
  RColorBrewer
)

source("module_make_corrmaps.R")
source("module_colscheme_selector.R")
source("module_make_graphics.R")
source("module_select_data.R")


full_data <- read_csv("data/hmd_data.csv")

codes_named <- read_rds("data/codes_named.rds")