# Global configuration for HMD Explorer (Modularized)

# Load packages
suppressPackageStartupMessages({
  library(magrittr)
  library(tidyverse)
  library(plotly)
  library(shiny)
  library(bslib)
  library(RColorBrewer)
  library(viridis)
  library(scales)
})

# Source utility functions (analogous to Python utils/ folder)
source("R/utils/color_scales.R")
source("R/utils/make_z_list.R")
source("R/utils/extract_subplot_data.R")
source("R/utils/make_surface_plot.R")
source("R/utils/calc_lifetable.R")

# Source modules
source("module_colscheme_selector.R")
source("module_select_data.R")
source("module_make_graphics.R")
source("module_make_corrmaps.R")
source("module_lifetable.R")

# Load data
full_data <- read_csv("data/hmd_data.csv", show_col_types = FALSE)
names(full_data) <- tolower(names(full_data))

codes_named <- read_rds("data/codes_named.rds")

# Load life expectancy data if available
hmd_e0_path <- "data/hmd_e0.csv"
if (file.exists(hmd_e0_path)) {
  hmd_e0 <- read_csv(hmd_e0_path, show_col_types = FALSE)
  names(hmd_e0) <- tolower(names(hmd_e0))
  hmd_e0 <- hmd_e0 %>%
    mutate(gender = tolower(gender))
}

# Get available palettes
all_colbrew_pals <- get_all_palettes()
