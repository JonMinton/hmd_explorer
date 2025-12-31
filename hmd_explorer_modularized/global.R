# Global configuration for HMD Explorer (Modularized)

# Load packages
suppressPackageStartupMessages({
  library(magrittr)
  library(tidyverse)
  library(plotly)
  library(shiny)
  library(RColorBrewer)
  library(viridis)
})

# Source helper functions
source("R/data_transforms.R")

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

# Color palette definitions
all_colbrew_pals <- brewer.pal.info %>%
  as_tibble(rownames = "pal_name") %>%
  pull(pal_name) %>%
  str_c("RColorBrewer:", .)

viridis_pals <- c(
  "Viridis:viridis",
  "Viridis:magma",
  "Viridis:plasma",
  "Viridis:inferno",
  "Viridis:cividis"
)

all_palettes <- c(viridis_pals, all_colbrew_pals)
