# This script is designed to house modules for the hmd explorer app. 

# Eventually the app will also transition to a single, app.R, file from the current 
# two file configuration (ui.R and global.R)

library(plotly)
library(tidyverse)

library(shiny)

codes_named <- read_rds("data/codes_named.rds")


sideBarUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    conditionalPanel("input.tabset_1 !='Mortality Group Comparisons' & input.tabset_1 != 'Tadpole Charts'", ns = ns,
                     selectInput(ns("code_select"),
                                 "Select country of interest",
                                 choices = codes_named,
                                 selected = "GBR_SCO",
                                 multiple = FALSE)
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     selectInput(ns("group_mode"), "Show populations or population differences",
                                 choices = c("Differences in log mortality" = 'diff',
                                             "Both populations individually" = 'both'),
                                 selected = "diff",
                                 multiple = FALSE
                     )                
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     selectInput(ns("multi_code_select_B"),
                                 "Select populations of interest",
                                 choices = codes_named,
                                 selected = "GBR_SCO",
                                 multiple = TRUE)                 
    ),
    
    
    conditionalPanel("input.tabset_1 == 'Tadpole Charts'", ns = ns,
                     selectInput(ns("tadpole_highlight"),
                                 "Select population to focus on",
                                 choices = codes_named,
                                 selected = "GBR_SCO"
                     )                       
    ),
    conditionalPanel("input.tabset_1 == 'Tadpole Charts'", ns = ns,
                     selectInput(ns("tadpole_group"),
                                 "Select populations to include",
                                 choices = codes_named, multiple = TRUE, 
                                 selected = c("GBRTENW", "GBR_SCO", "GBR_NIR")
                     )                      
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     selectInput(ns("multi_code_select_A"),
                                 "Select comparator populations of interest",
                                 choices = codes_named,
                                 selected = "GBRTENW",
                                 multiple = TRUE)                 
    ),
    
    conditionalPanel("input.group_mode == 'both'", ns = ns,
                     sliderInput(ns("group_B_alpha"), "Select transparency of top pop list",
                                 value = 0.7,
                                 min = 0, max = 1, step = 0.1
                     )                
    ),
    
    conditionalPanel("input.group_mode == 'both'", ns = ns,
                     sliderInput(ns("group_A_alpha"), "Select transparency of bottom pop list",
                                 value = 0.7,
                                 min = 0, max = 1, step = 0.1
                     )                
    ),
    
    conditionalPanel("input.tabset_1=='Mortality' || input.tabset_1=='Population' || input.tabset_1=='Mortality Group Comparisons'", ns = ns,
                     selectInput(ns("gender_select"),
                                 "Select gender of interest",
                                 choices = c("Male", "Female", "Total"),
                                 selected = "Total")
    ),
    conditionalPanel("input.tabset_1=='Lifetable' ", ns = ns,
                     selectInput(ns("gender_select_nototal"),
                                 "Select gender of interest",
                                 choices = c("Male", "Female"),
                                 selected = "Female")
    ),
    
    conditionalPanel("input.tabset_1 != 'Tadpole Charts'", ns = ns,
                     checkboxInput(ns("limit_age"),
                                   "Check to limit ages",
                                   value = F
                     )
    ),
    conditionalPanel("input.limit_age == true", ns = ns,
                     sliderInput(ns("age_limits"),
                                 "Select age limit range",
                                 min = 0, max = 110, step = 1, 
                                 value = c(0, 110))              
    ),
    checkboxInput(ns("limit_period"), 
                  "Check to limit year range",
                  value = F
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     checkboxInput(ns("limit_diffz"),
                                   "Check to set range for differences shown",
                                   value = F)
    ),
    conditionalPanel("input.limit_diffz == true", ns = ns,
                     sliderInput(ns("diffz_limits"),
                                 "Select range of differences to display",
                                 min = -4, max = 4, step = 0.1,
                                 value = c(-3, 3))
    ),
    conditionalPanel("input.limit_period == true", ns = ns,
                     sliderInput(ns("period_limits"), sep = "",
                                 "Select range of years",
                                 min = 1750, max = 2020, step = 1, 
                                 value = c(1750, 2020))              
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Sex Ratios' || input.tabset_1 == 'Population Sex Ratios'", ns = ns,
                     sliderInput(ns("ratio_limiter"), 
                                 "Select ratio limiter",
                                 min = 1, max = 10, step = 0.5,
                                 value = 3
                     )              
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Sex Ratios' || input.tabset_1 == 'Population Sex Ratios' || input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     numericInput(ns("small_n_correction"), 
                                  "Add small value to cells",
                                  min = 0, value = 50, max = 10000
                     )              
    ),
    conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'", ns = ns,
                     actionButton(ns("recalc"), "Click to recalculate with new selection")          
    )
  )

}