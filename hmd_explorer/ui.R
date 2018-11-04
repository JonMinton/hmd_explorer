#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(plotly)
library(tidyverse)

library(shiny)

codes_named <- read_rds("data/codes_named.rds")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Human Mortality Database Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("input.tabset_1 !='Mortality Group Comparisons' & input.tabset_1 != 'Tadpole Charts'",
                       selectInput("code_select",
                                   "Select country of interest",
                                   choices = codes_named,
                                   selected = "GBR_SCO",
                                   multiple = FALSE)
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'",
                       selectInput("group_mode", "Show populations or population differences",
                          choices = c("Differences in log mortality" = 'diff',
                                      "Both populations individually" = 'both'),
                          selected = "diff",
                          multiple = FALSE
                                   )                
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'",
                       selectInput("multi_code_select_B",
                                   "Select populations of interest",
                                   choices = codes_named,
                                   selected = "GBR_SCO",
                                   multiple = TRUE)                 
      ),
      
      
      conditionalPanel("input.tabset_1 == 'Tadpole Charts'",
                       selectInput("tadpole_highlight",
                                   "Select population to focus on",
                                   choices = codes_named,
                                   selected = "GBR_SCO"
                       )                       
      ),
      conditionalPanel("input.tabset_1 == 'Tadpole Charts'",
                       selectInput("tadpole_group",
                                   "Select populations to include",
                                   choices = codes_named, multiple = TRUE, 
                                   selected = c("GBRTENW", "GBR_SCO", "GBR_NIR")
                       )                      
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'",
                       selectInput("multi_code_select_A",
                                   "Select comparator populations of interest",
                                   choices = codes_named,
                                   selected = "GBRTENW",
                                   multiple = TRUE)                 
      ),
      
      conditionalPanel("input.group_mode == 'both'",
                       sliderInput("group_B_alpha", "Select transparency of top pop list",
                          value = 0.7,
                          min = 0, max = 1, step = 0.1
                       )                
      ),

            conditionalPanel("input.group_mode == 'both'",
                       sliderInput("group_A_alpha", "Select transparency of bottom pop list",
                                   value = 0.7,
                                   min = 0, max = 1, step = 0.1
                       )                
      ),
      
      conditionalPanel("input.tabset_1=='Mortality' || input.tabset_1=='Population' || input.tabset_1=='Mortality Group Comparisons'",
                       selectInput("gender_select",
                                   "Select gender of interest",
                                   choices = c("Male", "Female", "Total"),
                                   selected = "Total")
      ),
      conditionalPanel("input.tabset_1 != 'Tadpole Charts'",
                       checkboxInput("limit_age",
                                     "Check to limit ages",
                                     value = F
                       )
      ),
      conditionalPanel("input.limit_age == true",
                       sliderInput("age_limits",
                                   "Select age limit range",
                                   min = 0, max = 110, step = 1, 
                                   value = c(0, 110))              
      ),
      checkboxInput("limit_period",
                    "Check to limit year range",
                    value = F
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'",
                       checkboxInput("limit_diffz",
                                     "Check to set range for differences shown",
                                     value = F)
      ),
      conditionalPanel("input.limit_diffz == true",
                       sliderInput("diffz_limits",
                                   "Select range of differences to display",
                                   min = -4, max = 4, step = 0.1,
                                   value = c(-3, 3))
      ),
      conditionalPanel("input.limit_period == true",
                       sliderInput("period_limits", sep = "",
                                   "Select range of years",
                                   min = 1750, max = 2020, step = 1, 
                                   value = c(1750, 2020))              
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Sex Ratios' || input.tabset_1 == 'Population Sex Ratios'",
                       sliderInput("ratio_limiter", 
                                   "Select ratio limiter",
                                   min = 1, max = 10, step = 0.5,
                                   value = 3
                       )              
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Sex Ratios' || input.tabset_1 == 'Population Sex Ratios' || input.tabset_1 == 'Mortality Group Comparisons'",
                       numericInput("small_n_correction", 
                                    "Add small value to cells",
                                    min = 0, value = 50, max = 10000
                       )              
      ),
      conditionalPanel("input.tabset_1 == 'Mortality Group Comparisons'",
                       actionButton("recalc", "Click to recalculate with new selection")                
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabset_1", type = "tab",
                  tabPanel(title = "Mortality",
                           plotlyOutput("mort_surface"),
                           plotlyOutput("mort_subplot")
                           
                  ),
                  tabPanel(title = "Population",
                           plotlyOutput("pop_surface"),
                           plotlyOutput("pop_subplot")
                  ),
                  tabPanel(title = "Mortality Sex Ratios",
                           plotlyOutput("mort_ratio_surface")
                  ),
                  tabPanel(title = "Population Sex Ratios",
                           plotlyOutput("pop_ratio_surface")
                  ),
                  tabPanel(
                    title = "Mortality Group Comparisons",
                    plotlyOutput("mort_group_surface"),
                    plotlyOutput("mort_group_subplot")
                  )#,
                  # tabPanel(
                  #   title = "Tadpole Charts",
                  #   plotlyOutput("tadpole_plot")
                  #   
                  # )
      )
    )
  ))
)