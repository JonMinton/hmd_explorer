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
       selectInput("code_select",
                   "Select country of interest",
                   choices = codes_named,
                   selected = "GBR_SCO",
                   multiple = FALSE),
       selectInput("gender_select",
                   "Select gender of interest",
                   choices = c("Male", "Female", "Total"),
                   selected = "Total"),
       checkboxInput("limit_age",
                     "Check to limit ages",
                     value = F
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
       conditionalPanel("input.limit_period == true",
                        sliderInput("period_limits", sep = "",
                                    "Select range of years",
                                    min = 1750, max = 2020, step = 1, 
                                    value = c(1750, 2020))              
       )
       
       
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "tabset_01",
        tabPanel(title = "Mortality", 
        {
          plotlyOutput("mort_surface")
          plotlyOutput("mort_subplot")
        }
        )
                   
      )
    
    )
  )
))
