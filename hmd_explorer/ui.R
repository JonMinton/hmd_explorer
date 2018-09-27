#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(HMDHFDplus)
library(tidyverse)
library(shiny)
library(plotly)

#codes_available <- HMDHFDplus::getHMDcountries()
codes_available <- read_rds(path = "data/codes_available.rds")

code_lookup <- read_csv("data/country_code_lookup.csv")
code_lookup <- code_lookup %>% 
  filter(country_code %in% codes_available)

named_codes <- pull(code_lookup, country_code)
names(named_codes) <- pull(code_lookup, country_name)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Human Mortality Database Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("code_select",
                   "Select country of interest",
                   choices = named_codes,
                   selected = "GBR_SCO",
                   multiple = FALSE),
       selectInput("gender_select",
                   "Select gender of interest",
                   choices = c("Male", "Female", "Total"),
                   selected = "Total")
       
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Mortality", 
          {
            plotlyOutput("mort_surface")
            plotlyOutput("subplots")
#            verbatimTextOutput("selection")
#            verbatimTextOutput("input_checker")
          }
        #     #   ),
        #     # fluidRow(
        #     #   column(12,
        #              plotlyOutput("subplots")
        #     #   )
        #     # )
        #   )
        # # }
      ), 
        tabPanel("Population",
            plotlyOutput("pop_surface")                 
        )

      )
    )
  )
))