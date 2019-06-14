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
      sideBarUI("first")
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
                           plotlyOutput("pop_ratio_surface"),
                           plotlyOutput("pop_ratio_subplot")
                  ),
                  tabPanel(
                    title = "Mortality Group Comparisons",
                    plotlyOutput("mort_group_surface"),
                    plotlyOutput("mort_group_subplot")
                  ), 
                  tabPanel(
                    title = "Lifetable",
                    plotOutput("lifetable_surface")
                  )
                  
                  #,
                  # tabPanel(
                  #   title = "Tadpole Charts",
                  #   plotlyOutput("tadpole_plot")
                  #   
                  # )
      )
    )
  ))
)