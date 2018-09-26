#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(HMDHFDplus)
library(shiny)

codes <- HMDHFDplus::getHMDcountries()

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Human Mortality Database Explorer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("code_select",
                   "Select gender of interest",
                   choices = codes,
                   selected = "GBR_SCO",
                   multiple = FALSE),
       selectInput("gender_select",
                   "Select gender of interest",
                   choices = c("Male", "Female", "Total"),
                   selected = "Total")
       
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("mort_surface")
      
    )
  )
))
