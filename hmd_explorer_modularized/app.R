


library(shiny)
library(tidyverse)
library(viridis)
library(plotly)


### Data 

codes_named <- read_rds("data/codes_named.rds")

#data_all <- read_csv("data/hmd_data.csv")
#names(data_all) <- tolower(data_all)

# Source

source("module_select_data.R")
source("module_make_graphics.R")

ui <- fluidPage(
  titlePanel("Human Mortality Database Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      select_data_ui("data_module")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      make_graphics_ui("graphics_module")
    )
  )
)

server <- function(input, output) {
  data_ss <- callModule(select_data_server, "data_module")
  output$dim_of_dta <- renderText({
    paste("The selection has", dim(data_ss())[1], "rows and", dim(data_ss())[2], "columns")
  })
  
  callModule(make_graphics_server, "graphics_module", data = data_ss)
}




shinyApp(ui, server)