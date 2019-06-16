


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
source("module_make_corrmaps.R")

ui <- fluidPage(
  titlePanel("Human Mortality Database Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      select_data_ui("data_module1_singular"),
      
      verbatimTextOutput("tab_active")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", id = "main_tabset", 
        tabPanel("individual",title = "Individual Population",
          make_graphics_ui("graphics_module_singular-singular")         
                 ),
        tabPanel("group", title = "Group of Populations",
          make_graphics_ui("graphics_module_group-singular")       
                 ),
        tabPanel("sex_compare", title = "Comparison between Genders",
          make_graphics_ui("graphics_module_singular-comparative")       
                 ),
        tabPanel("group_compare", title = "Comparison between Populations",
          make_graphics_ui("graphics_module_group-comparative")       
                 ),
        tabPanel("corr_individual", title = "Correlation in trends by age",
                 make_corrmaps_ui("corrmaps_singular-singular")       
        )
        
      )
    )
  )
)

server <- function(input, output) {
  get_active_tab <- reactive({input$main_tabset})
  
  data1_singular <- callModule(select_data_server, "data_module1_singular")

  
  callModule(make_graphics_server, "graphics_module_singular-singular", 
             mode = "singular-singular", data = data1_singular)
  callModule(make_graphics_server, "graphics_module_group-singular", 
             mode = "group-singular", data = data1_singular)
  callModule(make_graphics_server, "graphics_module_singular-comparative", 
             mode = "singular-comparative", data = data1_singular)
  callModule(make_graphics_server, "graphics_module_group-comparative", 
             mode = "group-comparative", data = data1_singular)
  
  callModule(make_corrmaps_server, "corrmaps_singular-singular", 
             data = data1_singular)
  
  
}




shinyApp(ui, server)