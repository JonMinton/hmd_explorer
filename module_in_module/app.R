#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

outer_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    numericInput(ns("num_outer1"), min = 0, max = 5, value = 3,label = "Slide!"),
    textOutput(ns("out_from_user")),
    inner_ui(ns("inner_instance"))
    
  )
}

inner_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    actionButton(ns("inner_action"), label = "action"),
    textOutput(ns("inner_response")),
    textOutput(ns("twice_outer"))
    
  )
}

outer_server <- function(input, output, session){
  get_multiplier_input <- reactive({input$num_outer1})
  
  callModule(inner_server, "inner_instance", val_from_outer = get_multiplier_input)
  
  output$out_from_user <- renderText({get_multiplier_input()})

  
}

inner_server <- function(input, output, session, val_from_outer){
  check_if_action <- reactive({input$inner_action})
  output$inner_response <- renderText({check_if_action()})
  output$twice_outer    <- renderText({
    2 * val_from_outer()
  })
  
  
}


# Define UI for application that draws a histogram
ui <- fluidPage(

     outer_ui("outer_instance")
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  callModule(outer_server, "outer_instance")  
}

# Run the application 
shinyApp(ui = ui, server = server)


