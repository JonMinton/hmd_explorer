

library(shiny)

ui <- fluidPage(
    titlePanel("Conditional panel test"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("the_choice", label = "Make a choice",
                      choices = c("Daddy", "Chips"), selected = "Daddy"
        ),
        textOutput("the_chosen")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
    )
  )

server <- function(input, output) {
  chosen_option <- reactive({input$the_choice})
  
  output$the_chosen <- renderText({paste("The choice is", chosen_option())})
}


shinyApp(ui, server)


## Now add choice to make a choice


ui <- fluidPage(
  titlePanel("Conditional panel test"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("choose_to_choose", label = "select to make a choice",
                  value = FALSE),
      selectInput("the_choice", label = "Make a choice",
                  choices = c("Daddy", "Chips"), selected = "Daddy"
      ),
      textOutput("chosen_to_choose"),
      textOutput("the_chosen")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    )
  )
)

server <- function(input, output) {
  chosen_checkbox_option <- reactive({input$choose_to_choose})
  chosen_option <- reactive({input$the_choice})
  
  output$chosen_to_choose <- renderText({chosen_checkbox_option()})
  output$the_chosen <- renderText({paste("The choice is", chosen_option())})
}


shinyApp(ui, server)


## Now to make the choice conditional


ui <- fluidPage(
  titlePanel("Conditional panel test"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("choose_to_choose", label = "select to make a choice",
                    value = FALSE),
      conditionalPanel("choose_to_choose == true", 
          selectInput("the_choice", 
            label = "Make a choice",
            choices = c("Daddy", "Chips"), selected = "Daddy"
        ),
      )
      textOutput("chosen_to_choose"),
      textOutput("the_chosen")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    )
  )
)

server <- function(input, output) {
  chosen_checkbox_option <- reactive({input$choose_to_choose})
  chosen_option <- reactive({input$the_choice})
  
  output$chosen_to_choose <- renderText({chosen_checkbox_option()})
  output$the_chosen <- renderText({paste("The choice is", chosen_option())})
}


shinyApp(ui, server)


# Make modular

module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns("choose_to_choose"), label = "select to make a choice",
                  value = FALSE),
    conditionalPanel("choose_to_choose == true", ns = ns, 
                     selectInput("the_choice", 
                                 label = "Make a choice",
                                 choices = c("Daddy", "Chips"), selected = "Daddy"
                     ),
    ),
    textOutput(ns("chosen_to_choose")),
    textOutput(ns("the_chosen"))    
    
  )
}

module_server <- function(input, output, session){
  
  ns <- session$ns
  
  chosen_checkbox_option <- reactive({input$choose_to_choose})
  chosen_option <- reactive({input$the_choice})
  
  output$chosen_to_choose <- renderText({chosen_checkbox_option()})
  output$the_chosen <- renderText({paste("The choice is", chosen_option())})
  
}

ui <- fluidPage(
  titlePanel("Conditional panel test"),
  
  sidebarLayout(
    sidebarPanel(
      module_ui("module")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
    )
  )
)

server <- function(input, output) {
  callModule(module_server, "module")
  outputOptions(output, "chosen_to_choose", suspendWhenHidden = FALSE)
}


shinyApp(ui, server)



## Now trying something using dynamicUI

# unmodularised 
# from https://gist.github.com/wch/9609200


server <- function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "slider" = sliderInput("dynamic", "Dynamic",
                                  min = 1, max = 20, value = 10),
           "text" = textInput("dynamic", "Dynamic",
                              value = "starting value"),
           "numeric" =  numericInput("dynamic", "Dynamic",
                                     value = 12),
           "checkbox" = checkboxInput("dynamic", "Dynamic",
                                      value = TRUE),
           "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",
                                                choices = c("Option 1" = "option1",
                                                            "Option 2" = "option2"),
                                                selected = "option2"
           ),
           "radioButtons" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
           ),
           "selectInput" = selectInput("dynamic", "Dynamic",
                                       choices = c("Option 1" = "option1",
                                                   "Option 2" = "option2"),
                                       selected = "option2"
           ),
           "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                               choices = c("Option 1" = "option1",
                                                           "Option 2" = "option2"),
                                               selected = c("option1", "option2"),
                                               multiple = TRUE
           ),
           "date" = dateInput("dynamic", "Dynamic"),
           "daterange" = dateRangeInput("dynamic", "Dynamic")
    )
  })
  
  output$input_type_text <- renderText({
    input$input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
}


ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    
    column(3, wellPanel(
      selectInput("input_type", "Input type",
                  c("slider", "text", "numeric", "checkbox",
                    "checkboxGroup", "radioButtons", "selectInput",
                    "selectInput (multi)", "date", "daterange"
                  )
      )
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput("input_type_text"),
           tags$p("Dynamic input value:"),
           verbatimTextOutput("dynamic_value")
    )
  )
)


shinyApp(ui = ui, server = server)


# renderUI - modularised

module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    column(3, wellPanel(
      selectInput(ns("input_type"), "Input type",
                  c("slider", "text", "numeric", "checkbox",
                    "checkboxGroup", "radioButtons", "selectInput",
                    "selectInput (multi)", "date", "daterange"
                  )
      )
    )),
    
    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput(ns("ui"))
    )),
    
    column(3,
           tags$p("Input type:"),
           verbatimTextOutput(ns("input_type_text")),
           tags$p("Dynamic input value:"),
           verbatimTextOutput(ns("dynamic_value"))
    )
  )
  
}

module_server <- function(input, output, session){
  ns <- session$ns
  
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "slider" = sliderInput(ns("dynamic"), "Dynamic",
                                  min = 1, max = 20, value = 10),
           "text" = textInput(ns("dynamic"), "Dynamic",
                              value = "starting value"),
           "numeric" =  numericInput(ns("dynamic"), "Dynamic",
                                     value = 12),
           "checkbox" = checkboxInput(ns("dynamic"), "Dynamic",
                                      value = TRUE),
           "checkboxGroup" = checkboxGroupInput(ns("dynamic"), "Dynamic",
                                                choices = c("Option 1" = "option1",
                                                            "Option 2" = "option2"),
                                                selected = "option2"
           ),
           "radioButtons" = radioButtons(ns("dynamic"), "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
           ),
           "selectInput" = selectInput(ns("dynamic"), "Dynamic",
                                       choices = c("Option 1" = "option1",
                                                   "Option 2" = "option2"),
                                       selected = "option2"
           ),
           "selectInput (multi)" = selectInput(ns("dynamic"), "Dynamic",
                                               choices = c("Option 1" = "option1",
                                                           "Option 2" = "option2"),
                                               selected = c("option1", "option2"),
                                               multiple = TRUE
           ),
           "date" = dateInput(ns("dynamic"), "Dynamic"),
           "daterange" = dateRangeInput(ns("dynamic"), "Dynamic")
    )
  })
  
  output$input_type_text <- renderText({
    input$input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  
}

ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    module_ui("module")
  )
)



server <- function(input, output) {
  callModule(module_server, "module")
  
}


shinyApp(ui = ui, server = server)




# Two different outputs - not modularised

ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    selectInput("output_type",
      label = "Select type of output", 
      selected = "table",
      choices = c("table", "barplot", "graph") 
    ),
    uiOutput("diff_outputs")
    # textOutput("choice")
  )
)


server <- function(input, output){
  
  # output$choice <- renderText({
  #   switch(
  #     input$output_type,
  #     "table" = "you chose table",
  #     "barplot" = "you chose barplot",
  #     "graph" = "you chose graph"
  #   )
  #   
  # })
  get_choice <- reactive({input$choice})
  output$diff_outputs <- renderUI({
    if (is.null(input$output_type))
      return()

    switch(
      # input$output_type,
      get_choice(),
      "table" = renderTable({head(women)}),
      "barplot" = renderPlot({barplot(women$height)}),
      "graph"  = renderPlot({plot(women$height ~ women$weight)})
    )
  })
  # 
  output$output_type <- renderText({input$input_type})

}

shinyApp(ui = ui, server = server)



## Example

library(shiny)
library(dplyr)
library(ggplot2)


innerModUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(fluidRow(
    uiOutput(ns("inner_slider")),
    plotOutput(ns("inner_plot"))
  ))
}

innerMod <- function(input, output, session) {
  output$inner_slider <- renderUI({
    sliderInput(session$ns("slider2"), label = "inner module slider", min = round(min(mtcars$mpg)), 
                max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg), step = 1))
  })
  
  output$inner_plot <- renderPlot({
    req(input$slider2)
    data <- filter(mtcars, between(mpg, input$slider2[1], input$slider2[2]))
    ggplot(data, aes(mpg, wt)) + geom_point()
  })
}

outerModUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    uiOutput(ns("outer_slider")),
    plotOutput(ns("outer_plot")),
    innerModUI(ns("inner"))
  ))
}

outerMod <- function(input, output, session) {
  callModule(innerMod, "inner")
  
  output$outer_slider <- renderUI({
    sliderInput(session$ns("slider1"), label = "outer module slider", min = round(min(mtcars$mpg)), 
                max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg), step = 1))
  })
  
  output$outer_plot <- renderPlot({
    req(input$slider1)
    data <- filter(mtcars, between(mpg, input$slider1[1], input$slider1[2]))
    ggplot(data, aes(mpg, wt)) + geom_point()
  })
}

ui <- fluidPage(
  fluidRow(
    outerModUI("outer")
  )
)

server <- function(input, output, session) {
  callModule(outerMod, "outer")
  
}

shinyApp(ui = ui, server = server)

### Another example 
# From https://stackoverflow.com/questions/46898569/using-shinys-renderui-in-module


Module_YYY_Server <- function(input, output, session){
  
  output$DynamicContent <- renderUI({
    selectInput(session$ns("S_A_Input"), "Change Me for print message", choices = 1:3)
  })
  
  output$text <- renderText({
    req(input$S_A_Input)
    input$S_A_Input})
}


Module_YYY_Ui <- function(id){
  
  ns <- NS(id) # Creates Namespace
  
  tagList(
    uiOutput(ns("DynamicContent")),
    textOutput(ns("text"))
  )
}


ui <- bootstrapPage(
  Module_YYY_Ui("YYY")
)

server <- function(input, output,session) {
  callModule(Module_YYY_Server,"YYY")
}

shinyApp(ui = ui, server = server)


## working solution

ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    selectInput("output_type",
                label = "Select type of output", 
                selected = "table",
                choices = c("table", "barplot", "graph") 
    ),
    uiOutput("diff_outputs")
  )
)

server <- function(input, output){
  
  output$table <- renderTable({head(women)}) 
  
  output$barplot <- renderPlot({barplot(women$height)})
  
  output$scatterplot <- renderPlot({plot(women$height ~ women$weight)})
  
  output$diff_outputs <- renderUI({
    if (is.null(input$output_type))
      return()
    switch(
      input$output_type,
      "table" = tableOutput("table"),
      "barplot" = plotOutput("barplot"),
      "graph" = plotOutput("scatterplot")
    )
  })
  
}

shinyApp(ui = ui, server = server)


# Modularised version

## working solution

module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    selectInput(ns("output_type"),
                label = "Select type of output", 
                selected = "table",
                choices = c("table", "barplot", "graph") 
    ),
    uiOutput(ns("diff_outputs"))
  )
}


module_server <- function(input, output, session){
  ns <- session$ns
  
  output$table <- renderTable({head(women)}) 
  
  output$barplot <- renderPlot({barplot(women$height)})
  
  output$scatterplot <- renderPlot({plot(women$height ~ women$weight)})
  
  output_selected <- reactive({input$output_type})
  
  output$diff_outputs <- renderUI({
    if (is.null(output_selected()))
      return()
    switch(
      output_selected(),
      "table" = tableOutput(ns("table")),
      "barplot" = plotOutput(ns("barplot")),
      "graph" = plotOutput(ns("scatterplot"))
    )
  })
  
}

ui <- fluidPage(
  titlePanel("Dynamically generated user interface components"),
  fluidRow(
    module_ui("module")
  )
)

server <- function(input, output){
  callModule(module_server, "module")
}

shinyApp(ui = ui, server = server)



# Want a reactive input 

module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns("checkbox_condition"), 
                label = "Check to display slider",
                value = FALSE
    )#, 
    # sliderInput(ns("conditional_value"),
    #             label = "slide to select value", 
    #             min = 0, max = 5, value = 2
    # ),
    # uiOutput(ns("selective_slider")),
    #   
    # textOutput(ns("output_from_checkbox")),
    # textOutput(ns("output_from_conditional_value"))
    
  )
  
}


module_server <- function(input, output, session){
  ns <- session$ns
  
  # get_conditional_value <- reactive({output$conditional_value})
  # get_checkbox_value    <- reactive({input$selective_slider})
  
  # output$output_from_checkbox          <- renderText({
  #   paste("The value from the checkbox is", get_checkbox_value())
  # })

  input$selective_slider <- renderUI({
    if(!get_conditional_value()){
      return(NULL)
    } else { sliderInput(ns("conditional_value"),
                         label = "slide to select value",
                         min = 0, max = 5, value = 2
    )
    }
  })


  output$output_from_conditional_value <- renderText({
    paste("The conditional value is", get_conditional_value())
  })

  
}


ui <- fluidPage(
  titlePanel("Conditional slider"),
  fluidRow(
    module_ui("module")
  )
)

server <- function(input, output){
  callModule(module_server, "module")
}

shinyApp(ui = ui, server = server)


###############################################


module_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    checkboxInput(ns("checkbox_condition"), 
                  label = "Check to display slider",
                  value = FALSE
    ),
    
    sliderInput(ns("slider_input"),
                 label = "choose slider value", min = 0, max = 5, value =2
    ),
    
    uiOutput(ns("dynamic_part")),
  
    verbatimTextOutput(ns("checkbox_condition_output")),
    verbatimTextOutput(ns("slider_output")),
    verbatimTextOutput(ns("dynamic_value")),
    verbatimTextOutput(ns("dynamic_value_class"))
  )
  
}


module_server <- function(input, output, session){
  ns <- session$ns
  
  get_checkbox_condition   <- reactive({input$checkbox_condition})
  get_slider_value         <- reactive({input$slider_input})
  get_dynamic_value        <- reactive({input$dynamic})
  get_dynamic_value_class  <- reactive({class(get_dynamic_value())})
  
  
  toss_coin <- function() {rbernoulli(1)}
  

  
  output$dynamic_part              <- renderUI({
#    heads_or_tails <- toss_coin()
    heads_or_tails <- get_checkbox_condition()   
    if (heads_or_tails){
      sliderInput(ns("dynamic"), "Dynamic", 
                  min = 1, max = 10, value = 3
      )
    } else {
      return()
#      textInput(ns("dynamic"), "Dynamic"
#      )
    }
    
  })
  
  
  output$dynamic_value             <-  renderText({get_dynamic_value()})
  output$checkbox_condition_output <-  renderText({get_checkbox_condition()})
  output$slider_output             <-  renderText({get_slider_value()})
  output$dynamic_value_class       <-  renderText({get_dynamic_value_class()})
}


ui <- fluidPage(
  titlePanel("Conditional slider"),
  fluidRow(
    module_ui("module")
  )
)

server <- function(input, output){
  callModule(module_server, "module")
}

shinyApp(ui = ui, server = server)


