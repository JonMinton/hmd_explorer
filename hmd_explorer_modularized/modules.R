# # Function for module UI
# hello_worldUI <- function(id) {
#   ns <- NS(id)
#   
#   fluidPage(
#     fluidRow(
#       column(2, textInput(ns("TI_username"), label = NULL, placeholder = "your name")),
#       column(2, actionButton(ns("AB_hello"), label = "Hello !"))
#     ),
#     hr(),
#     fluidRow(
#       column(12, textOutput(ns("TO_Hello_user")))
#     )
#   )
#   
# }
# 
# # Function for module server logic
# hello_world <- function(input, output, session) {
#   
#   # When user clicks on "Hello" button : Update reactive variable "name"
#   name <- eventReactive(input$AB_hello, {
#     return(input$TI_username)
#   })
#   
#   # Show greetings
#   output$TO_Hello_user <- renderText({
#     if (name() %in% "") {
#       return("Hello world !")
#     } else {
#       return(paste("Hello", name(), "!"))
#     }
#   })
#   
# }
# 








codes_named <- read_rds("data/codes_named.rds")

# Select Population 

select_population_ui <- function(id){
  ns <- NS(id)

  tagList(

    selectInput(ns("code_select"),
                "Select country of interest",
                choices = codes_named,
                selected = "GBR_SCO",
                multiple = FALSE),
    conditionalPanel("input.mode_select == 'Absolute'", ns = ns,
      selectInput(ns("sex_select"),
                "Select sex of interest",
                choices = c("Male", "Female","Total"))
    ),
    sliderInput(ns("period_limits"), sep = "",
                "Select range of years",
                min = 1750, max = 2020, step = 1, 
                value = c(1750, 2020)),    
    sliderInput(ns("age_limits"), sep = "",
                "Select range of ages",
                min = 0, max = 110, step = 1, 
                value = c(0, 90)),  

    actionButton(ns("confirm_selection"),
                     "Load Data"
    ),
    
    
    textOutput(ns("country_selected")),
    textOutput(ns("sex_selected")),
    textOutput(ns("years_selected")),
    textOutput(ns("ages_selected")),
    textOutput(ns("mode_selected")),
    textOutput(ns("selection_confirmed"))
  )
  
}


select_population_server <- function(input, output, session){
  
  # load_subset <- function(
  #   full_data = "data/hmd_data.csv", 
  #   country, sex_selection, age_range, year_range
  #   ){
  #   dta <- read_csv(full_data) 
  #   names(dta) <- tolower(names(dta))
  #   
  #   output <- dta %>% 
  #     filter(code %in% country) %>% 
  #     filter(gender %in% sex_selection) %>% 
  #     filter(between(age, age_range[1], age_range[2])) %>% 
  #     filter(between(year, year_range[1], year_range[2]))
  #   
  #   return(output)
  # }
  
  
  country_selected <- eventReactive(input$confirm_selection, {input$code_select})
  # sex_selected <- reactive({input$sex_select})
  # years_selected <- reactive({input$period_limits})
  # ages_selected <- reactive({input$age_limits})
  # mode_selected <- reactive({input$mode_select})
  # selection_confirmed <- reactive({input$confirm_selection})
  # outcome_selected <- reactive({input$outputType_select})

  
  output$mode_selected <- renderText({
    paste("The country selected was", country_selected())
  })
  

# 
#   # This bit will load the data, but only if the selection_confirmed option has been clicked
#   data_selection <- eventReactive(selection_confirmed(), {
#     load_subset(
#       full_data = "data/hmd_data.csv", 
#       country = country_selected(), 
#       sex_selection = sex_selected(), 
#       age_range = ages_selected(), 
#       year_range = years_selected()
#     )
#   })
# 

  return("Howdy!")
  
}


### makeGraphics

# 
make_graphics_ui <- function(id){
  ns <- NS(id)

  tagList(
    # plotOutput(ns("main_vis"))
    textOutput(ns("main_text"))
  )
}
# 
make_graphics_server <- function(input, output, session, special_input){
  
  special_input <- reactive({special_input})
  # browser()
  # this_data <- reactive({
  #   if (type == "population") {
  #     data() %>%
  #       select(age, year, outcome = population)
  #   } else if (type == "lmr") {
  #     data() %>%
  #       select(age, year, num_deaths, num_population) %>%
  #       mutate(mr = num_deaths / num_population) %>%
  #       mutate(lmr = log(mr)) %>%
  #       rename(outcome = lmr)
  #   }
  output$main_text <- renderText(reactive({special_input()}))
}
# 
#   
#   
#   output$main_vis <- renderPlot({
#     output <- this_data() %>% 
#       ggplot(aes(x = year, y = age, fill = outcome)) +
#       geom_tile()
# 
#      return(output) 
#   })
#     
#   
#   # output$data_dimensions <- renderText({
#   #   dims_to_use <- dim(this_data())
#   #   paste("The data have", dims_to_use[1], "rows and", dims_to_use[2], "columns")
#   # })
#   # 
#   # output$datatable <- renderTable({
#   #   this_data() %>% head()
#   # })
#   # 
#   
#   return(output$main_vis)
#   
# }