library(shiny)
library(tidyverse)


### Data 

codes_named <- read_rds("data/codes_named.rds")

#data_all <- read_csv("data/hmd_data.csv")
#names(data_all) <- tolower(data_all)

#### MODULE

select_data_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    selectInput(ns("code_select"),
                "Select country of interest",
                choices = codes_named,
                selected = "GBR_SCO",
                multiple = FALSE),

   selectInput(ns("sex_select"),
               "Select sex of interest",
               choices = c("Male", "Female","Total")),
   
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
    textOutput(ns("selection_confirmed")),
    
    tableOutput(ns("data_head_selected"))
    
  )
}

select_data_server <- function(input, output, session){
  ns <- session$ns
  
  # Internal functions
  load_subset <- function(full_data = "data/hmd_data.csv"){
    
    dta <- read_csv(full_data)
    names(dta) <- tolower(names(dta))
    
    output <- dta %>%
      filter(code %in% country_selected()) %>%
      filter(gender %in% sex_selected()) %>%
      filter(between(age, ages_selected()[1], ages_selected()[2])) %>%
      filter(between(year, years_selected()[1], years_selected()[2]))
    
    return(output)
  }
  
  
  # reactive events
  country_selected     <- eventReactive(input$confirm_selection, {input$code_select   })
  sex_selected         <- eventReactive(input$confirm_selection, {input$sex_select    })
  ages_selected        <- eventReactive(input$confirm_selection, {input$age_limits    })
  years_selected       <- eventReactive(input$confirm_selection, {input$period_limits })
  
  data_subset_selected <- eventReactive(input$confirm_selection, {load_subset()       })
  
  

  
  output$country_selected <- renderText({
    paste("The country selected was", country_selected())
  })
  
  output$sex_selected <- renderText({
    paste("The sex selected was", sex_selected())
  })
  
  output$ages_selected <- renderText({
    paste("The ages selected were", ages_selected()[1], "to", ages_selected()[2])
  })

  output$years_selected <- renderText({
    paste("The years selected were", years_selected()[1], "to", years_selected()[2])
  })

  output$data_head_selected  <- renderTable({data_subset_selected() %>% head()})
  data_all_selected <- reactive({data_subset_selected()})

  return(reactive({data_subset_selected()}))
  
}




ui <- fluidPage(
  select_data_ui("module"),
  textOutput("dim_of_dta")
)

server <- function(input, output) {
  data_ss <- callModule(select_data_server, "module")
  output$dim_of_dta <- renderText({
    paste("The selection has", dim(data_ss())[1], "rows and", dim(data_ss())[2], "columns")
  })
}




shinyApp(ui, server)