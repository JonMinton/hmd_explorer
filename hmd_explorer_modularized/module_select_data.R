
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
    
    selectInput(ns("output_type"),
                "Select output of interest",
                choices = c("Mx", "Dx", "e0"),
                selected = "Mx"),
    
    checkboxInput(ns("snap_period"),
                  "Check to snap years to range available",
                  value = FALSE),
  
    uiOutput(ns("dynamic_year_slider")),
    
    
    sliderInput(ns("age_limits"), sep = "",
                "Select range of ages",
                min = 0, max = 110, step = 1, 
                value = c(0, 90)),  
    
    actionButton(ns("confirm_selection"),
                 "Load Data"
    ),
    
    verbatimTextOutput(ns("minmaxyear"))
    # textOutput(ns("country_selected")),
    # textOutput(ns("sex_selected")),
    # textOutput(ns("years_selected")),
    # textOutput(ns("ages_selected")),
    # textOutput(ns("selection_confirmed")),
    # 
    # tableOutput(ns("data_head_selected"))
    
  )
}

select_data_server <- function(input, output, session){
  ns <- session$ns
  
  # Work out min and max year for the data 
  get_minmaxyear <- function(full_data = "data/hmd_data.csv"){
    data <- read_csv(full_data)
    names(data) <- tolower(names(data))
    
    out <- data %>% 
      filter(code %in% country_selected()) %>% 
      pull(year) %>% 
      (function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
    
    out 
  }
  
  
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
  country_selected     <- reactive(                              {input$code_select   })
  period_snap_selected <- reactive(                              {input$snap_period   })
  
  sex_selected         <- eventReactive(input$confirm_selection, {input$sex_select    })
  ages_selected        <- eventReactive(input$confirm_selection, {input$age_limits    })
  output_selected      <- eventReactive(input$confirm_selection, {input$output_type   })
  
  # Functions 
  data_subset_selected <- eventReactive(input$confirm_selection, {load_subset()       })
  
  output$minmaxyear    <- renderText({
    get_minmaxyear()  
  })
  
  output$dynamic_year_slider <- renderUI({
    if (!period_snap_selected()){
      sliderInput(ns("period_limits"), sep = "",
                  "Select range of years",
                  min = 1750, max = 2020, step = 1, 
                  value = c(1750, 2020))   
    } else {
      year_range <- get_minmaxyear()
      sliderInput(ns("period_limits"), sep = "",
                  "Select range of available years",
                  min = year_range[1], max = year_range[2], step = 1,
                  value = year_range)
    }
  })
  
  years_selected       <- eventReactive(input$confirm_selection, {input$period_limits })
  
  # 
  # 
  # output$country_selected <- renderText({
  #   paste("The country selected was", country_selected())
  # })
  # 
  # output$sex_selected <- renderText({
  #   paste("The sex selected was", sex_selected())
  # })
  # 
  # output$ages_selected <- renderText({
  #   paste("The ages selected were", ages_selected()[1], "to", ages_selected()[2])
  # })
  # 
  # output$years_selected <- renderText({
  #   paste("The years selected were", years_selected()[1], "to", years_selected()[2])
  # })
  # 
  # output$data_head_selected  <- renderTable({data_subset_selected() %>% head()})
  data_all_selected <- reactive({data_subset_selected()})
  
  return(reactive({data_subset_selected()}))
  
}

