
#### MODULE

select_data_ui <- function(id, allow_multiple = FALSE, select_sex = TRUE){
  ns <- NS(id)
  cat(file=stderr(), "select_data_ui()\n")
  sex_select_element <- selectInput(ns("sex_select"),
                "Select sex of interest",
                choices = c("Total", "Male", "Female"))
  
    
    pt1 <- list(
      selectInput(ns("code_select"),
                  "Select country of interest",
                  choices = codes_named,
                  selected = "GBR_SCO",
                  multiple = allow_multiple),
      
      selectInput(ns("output_type"),
                  "Select output of interest",
                  choices = c("Mx", "Dx", "e0"),
                  selected = "Mx")
    )
    
    pt2 <- list(
      
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
      )
      
      
    )
    output <- if(select_sex){
      tagList(pt1, sex_select_element, pt2)
    } else {
      tagList(pt1, pt2)
    }
    
    output <- tagList(
      output,
      verbatimTextOutput(ns("minmaxyear")),
      textOutput(ns("country_selected")),
      textOutput(ns("sex_selected")),
      textOutput(ns("years_selected")),
      textOutput(ns("ages_selected")),
      textOutput(ns("selection_confirmed")),

      tableOutput(ns("data_head_selected"))
      
    )
    
    return(output)
    
#  )
}

select_data_server <- function(input, output, session, mode = "standard"){
  cat(file=stderr(), "select_data_server()\n")
  
  # options for mode should be:
  # # standard      : one or more populations 
  # # sex-compare   : males cf females in same country 

  ns <- session$ns
  
  # Work out min and max year for the data 
  get_minmaxyear <- function(full_data = "data/hmd_data.csv"){
    cat(file=stderr(), "select_data_server::get_minmaxyear\n")
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
    cat(file=stderr(), "select_data_server::load_subset()\n")

    req(country_selected(), ages_selected(), years_selected(), sex_selected())
    
    dta <- read_csv(full_data)
    names(dta) <- tolower(names(dta))
        
    selected_country <- country_selected()
    selected_ages    <- ages_selected()
    selected_years   <- years_selected()
    selected_sex     <- sex_selected() 
    
    output <- dta %>%
      filter(code %in% selected_country) %>%
      filter(between(age, selected_ages[1], selected_ages[2])) %>%
      filter(between(year, selected_years[1], selected_years[2])) 

    
    if (mode == "sex-compare")  {
      output <- output %>% 
        filter(gender %in% c("Male", "Female")) %>% 
        group_by(gender) %>% 
        nest()

    } else {
      output <- output %>% filter(gender %in% selected_sex)
    }
    

    
    return(output)
  }
  
  
  # Reactive events
  country_selected     <- eventReactive(input$confirm_selection, {input$code_select   })
  period_snap_selected <- eventReactive(input$confirm_selection, {input$snap_period   })
  sex_selected         <- eventReactive(input$confirm_selection, {input$sex_select    })
  ages_selected        <- eventReactive(input$confirm_selection, {input$age_limits    })
  output_selected      <- eventReactive(input$confirm_selection, {input$output_type   })
  years_selected       <- eventReactive(input$confirm_selection, {input$period_limits })
  
  # Reactive Functions 
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

