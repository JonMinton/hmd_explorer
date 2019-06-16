
make_graphics_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    
    # INPUTS
    selectInput(ns("select_outcome"),
      label = "Select outcome to display",
      choices = c("Population", "Mortality", "Log mortality"),
      selected = "Log mortality"
    ),
    selectInput(ns("select_colourscheme"),
      label = "Select colour scheme",
      choices = c(
        "Viridis:viridis",
        "Viridis:magma", 
        "Viridis:plasma",
        "Viridis:inferno",
        "Viridis:cividis"
        )
      ),
    checkboxInput(ns("add_continuity"), value = FALSE,
                label = "Check to add continuity correction"
      ),
    uiOutput(ns("dynamic_continuity")),
    selectInput(ns("select_devicevis"),
                label = "Select Graphics device and visualisation type",
                choices = c(
                  `ggplot2:Lexis surface` = "ggplot2:lexis",
                  `plotly:Lexis surface` = "plotly:lexis",
                  `plotly:3D surface` = "plotly:3d_surface"
                )
    ),
    
    
    
    
    # OUTPUTS
    
    textOutput(ns("outcome_selected")),
    textOutput(ns("colourscheme_selected")),
    textOutput(ns("devicevis_selected")),
    textOutput(ns("dims_of_data")),
    textOutput(ns("continuity_selected")),
    verbatimTextOutput(ns("mode_selected")),
    br(),
    uiOutput(ns("mainplot"))
  )
}

make_graphics_server <- function(input, output, session, mode = "singular-singular", data){
  ns <- session$ns
  
  # Internal functions

  
  # Given the specified outcome, produce a table with age, year and the outcome
  derive_outcome <- function(data, outcome, correction){

    if (is.null(correction)){
      correction <- 0
    }
    
    data <- data()

    output <- if (outcome == "Log mortality"){
      data %>% 
        select(year, age, num_deaths, num_population) %>% 
        mutate(
          num_deaths = num_deaths + correction,
          num_population = num_population + correction
        ) %>% 
        mutate(z_var = (num_deaths / num_population) %>% log(10)) %>% 
        select(year, age, z_var)
    } else if (outcome == "Mortality"){
      data %>% 
        select(year, age, num_deaths, num_population) %>% 
        mutate(
          num_deaths = num_deaths + correction,
          num_population = num_population + correction
        ) %>% 
        mutate(z_var = num_deaths / num_population) %>% 
        select(year, age, z_var)
    } else if (outcome == "Population") {
      data %>% 
        select(year, age, num_population) %>% 
        mutate(z_var = num_population) %>% 
        select(year, age, z_var)
    }
    output 
  }
  
  produce_surface_ggplot <- function(data, colscheme, devicetype){
    
    colscheme_tidied  <- colscheme %>% 
      str_split(":") %>% 
      pluck(1) %>% 
      setNames(c("Family", "pal"))
    
    if (devicetype != "ggplot2:lexis"){
      return(NULL)
    }
    
    p <- data %>% 
      ggplot(aes(x = year, y = age, fill = z_var)) + 
      geom_tile()
    
    if (colscheme_tidied["Family"] == "Viridis"){
      p <- p +
        scale_fill_viridis_c(option = colscheme_tidied["pal"])
    }
    
    p
  }
  
  produce_surface_plotly <- function(data, colscheme, devicetype){
    
    colscheme_tidied  <- colscheme %>% 
      str_split(":") %>% 
      pluck(1) %>% 
      setNames(c("Family", "pal"))
    
    if (colscheme_tidied["Family"] == "Viridis") {
      hue_posn <- seq(0, 1, length = 20)
      colscale_list <- list(
          hue_posn,
          viridis_pal(option = colscheme_tidied["pal"])(20)                
      )
    } else {
      
      # DO FOR OTHER PALETTES HERE
    }
    
    
    make_z_list <- function(X) {
      # adjust: amount to add to numerator and denominator
      # k: base to use if logging
      tmp <- X %>%
        spread(age, z_var) 
      
      years <- tmp$year
      tmp$year <- NULL
      ages = as.numeric(names(tmp))
      val_mtrx <- as.matrix(tmp)
      out <- list(age = ages, year = years, vals = val_mtrx)
    }
    
    data_mtrx <- make_z_list(data)

    
    n_ages  <-  length(data_mtrx[["age"]]  )
    n_years <-  length(data_mtrx[["year"]] )
    
    if (devicetype == "plotly:lexis"){
      p <- plot_ly(z = ~t(data_mtrx[["vals"]]), 
                   x = ~data_mtrx[["year"]],
                   y = ~data_mtrx[["age"]]
      ) %>% 
        add_heatmap(
          colorscale = colscale_list
          
        ) %>%  
        layout(
          yaxis = list(title = "Age in years"),
          xaxis = list(title = "Year")
        )
      
    } else if (devicetype == "plotly:3d_surface"){

      
      p <- plot_ly(z = ~data_mtrx[["vals"]], 
                   y = ~data_mtrx[["year"]],
                   x = ~data_mtrx[["age"]]
      ) %>% add_surface(
        colorscale = colscale_list
        
      ) %>% 
        layout(scene = list(
          zaxis = list(title = "Z"),
          xaxis = list(title = "Age in years"),
          yaxis = list(title = "Year"),
          aspectratio = list(
            x = n_ages / n_years,
            y = 1,
            z = 0.5
          )
        ))
    }  else {
      return(NULL)
    }  

    p
  }
  

  # reactive events
  selected_outcome          <- reactive({input$select_outcome        })
  selected_colourscheme     <- reactive({input$select_colourscheme   })
  selected_devicevis        <- reactive({input$select_devicevis      })
  selected_add_continuity   <- reactive({input$add_continuity        })
  selected_continuityvalue  <- reactive({input$continuity_value      })
  
  
  output$dynamic_continuity <- renderUI({
    if (!selected_add_continuity()){
      return()
    } else{
      numericInput(ns("continuity_value"), 
                   label = "Enter continuity correction value", 
                   value = 5,
                   min = 0, max = 500)               
    }
  })
  
  
  
  # output vector
  output$outcome_selected         <- reactive({selected_outcome()          })
  output$colourscheme_selected    <- reactive({selected_colourscheme()     })
  output$devicevis_selected       <- reactive({selected_devicevis()        })
  output$continuity_selected      <- reactive({paste("Cont value:", selected_continuityvalue())  })
  output$mode_selected            <- reactive({mode})

  
  output$dims_of_data <- renderText({
    paste("The data loaded have", dim(data())[1], "rows and", dim(data())[2], "columns.")
  })
  
  output$main_ggplot <- renderPlot({
    derive_outcome(data(), selected_outcome(), correction = selected_continuityvalue()) %>% 
      produce_surface_ggplot(selected_colourscheme(), selected_devicevis())
  })
  
  output$main_plotly <- renderPlotly({
    derive_outcome(data(), selected_outcome(), correction = selected_continuityvalue()) %>% 
      produce_surface_plotly(selected_colourscheme(), selected_devicevis())
  })
  

  output$mainplot <- renderUI({
    dv_selection <- selected_devicevis()
    out <- switch(
      dv_selection,
      "ggplot2:lexis"      = plotOutput(ns("main_ggplot")), 
      "plotly:lexis"       = plotlyOutput(ns("main_plotly")),
      "plotly:3d_surface"  = plotlyOutput(ns("main_plotly")) 
    )
    
    return(out)
  })
  

  # output 
  return(NULL)
  
}

