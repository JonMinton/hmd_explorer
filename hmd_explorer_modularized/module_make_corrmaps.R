make_corrmaps_ui <- function(id){
  ns <- NS(id)
  cat(file=stderr(), "make_corrmaps_ui\n")
  tagList(
    # INPUTS
    selectInput(ns("select_corrtype"),
                label = "Select correlation type",
                choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                selected = "pearson"
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
    
    checkboxInput(ns("fix_scalelimits"), value = FALSE,
                     label = "check to select scale limits"
    ),
    uiOutput(ns("selected_scalelimits")),
    
    
    selectInput(ns("select_devicevis"),
                label = "Select Graphics device and visualisation type",
                choices = c(
                  `ggplot2:Lexis surface` = "ggplot2:lexis",
                  `plotly:Lexis surface` = "plotly:lexis"
                )
    ),
    
    
    
    
    # OUTPUTS
    
    textOutput(ns("corrtype_selected")),
#    plotOutput(ns("corrmap")),
#    plotlyOutput(ns("corrmap_plotly")),
    textOutput(ns("colourscheme_selected")),
    textOutput(ns("devicevis_selected")),
    textOutput(ns("continuity_selected")),
    verbatimTextOutput(ns("mode_selected")),

    br(),
    uiOutput(ns("mainplot")),
    plotlyOutput(ns("show_age_trends"))
  )
}

make_corrmaps_server <- function(input, output, session, data, mode){
  ns <- session$ns
  cat(file=stderr(), "make_corrmaps_server\n")
  

  

  selected_add_continuity   <- reactive({input$add_continuity        })
  selected_continuityvalue  <- reactive({input$continuity_value      })
  selected_corrtype         <- reactive({input$select_corrtype       })
  selected_colscheme        <- reactive({input$select_colourscheme   })
  selected_devicevis        <- reactive({input$select_devicevis      })
  selected_setscale         <- reactive({input$fix_scalelimits       })
  selected_scalelimits      <- reactive({input$corr_scalelimits      })
  
  calc_corrdata <- function(data, corrtype, correction, mode= "ggplot"){
    cat(file=stderr(), "make_corrmaps_server::calc_corrdata\n")
    
    data <- data()
    
    if(is.null(correction)){
      correction <- 0
    } 
    
    
    dta_trnd <- data %>% 
      # NOTE: CHANGE TO draw from Mx rather than the numerators and denominators (need option for module_Select_data)
      mutate(Mx_approx = (num_deaths + correction ) / (num_population + correction)) %>% 
      group_by(year, age) %>% 
      summarise(mean_Mx = mean(Mx_approx, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(log_mean_Mx = log(mean_Mx, 10)) 
    
    tmp <- dta_trnd %>% 
      select(-mean_Mx) %>% 
      spread(age, log_mean_Mx) %>%
      select(-year) %>% 
      cor(method = corrtype) 
    
    if(mode == "plotly") {return(tmp)}
    
    cor_df <- tmp %>% 
      as_tibble() %>% 
      mutate(from_age = rownames(tmp)) %>% 
      gather(key="to_age", value = "value", -from_age) %>% 
      mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    
    return(cor_df)
  }
  
  calc_twoage_data <- function(data, first_age, second_age, correction){
    cat(file=stderr(), "make_corrmaps_server::calc_twoage_data()\n")
    
    if(is.null(correction)){
      correction <- 0
    }
    
    dta_age1 <- data %>% 
      # NOTE: CHANGE TO draw from Mx rather than the numerators and denominators (need option for module_Select_data)
      filter(age == first_age) %>% 
      mutate(Mx_approx = (num_deaths + correction ) / (num_population + correction)) %>% 
      mutate(log_mean_Mx = log(Mx_approx, 10)) %>% 
      select(year, age, log_mean_Mx)
    
    dta_age2 <- data %>% 
      # NOTE: CHANGE TO draw from Mx rather than the numerators and denominators (need option for module_Select_data)
      filter(age == second_age) %>% 
      mutate(Mx_approx = (num_deaths + correction ) / (num_population + correction)) %>% 
      mutate(log_mean_Mx = log(Mx_approx, 10)) %>% 
      select(year, age, log_mean_Mx)
    
    dta_combined <- bind_rows(dta_age1, dta_age2)
    
    dta_combined
    
  }
  
  
  plot_corrmap <- function(data, colscheme, scalelimit){
    cat(file=stderr(), "make_corrmaps_server::plot_corrmap()\n")
    
    colscheme_tidied  <- colscheme %>% 
      str_split(":") %>% 
      pluck(1) %>% 
      setNames(c("Family", "pal"))
    

    
    
    p <- data %>%
      ggplot(aes(x = from_age, y = to_age, fill = value)) +
      geom_tile() +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      scale_y_continuous(breaks = seq(0, 100, by = 10)) +
      coord_equal()
    
    if (colscheme_tidied["Family"] == "Viridis" & is.null(scalelimit)){
      p <- p +
        scale_fill_viridis_c(option = colscheme_tidied["pal"])
    } else if (colscheme_tidied["Family"] == "Viridis" & !is.null(scalelimit)){
      p <- p +
        scale_fill_viridis_c(
          option = colscheme_tidied["pal"],
          limits = scalelimit                   
      )
    }
    
    p
  }
  
  plotly_corrmap    <- function(data, colscheme, scalelimit){
    cat(file=stderr(), "make_corrmaps_server::plotly_corrmap()\n")
    
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
    }
    
    hovertext_matrix <- paste0(
      "(", 
      rep(rownames(data), each = length(colnames(data))), 
      ", ", 
      rep(colnames(data), times = length(rownames(data))), 
      "): ", 
      round(data, 2)
    ) %>% 
      matrix(nrow = length(rownames(data)))

    
    p <- plot_ly(
      source = "corrmap_main",
      x = ~rownames(data), 
      y = ~colnames(data), 
      z = ~data, 
      hoverinfo = "text", 
      hovertext = hovertext_matrix
      ) %>% 
      add_heatmap(
        colorscale = colscale_list,
        zmin = scalelimit[1],
        zmax = scalelimit[2]
      ) %>% 
      layout(
        yaxis = list(
          title = "Age",
          scaleanchor = "x"           
        ),
        xaxis = list(title = "Age")

      )

    p
  }
  
  output$show_age_trends <-     renderPlotly({
    s <- event_data("plotly_click", source = "corrmap_main")

    if(is.null(s)){
      return(NULL)
    }
    
    first_age      <- s$x
    second_age     <- s$y 

    
    data_ss <- data() %>% 
      calc_twoage_data(
        first_age   = first_age, 
        second_age  = second_age, 
        correction  = selected_continuityvalue())
    
    p <- data_ss %>% 
      plot_ly(
        x = ~year, y = ~ log_mean_Mx, 
        color = ~factor(age), 
        colors = c("red", "darkgreen")
      ) %>% 
      add_lines()

    p
  })
  
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
  
  output$selected_scalelimits <- renderUI({
    if (!selected_setscale()){
      return()
    } else{
      sliderInput(ns("corr_scalelimits"),  
                   label = "Selected limits for correlation heatmap scales", 
                   value = c(-1, 1),
                   min = -1, max = 1, step = 0.1
      )               
    }
  })
  
  output$corrmap                  <- renderPlot({
    data() %>% 
      calc_corrdata(corrtype = selected_corrtype(), correction = selected_continuityvalue()) %>% 
      plot_corrmap(colscheme = selected_colscheme(), scalelimit = selected_scalelimits())
  })
  
  output$corrmap_plotly            <- renderPlotly({
    data() %>% 
      calc_corrdata(corrtype = selected_corrtype(), 
                    correction = selected_continuityvalue(), 
                    mode = "plotly"
      ) %>% 
      plotly_corrmap(colscheme = selected_colscheme(), scalelimit = selected_scalelimits()) 
    
  })
  
  
  output$continuity_selected      <- renderText({paste("Cont value:", selected_continuityvalue())  })
  

  
  output$mainplot <- renderUI({
    dv_selection <- selected_devicevis()
    out <- switch(
      dv_selection,
      "ggplot2:lexis"      = plotOutput(ns("corrmap")), 
      "plotly:lexis"       = plotlyOutput(ns("corrmap_plotly"))
    )
    
    return(out)
  })
  

}