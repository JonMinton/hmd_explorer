#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)

full_data <- read_csv("data/hmd_data.csv")
codes_available <- read_rds(path = "data/codes_available.rds")

code_lookup <- read_csv("data/country_code_lookup.csv")
code_lookup <- code_lookup %>% 
  filter(country_code %in% codes_available)

named_codes <- pull(code_lookup, country_code)
names(named_codes) <- pull(code_lookup, country_name)

### FUNCTIONS 

make_z_list <- function(X, what = "lmr_k", adjust = 0, k = 10){
  tmp <- X %>% 
    select(year = Year, age = Age, n = num_deaths, N = exposure) 
  
  if(what == "lmr_k"){
    out_df <- tmp %>% 
      mutate(
        mr = (n + adjust) / (N+adjust), val = log(mr, k)
      ) %>% 
      mutate(val = ifelse(is.nan(val), NA, val))
  } else if (what == "mr") {
    out_df <- tmp %>% 
      mutate(val = (n + adjust) / (N+adjust))
  }
  
  out_df %>% 
    select(year, age, val) %>% 
    spread(age, val) -> tmp
  
  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}

make_z_list_pop <- function(X){


  out_df <- X %>% 
    select(year = Year, age = Age, val = num_population) 
  
  out_df %>% 
    select(year, age, val) %>% 
    spread(age, val) -> tmp
  
  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}


shinyServer(function(input, output) {
  

  output$mort_surface <- renderPlotly({
#    browser()

    
    dta_ss <- full_data %>% 
      filter(code == input$code_select) %>% 
      filter(gender == input$gender_select) %>% 
      group_by(code, gender) %>% 
      nest()
    
    z_list <- dta_ss %>% 
      mutate(lmr_list = map(data, make_z_list, adjust = 0.5))
    
    xx <- z_list[["lmr_list"]][[1]][["age"]]
    yy <- z_list[["lmr_list"]][[1]][["year"]]
    zz <- 10^z_list[["lmr_list"]][[1]][["vals"]]
    
    zc <- z_list[["lmr_list"]][[1]][["vals"]]

    p <-   plot_ly(
        x = ~xx,
        y = ~yy,
        z = ~zz,
        surfacecolor = ~zc,
        source = "mort_surface"
      ) %>% add_surface(
        colorbar = list(
          title = "Log Mortality rate"
        )
      ) %>% 
      layout(
        scene = list(
          zaxis = list(
            title = "Mortality rate",
            type = "log", 
            autorange = TRUE
          ),
          xaxis = list(
            title = "Age in years"
          ),
          yaxis = list(
            title = "Year"
          )
        )
      )
    browser()
    return(p)
  })

  output$pop_surface <- renderPlotly({
#    browser()
    dta_ss <- full_data %>% 
      filter(code == input$code_select) %>% 
      filter(gender == input$gender_select) %>% 
      group_by(code, gender) %>% 
      nest()
    
    z_list <- dta_ss %>% 
      mutate(pop_list = map(data, make_z_list_pop))
    
    xx <- z_list[["pop_list"]][[1]][["age"]]
    yy <- z_list[["pop_list"]][[1]][["year"]]
    zz <- z_list[["pop_list"]][[1]][["vals"]]
    
    zc <- z_list[["pop_list"]][[1]][["vals"]]
    
    p <-   plot_ly(
      x = ~xx,
      y = ~yy,
      z = ~zz,
      surfacecolor = ~zc
    ) %>% add_surface(
      colorbar = list(
        title = "Population"
      )
    ) %>% 
      layout(
        scene = list(
          zaxis = list(
            title = "Population"
          ),
          xaxis = list(
            title = "Age in years"
          ),
          yaxis = list(
            title = "Year"
          )
        )
      )
    
    return(p)
  })
  
    
  output$subplots <- renderPlotly({
#    browser()
    s <- event_data("plotly_hover", source = "mort_surface")
    
    if (length(s) == 0){ return(NULL)} else {
      
      this_age <- s$x
      this_year <- s$y
      this_cohort = this_year - this_age
      
      p1 <- full_data %>% 
        filter(Age <=90) %>% 
        filter(code == input$code_select) %>% 
        filter(gender == input$gender_select) %>% 
        filter(Age == this_age) %>% 
        mutate(mr = num_deaths / exposure) %>% 
        plot_ly(x = ~Year, y = ~mr) %>% 
        add_lines() 
      
      p2 <- full_data %>% 
        filter(Age <=90) %>% 
        filter(code == input$code_select) %>% 
        filter(gender == input$gender_select) %>% 
        filter(Year == this_year) %>% 
        mutate(mr = (num_deaths +0.5)/ (exposure+0.5)) %>% 
        plot_ly(x = ~Age, y = ~mr) %>% 
        add_lines() 
      
      p3 <- full_data %>% 
        filter(Age <=90) %>% 
        filter(code == input$code_select) %>% 
        filter(gender == input$gender_select) %>% 
        mutate(birth_cohort = Year - Age) %>% 
        filter(birth_cohort == this_cohort) %>% 
        mutate(mr = (num_deaths +0.5)/ (exposure+0.5)) %>% 
        plot_ly(x = ~Age, y = ~mr) %>% 
        add_lines() 
      
      this_country_name <- names(named_codes[named_codes == input$code_select])
      
      p <- subplot(list(p1, p2, p3), shareY = TRUE) %>% 
        layout(
          yaxis = list(
            title = "Mortality rate", 
            range = c(-5, 0), 
            type = "log"
          ),
          xaxis = list(title = "Year"),
          xaxis2 = list(title = "Age", range = c(0, 90)),
          xaxis3 = list(title = paste0("Age for ", this_cohort, " birth cohort"),
                        range = c(0, 90)),
          title = paste0(
            "Mortality schedules for ", input$gender_select, ", ", this_country_name, " in year ", this_year,
            " and age ", this_age
          ),
          showlegend = FALSE
        )
    }
    return(p)
    
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_hover")
    
    if (length(s) == 0) { print("Move around!")} else {
      as.list(s)
    }
    
  })
  
  output$input_checker <- renderPrint({
    cat(named_codes, "\t")
    cat(names(named_codes),"\n\n")
    this_country_name <- names(named_codes[named_codes == input$code_select])
#    browser()
    out <- cat("country: ", this_country_name, "\t(", input$code_select, ")\n",
                 "gender: ", input$gender_select, "\n")
    
    return(out)
    
  })
  
})
