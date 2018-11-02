#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(plotly)
library(tidyverse)

library(shiny)

full_data <- read_csv("data/hmd_data.csv") 
names(full_data) <- tolower(names(full_data))

codes_named <- read_rds("data/codes_named.rds")

hmd_e0 <- read_csv("data/hmd_e0.csv") 

names(hmd_e0) <- tolower(names(hmd_e0))

hmd_e0 <- hmd_e0 %>% 
  mutate(gender = tolower(gender))



### FUNCTIONS 

make_z_list_pop <- function(X, what = "num_population"){
  out_df <- X %>% 
    select(year = year, age = age, val = !!what) 
  
  out_df %>% 
    select(year, age, val) %>% 
    spread(age, val) -> tmp
  
  years <- tmp$year
  tmp$year <- NULL
  ages = as.numeric(names(tmp))
  val_mtrx <- as.matrix(tmp)
  out <- list(age = ages, year = years, vals = val_mtrx)
}


make_z_list <- function(X, what = "lmr_k", adjust = 0, k = 10){
  # adjust: amount to add to numerator and denominator 
  # k: base to use if logging
  tmp <- X %>% 
    select(year = year, age = age, n = num_deaths, N = exposure) 
  
  if(what == "lmr_k"){
    out_df <- tmp %>% 
      mutate(
        mr = (n + adjust) / (N+adjust), val = log(mr, k)
      ) %>% 
      mutate(val = ifelse(is.nan(val), NA, val))
  } else if (what == "mr") {
    out_df <- tmp %>% 
      mutate(val = (n + adjust) / (N + adjust) )
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



shinyServer(function(input, output){

  newdata <- eventReactive(input$recalc,
    {
       tmp <- full_data %>% 
         filter(gender == input$gender_select) %>% 
         mutate(
           pop_group = case_when(
             code %in% input$multi_code_select_B ~ "B", 
             code %in% input$multi_code_select_A ~ "A", 
             TRUE ~ NA_character_
           )
         ) %>% 
         filter(!is.na(pop_group)) 
       
       if(input$limit_age){
         tmp <- tmp %>%
           filter(age >= input$age_limits[1], age <= input$age_limits[2])
       }
       
       if (input$limit_period){
         tmp <- tmp %>%
           filter(year >= input$period_limits[1], year <= input$period_limits[2])
       }
       
       tmp <- tmp %>% 
         group_by(pop_group, year, age) %>% 
         summarise(
           exposure = sum(exposure, na.rm = T),
           num_deaths = sum(num_deaths, na.rm = T)
         ) %>% 
         ungroup() %>%
         mutate(lmr = log((num_deaths + input$small_n_correction)/(exposure + input$small_n_correction),10)) 
       
       return(tmp)
     })  
 
  output$mort_surface <- renderPlotly({

    dta_ss <- full_data %>% 
      filter(code == input$code_select) %>% 
      filter(gender == input$gender_select) 


    if(input$limit_age){
      dta_ss <- dta_ss %>% 
        filter(age >= input$age_limits[1], age <= input$age_limits[2])
    }    
    if (input$limit_period){
      dta_ss <- dta_ss %>% 
        filter(year >= input$period_limits[1], year <= input$period_limits[2])
    }
    
    dta_ss <- dta_ss %>% 
      group_by(code, gender) %>% 
      nest()
    
    z_list <- dta_ss %>% 
      mutate(lmr_list = map(data, make_z_list, adjust = 0.5))
    
    
    xx <- z_list[["lmr_list"]][[1]][["age"]]
    yy <- z_list[["lmr_list"]][[1]][["year"]]
    zz <- 10^z_list[["lmr_list"]][[1]][["vals"]]
    
    n_ages <- length(xx)
    n_years <- length(yy)
    
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
            title = "age in years"
          ),
          yaxis = list(
            title = "year"
          ),
          aspectratio = list(
            x = n_ages / n_years, y = 1, z = 0.5
          )
        )
      )
    

    return(p)
  })
  # 
  output$mort_subplot <- renderPlotly({
    s <- event_data("plotly_hover", source = "mort_surface")
    if (length(s) == 0){ return(NULL)} else {

      this_age <- s$x
      this_year <- s$y
      this_cohort = this_year - this_age

      p1 <- full_data %>%
        filter(age <=100) %>%
        filter(code == input$code_select) %>%
        filter(gender == input$gender_select) %>%
        filter(age == this_age) %>%
        mutate(mr = num_deaths / exposure) %>%
        plot_ly(x = ~year, y = ~mr) %>%
        add_lines()

      p2 <- full_data %>%
        filter(age <=100) %>%
        filter(code == input$code_select) %>%
        filter(gender == input$gender_select) %>%
        filter(year == this_year) %>%
        mutate(mr = (num_deaths +0.5)/ (exposure+0.5)) %>%
        plot_ly(x = ~age, y = ~mr) %>%
        add_lines()

      p3 <- full_data %>%
        filter(age <=100) %>%
        filter(code == input$code_select) %>%
        filter(gender == input$gender_select) %>%
        mutate(birth_cohort = year - age) %>%
        filter(birth_cohort == this_cohort) %>%
        mutate(mr = (num_deaths +0.5)/ (exposure+0.5)) %>%
        plot_ly(x = ~age, y = ~mr) %>%
        add_lines()
    #
       this_country_name <- names(codes_named[codes_named == input$code_select])
    #
      p <- subplot(list(p1, p2, p3), shareY = TRUE) %>%
        layout(
          yaxis = list(
            title = "Mortality rate",
            range = c(-5, 0),
            type = "log"
          ),
          xaxis = list(title = "year"),
          xaxis2 = list(title = "age", range = c(0, 100)),
          xaxis3 = list(title = paste0("age for ", this_cohort, " birth cohort"),
                        range = c(0, 100)),
          title = paste0(
            "Mortality schedules for ", input$gender_select, ", ", this_country_name,
            " in year ", this_year,
            " and age ", this_age
          ),
          showlegend = FALSE
        )
    }
     return(p)
  })
  
  output$pop_surface <- renderPlotly({

    this_code <- input$code_select
    this_gender <- input$gender_select
    
    if (this_gender == "Total"){
      
      dta_ss <- full_data %>% 
        filter(code == this_code) %>% 
        filter(gender != "Total") %>% 
        group_by(code, age, year) %>% 
        mutate(cumulative_pop = cumsum(num_population)) %>% 
        ungroup()
      
    } else {
      dta_ss <- full_data %>% 
        filter(code == this_code) %>% 
        filter(gender == this_gender) 
    }


    if(input$limit_age){
      dta_ss <- dta_ss %>%
        filter(age >= input$age_limits[1], age <= input$age_limits[2])
    }

    if (input$limit_period){
      dta_ss <- dta_ss %>%
        filter(year >= input$period_limits[1], year <= input$period_limits[2])
    }

    if (this_gender == "Total"){
      
      z_list <- dta_ss %>% 
        group_by(code, gender) %>% 
        nest() %>% 
        mutate(pop_list = map(data, make_z_list_pop, what = "cumulative_pop"))
      
    } else {
      z_list <- dta_ss %>% 
        group_by(code, gender) %>% 
        nest() %>% 
        mutate(pop_list = map(data, make_z_list_pop, what = "num_population"))
    }


    if (this_gender == "Total"){

      xx <- z_list[["pop_list"]][[1]][["age"]]
      yy <- z_list[["pop_list"]][[1]][["year"]]
      zzf <- z_list[["pop_list"]][[1]][["vals"]] # females
      zzm <- z_list[["pop_list"]][[2]][["vals"]] # males
      
      n_ages <- length(xx)
      n_years <- length(yy)
      
      p <- plot_ly(
        showscale = FALSE,
        source = "pop_surface"
      ) %>% 
      add_surface(
        x = ~xx,
        y = ~yy,
        z = ~zzf,
        name = "Females",
        opacity = 0.7,
        colorscale = list(
          c(0, 1),
          c("red", "red")
        )
      ) %>%
        add_surface(
          x = ~xx,
          y = ~yy, 
          z = ~zzm,
          name = "Males",
          opacity = 0.7,
          colorscale = list(
            c(0, 1),
            c("blue", "blue")
          )
          
        ) %>% 
        layout(
          scene = list(
            zaxis = list(
              title = "Population"
            ),
            xaxis = list(
              title = "age in years"
            ),
            yaxis = list(
              title = "year"
            ),
            aspectratio = list(
              x = n_ages / n_years, y = 1, z = 0.5
            ),
            showlegend = FALSE
          )
        )
      
      
      
    } else {
      xx <- z_list[["pop_list"]][[1]][["age"]]
      yy <- z_list[["pop_list"]][[1]][["year"]]
      zz <- z_list[["pop_list"]][[1]][["vals"]]
      
      n_ages <- length(xx)
      n_years <- length(yy)
      
      p <-   plot_ly(
        x = ~xx,
        y = ~yy,
        z = ~zz,
        surfacecolor = ~zz,
        source = "pop_surface"
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
              title = "age in years"
            ),
            yaxis = list(
              title = "year"
            ),
            aspectratio = list(
              x = n_ages / n_years, y = 1, z = 0.5
            )
          )
        )
      
    }

    return(p)
  })

  output$pop_subplot <- renderPlotly({

    s <- event_data("plotly_hover", source = "pop_surface")
    
    if (length(s) == 0){ return(NULL)} else {


      
      this_code <- input$code_select
      this_gender <- input$gender_select
      this_age <- s$x
      this_year <- s$y
      this_cohort = this_year - this_age
      
      if (this_gender == "Total") {
        

        data_ss <- full_data %>% 
          filter(age <= 100) %>% 
          filter(code == this_code) %>% 
          filter(gender != "Total") %>% 
          group_by(year, age) %>% 
          mutate(cumulative_pop = cumsum(num_population)) %>% 
          mutate(prev = lag(cumulative_pop, default = 0)) %>% 
          ungroup()
        
        max_pop <- data_ss %>% 
          pull(cumulative_pop) %>% max(na.rm = T)
        
        p1 <- data_ss %>% 
          filter(age == this_age) %>% 
          plot_ly(x = ~ year) %>% 
          add_ribbons(
            ymin = ~prev, ymax = ~cumulative_pop,
            color = ~gender,
            showlegend = TRUE
          )  
        

        p2 <- data_ss %>% 
          filter(year == this_year) %>%
          plot_ly(x = ~age) %>%
          add_ribbons(
            ymin = ~prev, ymax = ~cumulative_pop,
            color = ~gender,
            showlegend = FALSE
          )

        p3 <- data_ss %>% 
          mutate(birth_cohort = year - age) %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age) %>%
          add_ribbons(
            ymin = ~prev, ymax = ~cumulative_pop,
            color = ~gender,
            showlegend = FALSE
          )
        
      } else {
        data_ss <- full_data %>% 
          filter(age <= 100) %>% 
          filter(code == this_code) %>% 
          filter(gender == this_gender) 
        
        max_pop <- data_ss %>% 
          pull(num_population) %>% max(na.rm = T)
        
        p1 <- data_ss %>% 
          filter(code == this_code) %>% 
          filter(gender == this_gender) %>% 
          filter(age == this_age) %>% 
          plot_ly(x = ~year, y = ~num_population) %>% 
          add_lines(showlegend = FALSE)
        
        p2 <- data_ss %>% 
          filter(year == this_year) %>%
          plot_ly(x = ~age, y = ~num_population) %>%
          add_lines(showlegend = FALSE)
        
        p3 <- data_ss %>% 
          mutate(birth_cohort = year - age) %>%
          filter(birth_cohort == this_cohort) %>%
          plot_ly(x = ~age) %>%
          add_lines(y = ~num_population, showlegend = FALSE)
        
      }
      
      


      this_country_name <- names(codes_named[codes_named == input$code_select])
      #
      p <- subplot(list(p1, p2, p3), shareY = TRUE) %>%
        layout(
          yaxis = list(
            title = "Population size",
            range = c(0, max_pop)
          ),
          xaxis = list(title = "year"),
          xaxis2 = list(title = "age", range = c(0, 100)),
          xaxis3 = list(title = paste0("age for ", this_cohort, " birth cohort"),
                        range = c(0, 100)),
          title = paste0(
            "Population counts for ", input$gender_select, ", ", this_country_name,
            " in year ", this_year,
            " and age ", this_age
          )
        )
    }
    return(p)
  })
  
  
  output$mort_ratio_surface <- renderPlotly({
    
    ratio_limit <- as.double(input$ratio_limiter)
    this_code <- input$code_select
    n_correction <- input$small_n_correction
    

      dta_ss <- full_data %>% 
        filter(code == this_code) %>% 
        filter(gender != "Total") 
      
    
    if(input$limit_age){
      dta_ss <- dta_ss %>%
        filter(age >= input$age_limits[1], age <= input$age_limits[2])
    }
    
    if (input$limit_period){
      dta_ss <- dta_ss %>%
        filter(year >= input$period_limits[1], year <= input$period_limits[2])
    }
      z_list <- dta_ss %>% 
        mutate(mr = (num_deaths + n_correction) / (exposure + n_correction)) %>% 
        select(age, year, gender, mr) %>% 
        spread(gender, mr) %>% 
        mutate(
          ratio = Male / Female,
          excess = (Male - Female) / Female
        ) %>% 
        mutate(
          ratio = case_when(
            ratio < -ratio_limit ~ -ratio_limit,
            ratio > ratio_limit  ~ ratio_limit, 
            TRUE ~ ratio
          )
        ) %>% 
        nest() %>% 
        mutate(
          ratio_list = map(data, make_z_list_pop, what = "ratio"),
          excess_list = map(data, make_z_list_pop, what = "excess")
               )
    
    
      xx <- z_list[["ratio_list"]][[1]][["age"]]
      yy <- z_list[["ratio_list"]][[1]][["year"]]
      zz <- z_list[["ratio_list"]][[1]][["vals"]]
      zz_e <- z_list[["excess_list"]][[1]][["vals"]]

      n_ages <- length(xx)
      n_years <- length(yy)
      
      custom_text <- paste0(
        "In ", rep(yy, times = length(xx)), ", at age ", 
        rep(xx, each = length(yy)), ", there were \n",
        ifelse(zz > 1, 
               paste0(round(zz, 2), " male deaths/female death"),
               paste0(round(1/zz, 2), " female deaths/male death")
        ), "\n(",
        ifelse(zz_e > 0, 
               paste0( round(zz_e * 1000, 0), " excess male deaths/1000 females"),
               paste0(-round(zz_e * 1000, 0), " excess female deaths/1000 males")
        ), ")"
      ) %>% 
        matrix(length(yy), length(xx))
      
      p <- plot_ly(
        showscale = FALSE
      ) %>% 
        add_surface(
          name = "Male:Female Mortality",
          x = ~xx,
          y = ~yy,
          z = ~zz,
          surfacecolor = ~log(zz),
          colorscale = list(
            seq(from = -ratio_limit, to = ratio_limit, length.out = 10),
            colorRampPalette(RColorBrewer::brewer.pal(5, "RdBu"))(10)
          ),
          hoverinfo = "text", text = custom_text,
          cmin = -ratio_limit, cmax = ratio_limit,
          cauto = F
        ) %>%
        add_surface(
          name = "equal ratio",
          x = ~c(min(xx), max(xx)),
          y = ~c(min(yy), max(yy)),
          z = ~matrix(rep(1, 4), nrow = 2),
          opacity = 0.5
        ) %>% 
        layout(
          scene = list(
            zaxis = list(
              title = "Ratio",
              type = "log"
            ),
            xaxis = list(
              title = "age in years"
            ),
            yaxis = list(
              title = "year"
            ),
            aspectratio = list(
              x = n_ages / n_years, y = 1, z = 0.5
            ),
            showlegend = FALSE
          )
        )
      
    return(p)
  })
  
  
  output$pop_ratio_surface <- renderPlotly({
    
    ratio_limit <- as.double(input$ratio_limiter)
    this_code <- input$code_select
    n_correction <- input$small_n_correction
    
    
    dta_ss <- full_data %>% 
      filter(code == this_code) %>% 
      filter(gender != "Total") 
    
    
    if(input$limit_age){
      dta_ss <- dta_ss %>%
        filter(age >= input$age_limits[1], age <= input$age_limits[2])
    }
    
    if (input$limit_period){
      dta_ss <- dta_ss %>%
        filter(year >= input$period_limits[1], year <= input$period_limits[2])
    }
    
    z_list <- dta_ss %>% 
      mutate(N = (num_population + n_correction) ) %>% 
      select(age, year, gender, N) %>% 
      spread(gender, N) %>% 
      mutate(
        ratio = Male / Female,
        excess = (Male - Female) / Female,
        ratio = case_when(
          ratio < -ratio_limit ~ -ratio_limit,
          ratio > ratio_limit  ~ ratio_limit, 
          TRUE ~ ratio
        )
      ) %>% 
      nest() %>% 
      mutate(
        ratio_list = map(data, make_z_list_pop, what = "ratio"),
        excess_list = map(data, make_z_list_pop, what = "excess")
      )
    
    
    xx <- z_list[["ratio_list"]][[1]][["age"]]
    yy <- z_list[["ratio_list"]][[1]][["year"]]
    zz <- z_list[["ratio_list"]][[1]][["vals"]]
    zz_e <- z_list[["excess_list"]][[1]][["vals"]]
    
    
    n_ages <- length(xx)
    n_years <- length(yy)
    custom_text <- paste0(
      "In ", rep(yy, times = length(xx)), ", at age ", 
      rep(xx, each = length(yy)), ", there were \n",
      ifelse(zz > 1, 
             paste0(round(zz, 2), " males per female"),
             paste0(round(1/zz, 2), " females per male")
      ), "\n(",
  ifelse(zz_e > 0, 
         paste0(round(zz_e * 1000, 0), " excess males/1000 females"),
         paste0(-round(zz_e * 1000, 0), " excess females/1000 males")
  ), ")"
) %>% 
      matrix(length(yy), length(xx))
    
    p <- plot_ly(
      showscale = FALSE
    ) %>% 
      add_surface(
        name = "Male:Female\nPopulation Ratio",
        x = ~xx,
        y = ~yy,
        z = ~zz,
        surfacecolor = ~log(zz),
        colorscale = list(
          seq(from = -ratio_limit, to = ratio_limit, length.out = 10),
          colorRampPalette(RColorBrewer::brewer.pal(5, "RdBu"))(10)
        ),
        hoverinfo = "text", text = custom_text,
        cmin = -ratio_limit, cmax = ratio_limit,
        cauto = F
      ) %>%
      add_surface(
        name = "equal ratio",
        x = ~c(min(xx), max(xx)),
        y = ~c(min(yy), max(yy)),
        z = ~matrix(rep(1, 4), nrow = 2),
        opacity = 0.5
      ) %>% 
      layout(
        scene = list(
          zaxis = list(
            title = "Ratio",
            type = "log"
          ),
          xaxis = list(
            title = "age in years"
          ),
          yaxis = list(
            title = "year"
          ),
          aspectratio = list(
            x = n_ages / n_years, y = 1, z = 0.5
          ),
          showlegend = FALSE
        )
      )
    
    return(p)
  })
  
  output$mort_group_surface <- renderPlotly({
  

    
    diffs <- newdata() %>% 
      select(pop_group, year, age, lmr) %>% 
      spread(pop_group, lmr) %>% 
      mutate(diff_lmr = B - A)
    
    z_list <- make_z_list_pop(diffs, what = "diff_lmr")
    
    range_limits <- eventReactive(input$recalc,
      {
        if (input$limit_diffz){
          range_limits <- as.double(input$diffz_limits)
        } else {
          maxabs <- max(abs(zz[is.finite(zz)]))
          range_limits <- c(-maxabs, maxabs)
        }
        return(range_limits)
      })


    
    xx <- z_list[["age"]]
    yy <- z_list[["year"]]
    zz <- z_list[["vals"]]

    n_ages <- length(xx)
    n_years <- length(yy)
    
    range_limits <- range_limits()
    maxabs <- max(abs(range_limits))
    
    yearvec <- rep(yy, times = length(xx))
    agevec <- rep(xx, each = length(yy))
    cohortvec <- yearvec - agevec
    
    rr <- 10^zz # mortality ratio
    
    custom_text <- paste0(
      "In ", yearvec, ", at age ", 
      agevec, " (", cohortvec, " birth cohort)\n",
      "Diff in log mortality: ",
        paste0(round(zz, 2)),
      "\nRatio: ", paste0(round(rr, 2)), " (", paste0(round(1/rr, 2)), ")"
    ) %>% 
      matrix(length(yy), length(xx))
    
    
    p <- plot_ly(
      showscale = FALSE,
      source = "mort_group_surface"
    ) %>% 
      add_surface(
        name = "Diff in LMR",
        x = ~xx,
        y = ~yy,
        z = ~zz,
        surfacecolor = ~zz,
        colorscale = list(
          seq(from = -maxabs, to = maxabs, length.out = 10),
          colorRampPalette(RColorBrewer::brewer.pal(5, "RdBu"))(10)
        ),
        hoverinfo = "text", text = custom_text,
        cmin = range_limits[1], cmax = range_limits[2],
        cauto = F
      ) %>%
      add_surface(
        name = "equal ratio",
        x = ~c(min(xx), max(xx)),
        y = ~c(min(yy), max(yy)),
        z = ~matrix(rep(0, 4), nrow = 2),
        opacity = 0.5
      ) %>% 
      layout(
        scene = list(
          zaxis = list(
            title = "Diff in log mortality"
          ),
          xaxis = list(
            title = "age in years"
          ),
          yaxis = list(
            title = "year"
          ),
          aspectratio = list(
            x = n_ages / n_years, y = 1, z = 0.5
          ),
          showlegend = FALSE
        )
      )
    

    
    
    return(p)
  })
  
  
  output$mort_group_subplot <- renderPlotly({
    # cat(file=stderr(), "the value of recalc is ", input$recalc,"\n")   
    # s <- event_data("plotly_click", source = "mort_group_surface")
    # cat(file=stderr(), "s ", ifelse(is.null(s), "is", "is not"), "NULL\n")   

    if(is.null(s)) {return(NULL)} else {
      diffs <- newdata() %>% 
        select(pop_group, year, age, lmr) %>% 
        spread(pop_group, lmr) %>% 
        mutate(diff_lmr = B - A)
      
      this_age <- s$x[1]
      this_year <- s$y[1]
      this_cohort <- this_year - this_age
            
      absmax <- max(abs(diffs$diff_lmr), na.rm = T)
      
      p1 <- diffs %>% 
        filter(age == this_age) %>% 
        plot_ly(x = ~year, y = ~diff_lmr
        ) %>% 
        add_lines(showlegend = FALSE,
          hoverinfo = 'text',
          text = ~paste0(
            "Year: ", year,
            '\nLog mortalities: ', round(B, 3), ' - ', round(A, 3), ' = ', round(diff_lmr, 3),
            '\nDeaths per 10,000: ', round(10000 * 10^B, 0), ' - ', round(10000 * 10^A, 0), 
            "\nso ", 
            ifelse(
              B > A,
              paste0( round(10000 * (10^B - 10^A),0), " more deaths"),
              paste0( round(10000 * (10^A - 10^B),0), " fewer deaths")
            )
          )          
        )
      
      p2 <- diffs %>% 
        filter(year == this_year) %>%
        plot_ly(x = ~age, y = ~diff_lmr) %>%
        add_lines(showlegend = FALSE,
          hoverinfo = 'text',
          text = ~paste0(
            "Age: ", age,
            '\nLog mortalities: ', round(B, 3), ' - ', round(A, 3), ' = ', round(diff_lmr, 3),
            '\nDeaths per 10,000: ', round(10000 * 10^B, 0), ' - ', round(10000 * 10^A, 0), 
            "\nso ", 
            ifelse(
              B > A,
              paste0( round(10000 * (10^B - 10^A),0), " more deaths"),
              paste0( round(10000 * (10^A - 10^B),0), " fewer deaths")
            )
          )                
        )
      
      p3 <- diffs %>% 
        mutate(birth_cohort = year - age) %>%
        filter(birth_cohort == this_cohort) %>%
        plot_ly(x = ~age, y = ~diff_lmr) %>%
        add_lines(showlegend = FALSE,
          hoverinfo = 'text',
          text = ~paste0(
            "Age: ", age, 
            '\nLog mortalities: ', round(B, 3), ' - ', round(A, 3), ' = ', round(diff_lmr, 3),
            '\nDeaths per 10,000: ', round(10000 * 10^B, 0), ' - ', round(10000 * 10^A, 0), 
            "\nso ", 
            ifelse(
              B > A,
              paste0( round(10000 * (10^B - 10^A),0), " more deaths"),
              paste0( round(10000 * (10^A - 10^B),0), " fewer deaths")
            )
          )                
        ) 
      
      p <- subplot(list(p1, p2, p3), shareY = TRUE) %>%
        layout(
          yaxis = list(
            title = "Difference in log mortalities",
            range = c(-absmax, absmax)
          ),
          xaxis = list(
            title = paste0("Difference by year at age ", this_age)
            ),
          xaxis2 = list(
            title = paste0("Difference by age in year ", this_year), 
            range = c(0, 100)
            ),
          xaxis3 = list(
            title = paste0("Difference by age for ", this_cohort, " birth cohort"),
            range = c(0, 100)
            ),
          title = paste0(
            "Differences in log mortality between population groups in year ", this_year, " and age ", this_age),
          showlegend = FALSE
        )
    }    
    return(p)
  })
  
  output$tadpole_plot <- renderPlotly({
    
    browser()
    
    dta_highlight <- hmd_e0 %>% 
      filter(code == input$tadpole_highlight) %>% 
      filter(gender == "female") %>% 
      filter(year >= input$period_limits[1] - 1, year <= input$period_limits[2]) %>% 
      arrange(year) %>% 
      mutate(delta_e0 = e0 - lag(e0)) %>% 
      mutate(newness = year - min(year)) %>% 
      mutate(newness = newness / max(newness))
    
    p <- dta_highlight %>% 
      plot_ly(data = ., x = ~e0, y = ~delta_e0, opacity = ~newness) %>% # doesn't work but some deeper level hack possible I think
      add_lines()

    return(p)
  })
  
  
})
