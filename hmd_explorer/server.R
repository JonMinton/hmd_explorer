#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

full_data <- read_csv("data/hmd_data.csv")

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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$mort_surface <- renderPlotly({
    
    
    
    dta_ss <- full_data %>% 
      filter(code == input$code_select) %>% 
      filter(gender == input$gender_select) %>% 
      group_by(code, gender) %>% 
      nest()
    
    z_list <- dta_ss %>% 
      mutate(lmr_list = map(data, make_z_list, adjust = 0.5))
    
    p <-   plot_ly(
        x = ~z_list[["lmr_list"]][[1]][["age"]],
        y = ~z_list[["lmr_list"]][[1]][["year"]],
        z = ~z_list[["lmr_list"]][[1]][["vals"]]
      ) %>% add_surface()
    
    return(p)
  })
  
})
