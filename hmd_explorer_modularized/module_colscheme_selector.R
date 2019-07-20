
define_colbrew_pals <- function(category, colorblind){
  out <- RColorBrewer %>% 
    as_tibble(rownames = "pal_name") 
  
  if (!is.null(category)) {
    out <- out %>% filter(category == category)
  }
  if (!is.null(colorblind)) {
    out <- out %>% filter(colorblind == colorblind)
  }
  
  out
}

all_colbrew_pals <- brewer.pal.info %>% 
  as_tibble(rownames = "pal_name") %>% 
  pull(pal_name) %>% 
  str_c("RColorBrewer", ":", .)

create_colbrew_ramp <- function(palname){
  max_n <- brewer.pal.info %>% 
    as_tibble(rownames = "pal_name") %>% 
    filter(pal_name == palname) %>% 
    pull(maxcolors)
  
  colorRampPalette(brewer.pal(name = palname, n = max_n))
}


colscheme_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("select_colourscheme"),
                label = "Select colour scheme",
                choices = c(
                  "Viridis:viridis",
                  "Viridis:magma", 
                  "Viridis:plasma",
                  "Viridis:inferno",
                  "Viridis:cividis",
                  all_colbrew_pals
                )
    ),
    textOutput(ns("generic_message")),
    textOutput(ns("colourscheme_selected"))
  )
  
}

colscheme_selector_server <- function(input, output, session){
  browser()
  grab_colscheme <- reactive({input$select_colourscheme})
  output$generic_message      <- renderText({"Hellow from inner"})
  output$colourscheme_selected <- renderText({grab_colscheme()})
  
}
  