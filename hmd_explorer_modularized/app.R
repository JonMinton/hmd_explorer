
library(shiny)
source("global.R")

ui <- fluidPage(
  titlePanel("Human Mortality Database Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("tab_active"),
      

      uiOutput("sidebar_select")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", id = "main_tabset", 
        tabPanel("individual",         title = "Individual Population",
          make_graphics_ui("graphics_module_singular-singular")         
        ),
        tabPanel("group",              title = "Group of Populations",
          make_graphics_ui("graphics_module_group-singular")       
        ),
        tabPanel("sex_compare",        title = "Comparison between Genders",
          make_graphics_ui("graphics_module_singular-comparative")       
        ),
        tabPanel("group_compare",      title = "Comparison between Populations",
          make_graphics_ui("graphics_module_group-comparative")       
        ),
        tabPanel("corr_individual",    title = "Correlation in trends by age",
                 make_corrmaps_ui("corrmaps_singular-singular")       
        )
      )
    )
  )
)

server <- function(input, output) {
  get_active_tab <- reactive({input$main_tabset})
  
  output$sidebar_select <- renderUI({

    switch(get_active_tab(),
           "Individual Population"            = select_data_ui("data_module1_singular"),
           "Group of Populations"             = select_data_ui("data_module1_singular", allow_multiple = TRUE),
           "Comparison between Genders"       = select_data_ui("data_module1_singular", select_sex = FALSE),
           "Comparison between Populations"   = tagList(
                                                  select_data_ui("data_module1_singular"),
                                                  select_data_ui("data_module2_singular")
                                              ),
           "Correlation in trends by age"     = select_data_ui("data_module1_singular")
    )
    
  })
  
  
  
  data1_sexcompare <- callModule(select_data_server, "data_module1_singular", mode = "sex-compare")
  data1_singular   <- callModule(select_data_server, "data_module1_singular"                      )
  data2_singular   <- callModule(select_data_server, "data_module2_singular"                      )

  
  callModule(make_graphics_server, "graphics_module_singular-singular", 
             mode = "singular-singular", data = data1_singular)
  callModule(make_graphics_server, "graphics_module_group-singular", 
             mode = "group-singular",    data = data1_singular)
  callModule(make_graphics_server, "graphics_module_singular-comparative", 
             mode = "sex-compare",       data = data1_sexcompare)
  callModule(make_graphics_server, "graphics_module_group-comparative", 
             mode = "group-comparative", data = data1_singular)
  
  callModule(make_corrmaps_server, "corrmaps_singular-singular", 
                                         data = data1_singular)
  
  
}




shinyApp(ui, server)
