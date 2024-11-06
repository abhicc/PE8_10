library(tidyverse)   
library(shiny)
library(Lock5Data)
library(DT)
library(shinydashboard)

ui10 <- dashboardPage(
  
  dashboardHeader(title = "US States Summaries from 2014", titleWidth = 400),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Income", tabName = "income"),
      menuItem("Politics", tabName = "politics"),
      menuItem("Other Demographics", tabName = "demographics",
               menuSubItem("Health", tabName = "health"),
               menuSubItem("Education", tabName = "education"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "income",
              fluidRow(
                box(plotOutput("histplot", height = 250), width = 8),
                
                box(width = 4,
                    sliderInput(inputId = "number_bins", 
                                label = "Choose number of bins",
                                min = 20, 
                                max = 40,
                                value = 30
                    )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "politics",
              fluidRow(
                box(plotOutput("barplot", height = 250), width = 8),
                
                box(width = 4,
                    selectInput(inputId = "region", 
                                label = "Select a US region",
                                choices = c("Mid West" = "MW", "North East" = "NE", "South" = "S", "West" = "W"), 
                                selected = "MW"
                    )
                )
              )
      ),
      
      tabItem(tabName = "health",
              fluidRow(
                box(plotOutput("scatterplot", height = 250), width = 8),
                
                box(width = 4,
                    varSelectInput(inputId = "x_var", 
                                   label = "Select x-axis variable",
                                   data = USStates2e %>% dplyr::select(PhysicalActivity, HeavyDrinkers, Smokers),
                                   selected = "PhysicalActivity"
                    )
                )
              )
      ),
      
      tabItem(tabName = "education",
              fluidRow(
                box(DT::dataTableOutput("table"), width = "100%"),
              )
      )
      
      
      
    )
  )
)

server10 <- function(input, output) {
  
  output$histplot <- renderPlot({
    
    USStates2e %>% 
      ggplot() +
      geom_histogram(aes(x = HouseholdIncome), bins = input$number_bins, color = "white", fill = "blue")
    
  })
  
  output$barplot <- renderPlot({
    
    USStates2e %>% filter(Region == input$region) %>% 
      ggplot() +
      geom_bar(aes(y = ObamaRomney), fill = "purple") +
      labs(x = "States won by Obama and Romney", y = "") +
      scale_y_discrete(labels = c("Obama", "Romney"))
    
    
  })
  
  output$scatterplot <- renderPlot({
    
    USStates2e %>% 
      ggplot +
      geom_point(aes(x = !!input$x_var, y = Obese))
    
  })
  
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(USStates2e %>% 
                    group_by(Region) %>% 
                    summarize(Averegae_HSGrad_Percentage = round(mean(HighSchool, na.rm = TRUE), 2),
                              Average_CollegeGrad_Percentage = round(mean(College, na.rm = TRUE), 2)))
    
  })
}

# Run the app
shinyApp(ui = ui10, server = server10)
