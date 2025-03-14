library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# Load the dataset
graduate_survey<- read.csv(file.choose(), header = T)

install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Test Dashboard"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Eduvos IT Graduate Survey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Programming Languages", tabName = "languages", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "frameworks", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Survey Summary", width = 12, status = "primary",
                    "This dashboard provides insights into the technology tools used by Eduvos IT graduates.")
              )
      ),
      
      # Programming Languages Tab
      tabItem(tabName = "languages",
              fluidRow(
                box(title = "Popular Programming Languages", width = 12, status = "info",
                    selectInput("lang_select", "Select Language:", choices = unique(survey_data$Programming_Language)),
                    plotlyOutput("lang_plot"))
              )
      ),
      
      # Databases Tab
      tabItem(tabName = "databases",
              fluidRow(
                box(title = "Databases Usage", width = 12, status = "success",
                    plotlyOutput("db_plot"))
              )
      ),
      
      # Web Frameworks Tab
      tabItem(tabName = "frameworks",
              fluidRow(
                box(title = "Web Frameworks Usage", width = 12, status = "warning",
                    DTOutput("framework_table"))
              )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Programming Languages Plot
  output$lang_plot <- renderPlotly({
    lang_data <- survey_data %>%
      filter(Programming_Language == input$lang_select) %>%
      count(Usage)
    
    plot_ly(lang_data, x = ~Usage, y = ~n, type = "bar", name = input$lang_select) %>%
      layout(title = paste("Usage of", input$lang_select))
  })
  
  # Databases Usage Plot
  output$db_plot <- renderPlotly({
    db_data <- survey_data %>%
      count(Database)
    
    plot_ly(db_data, labels = ~Database, values = ~n, type = "pie") %>%
      layout(title = "Database Popularity")
  })
  
  # Web Frameworks Table
  output$framework_table <- renderDT({
    survey_data %>%
      select(Name, Web_Framework) %>%
      datatable()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
