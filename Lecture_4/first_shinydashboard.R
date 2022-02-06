#install.packages("shinydashboard")


#Blank dashboard
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
        
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
  
)

server <- function(input, output) { }

shinyApp(ui, server)




#Basic dashboard

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
        
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(),
  
  dashboardBody(
          
    # Boxes need to be put in a row (or column)
    fluidRow(
            
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
    
  )
  
)

server <- function(input, output) {
        
  set.seed(122)
        
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
          
    data <- histdata[seq_len(input$slider)]
    
    hist(data)
    
  })
}

shinyApp(ui, server)



## Final example

library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin = "red",
                     
   #primer elemento   
  dashboardHeader(title = "Basic dashboard"),
  
  #segundo elemento
  dashboardSidebar(
          
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("New Item", tabName = "Item3", icon = icon("th"))
    )
    
  ),
  
  #tercer elemento
  dashboardBody(
          
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              
              h2("Widgets tab content")
              
      )
    )
    
  )
  
)

server <- function(input, output) {
        
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
          
    data <- histdata[seq_len(input$slider)]
    hist(data)
    
  })
  
}

shinyApp(ui, server)


#skinks = blue, black, purple, green, red, yellow

