#http://rstudio.github.io/shinythemes/?_ga=2.132961481.679985627.1601843315-1966566028.1601473074

#install.packages("shinythemes")


library(shiny)
library(shinythemes)


ui <- fluidPage( theme = shinytheme("united"),
                 
  actionButton(inputId = "clicks", 
               label = "Click me")
)

server <- function(input, output) {
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
}

shinyApp(ui = ui, server = server)

### Utilizar un ejemplo anterior y cambiarle en shiny theme


