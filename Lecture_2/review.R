
#Recordando


library(shiny)


ui <- fluidPage() 

server <- function(input, output) {}

shinyApp(ui, server)



############################################


ui <- fluidPage(
       
        sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
        
        "then x times 5 is" ,
        
        textOutput("product")
        
) 

server <- function(input, output) {
        
        output$product <- renderText( input$x * 5 ) 
        
        
       # output$product <- renderText({
        #input$x * 5
        #}) 
        
}

shinyApp(ui, server)









#Customizing & publishing 

#orden: "rows_columns", "navlists", "pages", "tabs" y "shiny themes"



setwd("C:/Q_lab/Taller_Shiny/Taller2")

library(rio)
library(dplyr)

data <- import("fallecidos_covid.csv", encoding = "UTF-8")

data_= data %>%
        filter(DEPARTAMENTO=="LIMA" | 
               DEPARTAMENTO=="PIURA" |
               DEPARTAMENTO=="LA LIBERTAD" )

library("writexl")

write_xlsx(data_, "fallecidos_f.xlsx")
