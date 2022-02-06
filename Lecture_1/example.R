library(tidyverse)
library(shiny)
library(plotly) #instalar

#------------------------
#Creamos datos ficticios
#------------------------

countries=c("USA","China","Sudan")

all_data<- data.frame(
        "country" = rep(countries, each=5),
        "year" = rep( c(1990, 1991, 1992, 1993, 1994), 3 ), 
        "var1" = round( runif(15, min = 0, max = 5), 0 ),
        "var2" = round( runif(15, min = 0, max = 5), 0 ),
        "var3" = round( runif(15, min = 0, max = 5), 0 )
)


#---------------------------
#Comenzamos a generar la app
#---------------------------

ui <- fluidPage(
        
        h1("Primer ejemplo"),
        br(),br(),
        
        #INPUT (year1)
        h2("Slider Input con dos Sliders"),
        
        sliderInput(inputId="year1",
                    label="Selecciona el año",
                    min=1990,
                    max=1994,
                    value=c(1990,1994),
                    sep=""),  
        
        #INPUT (country1)
        h2("Checkbox Input"),
        
        checkboxGroupInput(inputId="country1",
                           label="¿Qué país te gustaría mostrar?",
                           choices=countries,
                           selected=countries),
        
        #OUTPUT (table1)
        h2("Una tabla estándar"),
        
        tableOutput(outputId = "table1"),

        #OUTPUT (table2)
        h2("Una tabla de datos"),
        
        dataTableOutput(outputId = "table2"),

        #IOUTPUT (plot1)
        h2("Un plot estándar"),
        
        plotOutput(outputId = "plot1"),

        #OUTPUT (plot2)
        h2("Un plotly plot"),
        
        plotlyOutput(outputId = "plot2")          #Se necesita la librería "plotly"
)


#Escribe la functión del servidor que te va a permitir generar los plots y las tablas que serán añadidos en los outputs

server <- function(input, output) {
        
        output$table1 <- renderTable({
                filter(all_data,country %in% input$country1 & year>=input$year1[1] & year<=input$year1[2])})
        
        output$table2 <- renderDataTable({
                filter(all_data,country %in% input$country1 & year>=input$year1[1] & year<=input$year1[2])})
        
        output$plot1 <- renderPlot({
                ggplot(filter(all_data,country %in% input$country1 & year>=input$year1[1] & year<=input$year1[2]),aes(x=var1,y=var2))+
                        geom_point()})
        
        output$plot2 <- renderPlotly({
                ggplot(filter(all_data,country %in% input$country1 & year>=input$year1[1] & year<=input$year1[2]),aes(x=var1))+
                        geom_histogram()})
}

# Corremos la aplicación 
shinyApp(ui = ui, server = server)