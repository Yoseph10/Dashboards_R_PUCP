library(shiny)
library(plyr) 
library(tidyverse)
library(rio)
library(shinythemes)

#---------------------------------------
#Cargamos los datos y realizamos cambios
#---------------------------------------

#importamos datos sobre los fallecidos por covid para los departamentos de Lima, Piura y La Libertad
#Más info: https://www.datosabiertos.gob.pe/group/datos-abiertos-de-covid-19

#setwd("C:/Q_lab/Taller_Shiny/Taller2")

all_data <- import("fallecidos_f.xlsx")  #datos actualizados hasta el 14 de noviembre

#Nos quedamos con el año y mes
all_data$FECHA <-substr(all_data$FECHA_FALLECIMIENTO, 1, 6)


#Hallamos la cantidad de fallecidos por mes y departamento
all_data <- all_data %>% 
        group_by(DEPARTAMENTO, FECHA) %>%
        summarise(fallecidos = n()) 

all_data <- as.data.frame(all_data)
colnames(all_data) <- c("DEPARTAMENTO", "FECHA", "fallecidos")

#convertinos el año y mes en formato "yearmon"
library(zoo)
all_data$ym <- as.yearmon(as.character(all_data$FECHA), "%Y%m")

#convertimos ym en formato "Date"
all_data$FECHA_ <- as.Date(all_data$ym)


#Skeleton UI

# ui<- fluidPage(
#   fluidRow(
#     column(4,
#            inputPanel(
#              sliderInput(...),
#              selectInput(...)
#            )),
#     column(4,
#            plotOutput(...)
#           ),
#     column(4,
#            tableOuput(...)
#            )
#   ),
#   fluidRow(
#     column(4,
#            inputPanel(
#              sliderInput(...),
#              selectInput(...)
#            )),
#     column(4,
#            plotOutput(...)
#     ),
#     column(4,
#            tableOuput(...)
#     )
#   )
#   
#  
# )



#---------------------------
#Comenzamos a generar la app
#---------------------------

ui <- fluidPage( 
  
  titlePanel("Multicolumn Format"),
  
  h2("Estos son los mismos inputs y figuras repetidos en dos filas."),

    fluidRow(
            
    column(5,
           inputPanel(
             sliderInput(inputId="input_year",
                         label="Selecciona la fecha",
                         min= min(all_data$FECHA_ ),
                         max= max(all_data$FECHA_ ),
                         value= c(min(all_data$FECHA_ ),max(all_data$FECHA_ )),
                         timeFormat= "%b %Y"),

             selectInput(inputId="city1",
                         label="¿Qué departamento quieres seleccionar?",
                         choices=c("LIMA","PIURA","LA LIBERTAD"),
                         selected=c("LIMA","PIURA","LA LIBERTAD")
             )
           )
    ),
    
    column(3,
           plotOutput("plot1")
    ),
    
    column(4,
           tableOutput("table1")
    )
  ),
  
  fluidRow(
    column(4,
           inputPanel(
             sliderInput(inputId="input_year2",
                         label="Selecciona la fecha",
                         min= min(all_data$FECHA_ ),
                         max= max(all_data$FECHA_ ),
                         value= c(min(all_data$FECHA_ ),max(all_data$FECHA_ )),
                         timeFormat= "%b %Y"),
             
             selectInput(inputId="city2",
                         label="¿Qué departamento quieres seleccionar?",
                         choices=c("LIMA","PIURA","LA LIBERTAD"),
                         selected=c("LIMA","PIURA","LA LIBERTAD")
             )
           )
    ),
    
    column(4,
           plotOutput("plot2")
    ),
    
    column(4,
           tableOutput("table2")
    )
  )
)

#Escribe la functión del servidor que te va a permitir generar los plots y las tablas que serán añadidos en los outputs

server <- function(input, output){
  
  output$table1 <- renderTable(
    
    {
      table_data1<-filter(
        all_data,
        DEPARTAMENTO==input$city1 & FECHA_>=input$input_year[1] & FECHA_<=input$input_year[2])
      
      table_data1 <- table_data1%>%select(DEPARTAMENTO,FECHA,fallecidos)
      table_data1
      
    }
  )
  
  output$plot1 <- renderPlot({
    
    plot_dat1<-filter(
      all_data,
      DEPARTAMENTO==input$city1 & FECHA_>=input$input_year[1] & FECHA_<=input$input_year[2])
    
    ggplot(plot_dat1,
           aes(x=FECHA_,y=fallecidos,group=DEPARTAMENTO))+
      geom_line()
    
  })
  
  
  output$table2 <- renderTable(
    
    {
      table_data2<-filter(
        all_data,
        DEPARTAMENTO==input$city2 & FECHA_>=input$input_year2[1] & FECHA_ <=input$input_year2[2])
      
      table_data2 <- table_data2%>%select(FECHA,fallecidos)
      table_data2
      
    }
  )
  
  output$plot2 <- renderPlot({
    
    plot_dat2<-filter(
      all_data,
      DEPARTAMENTO==input$city2 & FECHA_>=input$input_year2[1] & FECHA_ <=input$input_year2[2])
    
    ggplot(plot_dat2,
           aes(x=FECHA_,y=fallecidos,group=DEPARTAMENTO))+
      geom_line()
    
  })
}

# Corremos la aplicación 
shinyApp(ui = ui, server = server)