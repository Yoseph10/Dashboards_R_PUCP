library(shiny)
library(plyr) 
library(tidyverse)
library(rio)


#---------------------------------------
#Cargamos los datos y realizamos cambios
#---------------------------------------

#importamos datos sobre los fallecidos por covid para los departamentos de Lima, Piura y La Libertad

setwd("C:/Q_lab/Taller_Shiny/Taller2")

all_data <- import("fallecidos_f.xlsx")

#Nos quedamos con el año y mes
all_data$FECHA <-substr(all_data$FECHA_FALLECIMIENTO, 1, 6)


#Hallamos la cantidad de fallecidos por mes y departamento
all_data <- all_data %>% 
        group_by(DEPARTAMENTO, FECHA) %>%
        summarise(fallecidos = n()) 

all_data <- as.data.frame(all_data)
colnames(all_data) <- c("DEPARTAMENTO", "FECHA", "fallecidos")

#Solo nos quedaremos con el año 2021
all_data <- filter(all_data,FECHA>="202101")

#creamos una variable para solo indicar el mes
all_data$MES <- substr(all_data$FECHA, 5, 6)

all_data$MES <- as.numeric(all_data$MES)


# library(shiny)
# 
# ui <- navbarPage("My Application",
#                  tabPanel("Component 1"),
#                  tabPanel("Component 2"),
#                  tabPanel("Component 3")
# )
# 
# server<-function(input,output){}
# 
# shinyApp(ui=ui,server=server)


#---------------------------
#Comenzamos a generar la app
#---------------------------

ui <- navbarPage(
  title="Mi aplicación",
  tabPanel("Componente 1",
           
           sliderInput(
             inputId="year_input",
             label="Selecciona la fecha",
             min= min(all_data$MES),
             max= max(all_data$MES), 
             value= max(all_data$MES), 
           ),
           
           plotOutput("plot1")
  ),
  
  tabPanel("Componente 2"),
  
  tabPanel("Componente 3")
)

server<-function(input,output){
  
  output$plot1<-renderPlot({
    
    plot_dat1<-filter(all_data,MES==input$year_input)
    
    ggplot(plot_dat1,aes(x=DEPARTAMENTO,y=fallecidos,fill=DEPARTAMENTO))+geom_bar(stat="identity")
  })
  
  
}

shinyApp(ui=ui,server=server)
