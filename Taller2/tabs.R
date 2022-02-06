library(shiny)
library(tidyverse)


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



# 
# ui <- fluidPage(
#   
#   titlePanel("Tabsets"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
#     
#     mainPanel(
#       tabsetPanel(
#         tabPanel("Plot", plotOutput("plot")), 
#         tabPanel("Summary", verbatimTextOutput("summary")), 
#         tabPanel("Table", tableOutput("table"))
#       )
#     )
#   )
# )


#---------------------------
#Comenzamos a generar la app
#---------------------------

ui <- fluidPage(
  
  titlePanel("Tabsets"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        inputId="year_input",
        label="Selecciona la fecha",
        min= min(all_data$MES),
        max= max(all_data$MES), 
        value= max(all_data$MES), 
      ),
      
      selectInput(
        inputId="cities_input",
        label="Departamentos",
        choices=c("LIMA","PIURA","LA LIBERTAD"),
        selected=c("LIMA","PIURA","LA LIBERTAD"),
        multiple=TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Fallecidos", plotOutput("plot1")), 
        tabPanel("Variable 2", plotOutput("plot2")) 
      )
    )
  )
)


server<-function(input,output){
        
  output$plot1<-renderPlot(
    
    expr={
      
      plot_dat1<-filter(all_data,
                        DEPARTAMENTO %in% input$cities_input &
                          MES==input$year_input)
      
      ggplot(plot_dat1,aes(x=DEPARTAMENTO,y=fallecidos,fill=DEPARTAMENTO))+geom_bar(stat="identity")
    }
    
  )
  
  output$plot2<-renderPlot(
    
    expr={plot_dat2<-filter(all_data,
                            DEPARTAMENTO %in% input$cities_input &
                              MES==input$year_input)
    
    ggplot(plot_dat2,aes(x=DEPARTAMENTO,y=fallecidos,fill=DEPARTAMENTO))+geom_bar(stat="identity")}
    
  )
  
  
}

shinyApp(ui=ui,server=server)