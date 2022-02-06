library(shiny)
library(tidyverse)
library(shinythemes)
library(zoo)
library(xts) # To make the convertion data-frame / xts format
library(dygraphs)
library(htmlwidgets)

library(webshot2) #install.packages("remotes")
                  #remotes::install_github("rstudio/webshot2")

library("RColorBrewer")   #display.brewer.all()
library(DT)
library(ggplot2)

#install.packages(c("zoo", "xts", "dygraphs", "htmlwidgets", "DT", "ggplot2", "RColorBrewer" ))

################################################################################
#Limpieza y manejo de datos
################################################################################

setwd("C:/Q_lab/Taller_Shiny/Taller4/my_app")

departamento<- read.csv("covid_departamento_final.csv")
departamento <- departamento[,-1] #Delete first column

panel_departamento<- read.csv("_departamento_panel_.csv")
panel_departamento<- panel_departamento[,-1] #Delete first column

#convertinos el ano y mes en formato "yearmon"
panel_departamento$ym <- as.yearmon(as.character(panel_departamento$FECHA), "%Y%m")

#convertimos ym en formato "Date"
panel_departamento$FECHA_ <- as.Date(panel_departamento$ym)

#Colores para series de tiempo
my_colors <- RColorBrewer::brewer.pal(3, "Dark2")[1:3]  #Verde, naraja, morado


#----------------------------------------------
#CREANDO LA APP
#----------------------------------------------


ui <- fluidPage( theme = shinytheme("journal"),
                 
                 titlePanel("QLAB-COVID"),
                 
                 sidebarLayout(
                         
                         #Primer elemento del sidebarLayout
                         sidebarPanel(
                                 
                                 radioButtons(inputId="button",label="Nivel de información:",choices=c("General", "Departamental"))
                                 , width = 2,
  
                                 style="text-align:center;color:black"
                         ),
                         
                         
                         #Segundo elemento del sidebarLayout
                         mainPanel(
                                 
                                 tabsetPanel(id= "tabs",
                                             
                                             #Info general
                                             tabPanel("Información general",
                                                      
                                                      fluidRow(

                                                                     br(),
                                                                     br(),
                                                                     
                                                                     p("QLAB-PUCP presenta su dashboard para analizar la evolución del Covid-19 en el Perú. 
                                                                       El dashboard permite visualizar y descargar información desagregada sobre la incidencia de la epidemia del Covid-19 en el país.
                                                                       Los niveles de agregación disponibles son departamental, provincial y distrital. 
                                                                       Para estos tres niveles, es posible visualizar la evolución de diversos indicadores, así como descargar los datos panel mensuales. 
                                                                       Con este dashboard, QLAB-PUCP busca facilitar el acceso a información sobre la epidemia en el país y así promover la investigación científica.", 
                                                                       style="text-align:justify;color:black;background-color:#fcebeb;padding:15px;border-radius:10px"),
                                                                     
                                                              
                                                      )
                                             ),
                                             

                                             #A nivel departamental
                                             #DEPARTAMENTO
                                             tabPanel("Departamento", 
                                                      
                                                           
                                                      br(),
                                                                     
                                                      selectizeInput('select_dep', 'Departamento:', choices = c(departamento$DEPARTAMENTO)),
                                                              
                                                      
                                                      radioButtons(inputId="dep_button",label="Elige lo que desees visualizar:",choices=c("Serie de tiempo", "Datos panel")),
                                                  
                                                      
                                                      #conditional panel 1
                                                      conditionalPanel(
                                                              condition = "input.dep_button == 'Serie de tiempo'",
         
                                                                             br(),
                                                                             sliderInput(inputId="dep_year",
                                                                                         label="Selecciona el rango de tiempo:",
                                                                                         min= min(panel_departamento$FECHA_),
                                                                                         max= max(panel_departamento$FECHA_),
                                                                                         value= c(min(panel_departamento$FECHA_),max(panel_departamento$FECHA_)),
                                                                                         width='50%',
                                                                                         timeFormat= "%b %Y"),
                                                                             
                                                                             radioButtons(inputId="dep_grafico",label="Elige el gráfico:",choices=c("Evolución del Covid-19", "Evolución de muertes")),

                                                                             br(),
                                                                             dygraphOutput("serie_departamento") #ojo coma

                                                      ),
                                                      
                                                      #conditional panel 2
                                                      conditionalPanel(
                                                              condition = "input.dep_button == 'Datos panel'",

                                                                             br(),br(),
                                                                             dataTableOutput("data_panel_dep1")

                                                      ), 
                                                      
                                             ),
       
                                 )
                                 , style="text-align:justify;color:black")
                 )
)


server <- function(input, output, session) {
        
        
        ##################################
        #sidebarLayout
        ##################################
        
        ## observe the button being pressed
        observeEvent(input$button, {
                
                if(input$button == "Departamental"){
                        
                        showTab(inputId = "tabs", target = "Departamento")

                        hideTab(inputId = "tabs", target = "Información general")

                } else{
                        showTab(inputId = "tabs", target = "Información general")

                        hideTab(inputId = "tabs", target = "Departamento")

                }
        })
        

        ##################################
        #Tab: Departamento
        ##################################
        
        #Serie de tiempo
        
        #Tres series
        dep_create_dygraph <- reactive({
                
                mysubset_dep <- subset( panel_departamento,
                                        panel_departamento$DEPARTAMENTO == input$select_dep)
                
                b_dep <- xts(x = mysubset_dep[,c(3:5)] , order.by = mysubset_dep$FECHA_)
                
                g_dep <- dygraph(b_dep, main = paste("Evolución del Covid-19 en el departamento de", input$select_dep)) %>%
                        dyAxis("y", label = "Número de personas") %>%
                        dyOptions(drawPoints = TRUE, pointSize = 2, axisLabelFontSize = 13, fillGraph = TRUE, fillAlpha = 0.25, drawGrid = TRUE) %>%
                        dySeries("CASOS", color = my_colors[3], label = "Casos Covid") %>%
                        dySeries("FALLECIDOS", color = "red", label = "Muertes Covid") %>%
                        dySeries("FALLECIDOS_SINADEF", color = my_colors[1], label = "Muertes totales") %>%
                        dyRangeSelector(dateWindow = c(input$dep_year),height = 30) %>%
                        dyLegend(width = 150, labelsSeparateLines = TRUE)
                
                g_dep
                
        })
        
        
        #Dos series
        dep_create_dygraph1 <- reactive({
                
                mysubset_dep1 <- subset( panel_departamento,
                                         panel_departamento$DEPARTAMENTO == input$select_dep)
                
                b_dep1 <- xts(x = mysubset_dep1[,c(4:5)] , order.by = mysubset_dep1$FECHA_)
                
                g_dep1 <- dygraph(b_dep1, main = paste("Evolución de muertes en el departamento de", input$select_dep) ) %>%
                        dyAxis("y", label = "Número de personas") %>%
                        dyOptions(drawPoints = TRUE, pointSize = 2, axisLabelFontSize = 13, fillGraph = TRUE, fillAlpha = 0.25, drawGrid = TRUE) %>%
                        dySeries("FALLECIDOS", color = "red", label = "Muertes Covid") %>%
                        dySeries("FALLECIDOS_SINADEF", color = my_colors[1],label = "Muertes totales") %>%
                        dyRangeSelector(dateWindow = c(input$dep_year),height = 30) %>%
                        dyLegend(width = 150, labelsSeparateLines = TRUE)
                
                g_dep1
                
        })
   
        output$serie_departamento  <- renderDygraph({
                
                if(input$dep_grafico == "Evolución del Covid-19"){
                        
                        dep_create_dygraph()
                        
                }
                else{
                        dep_create_dygraph1()
                        
                }
                
        })
        
        #Datos panel
        
        panel_dep1 <- reactive({
                
                mysubset_panel_dep1 <- subset( panel_departamento[ ,c(1:5)],
                                               panel_departamento$DEPARTAMENTO == input$select_dep)
                
                mysubset_panel_dep1$ANO <- substr(mysubset_panel_dep1$FECHA,1,4)
                mysubset_panel_dep1$MES <- substr(mysubset_panel_dep1$FECHA,5,6)
                
                mysubset_panel_dep1 <- mysubset_panel_dep1[ ,c(1,7,6,3,4,5)]
                colnames(mysubset_panel_dep1) = c("Departamento", "Mes", "Año", "Casos", "Muertes Covid", "Muertes totales")
                mysubset_panel_dep1
                
        })
        
        output$data_panel_dep1 <- renderDataTable(
                
                panel_dep1()
        )
        
        
}


shinyApp(ui = ui, server = server)