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




#TRES SERIES
mysubset_dep <- subset( panel_departamento,
                        panel_departamento$DEPARTAMENTO == "AMAZONAS")

b_dep <- xts(x = mysubset_dep[,c(3:5)] , order.by = mysubset_dep$FECHA_)

g_dep <- dygraph(b_dep, main = paste("Evolución del Covid-19 en el departamento de", "Amazonas")) %>%
        
        dyAxis("y", label = "Número de personas") %>%
        dyOptions(drawPoints = TRUE, pointSize = 2, axisLabelFontSize = 13, fillGraph = TRUE, fillAlpha = 0.25, drawGrid = TRUE) %>%
        dySeries("CASOS", color = my_colors[3], label = "Casos Covid") %>%
        dySeries("FALLECIDOS", color = "red", label = "Muertes Covid") %>%
        dySeries("FALLECIDOS_SINADEF", color = my_colors[1], label = "Muertes totales") %>%
        dyRangeSelector(dateWindow = c(min(panel_departamento$FECHA_), max(panel_departamento$FECHA_)) ,height = 30) %>%
        dyLegend(width = 150, labelsSeparateLines = TRUE)

g_dep

#dataWindow = c(min(panel_departamento$FECHA_), max(panel_departamento$FECHA_))


#DOS SERIES
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



#DATOS PANEL
mysubset_panel_dep1 <- subset( panel_departamento[ ,c(1:5)],
                               panel_departamento$DEPARTAMENTO == "AMAZONAS")

mysubset_panel_dep1$ANO <- substr(mysubset_panel_dep1$FECHA,1,4)
mysubset_panel_dep1$MES <- substr(mysubset_panel_dep1$FECHA,5,6)

mysubset_panel_dep1 <- mysubset_panel_dep1[ ,c(1,7,6,3,4,5)]
colnames(mysubset_panel_dep1) = c("Departamento", "Mes", "Año", "Casos", "Muertes Covid", "Muertes totales")
mysubset_panel_dep1

