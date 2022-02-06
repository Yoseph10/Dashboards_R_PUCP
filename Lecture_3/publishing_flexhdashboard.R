#install.packages("rsconnect")

library(rsconnect)

#Info que se encuentra en shinyapps.io (Account -> Tokens)
rsconnect::setAccountInfo(name='cglmjx-yoseph10', token='', secret='')

#Se puede omitir el argumento "appName". Por defecto, el nombre de la app será el título del Rmd
#Es necesario especificar el "account" si es que tienes más de 1 cuenta vicunlada en tu R

rsconnect::deployApp("C:/Q_lab/Taller_Shiny/Taller3/app_publishing/app.Rmd", appName = "App2" , account = "cglmjx-yoseph10")

