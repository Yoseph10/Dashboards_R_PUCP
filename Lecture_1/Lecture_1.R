#install.packages("shiny")

library(shiny)


#####################################################
# INTRO: 
####################################################


#Plantilla (App)

ui <- fluidPage() 

server <- function(input, output) {}

shinyApp(ui = ui, server = server)




#Primera app

#user interface
ui <- fluidPage("¡Hola, Mundo!")

server <- function(input, output) {}

shinyApp(ui, server)



#####################################################
# APP EN ACCIÓN 
####################################################

#**************************
#Añadiendodo controles UI
#**************************

ui <- fluidPage(
        selectInput("dataset", label = "Bases de datos", choices = ls("package:datasets")),
        verbatimTextOutput("summary"), #displays code
        tableOutput("table")
)

server <- function(input, output) {}

shinyApp(ui, server)


#**************************
#Adición de comportamiento
#**************************


ui <- fluidPage(
        selectInput("dataset", label = "Bases de datos", choices = ls("package:datasets")),
        verbatimTextOutput("summary"),
        tableOutput("table")
)

server <- function(input, output) {
        #output$ID
        output$summary <- renderPrint({
                dataset <- get(input$dataset, "package:datasets")
                summary(dataset)
        })
        output$table <- renderTable({
                dataset <- get(input$dataset, "package:datasets")
                dataset
        })
}


shinyApp(ui, server)


#**************************
##Reactive expression (Expresión reactiva)
#**************************

ui <- fluidPage(
        selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
        verbatimTextOutput("summary"),
        tableOutput("table")
)

server <- function(input, output) {
        # Creamos una expresión reactiva
        
        dataset <- reactive({                                   #This is the essence of reactivity: outputs automatically react (recalculate) when their
                                                                #inputs change.
                
                get(input$dataset, "package:datasets") 
        })
        
        output$summary <- renderPrint({
                # Usa una expresión reactiva llamándola como una función
                summary(dataset())
        })
        
        output$table <- renderTable({
                dataset()
        })
}

shinyApp(ui, server)




#####################################################
# INPUTS y OUTPUTS 
####################################################

###########
##INPUTS
###########

ui <- fluidPage(
        # *Input() functions,
        # *Output() functions
        
        
) 

#**************************
#TEXTO LIBRE
#**************************

ui <- fluidPage(
         
        textInput("nombre", "¿Cuál es tu nombre?"),    #ID no deben tener espacios, puntos, guiones o caracteres especiales
                                                       #debe ser único
        passwordInput("contrasena", "Indique la contraseña"),
        
        textAreaInput("historia", "Cuéntame sobre ti", rows = 5),
        
        textInput("nombre2", "¿Cuál es tu color favorito?"),
)

server <- function(input, output) {}

shinyApp(ui, server)


#**************************
#INPUTS NUMÉRICOS
#**************************

ui <- fluidPage(
        numericInput("num", "Primer número", value = 0, min = 0, max = 100),
        sliderInput("num2", "Segundo número", value = 50, min = 0, max = 100),
        sliderInput("rng", "Rango", value = c(5, 15), min = 0, max = 100)
)

server <- function(input, output) {}

shinyApp(ui, server)

#?sliderInput


#**************************
#FECHAS
#**************************

ui <- fluidPage(
        dateInput("nac", "¿Cuánto naciste?", format = "dd-mm-yyyy", language = "es"),
        dateRangeInput("vac", "¿Cuándo te gustaría ir de vacaciones?", language = "es")
)

server <- function(input, output) {}

shinyApp(ui, server)

#?dateInput


#**************************
#OPCIONES LIMITADAS
#**************************

colores <- c("azul", "rosado", "negro", "amarillo", "otro", "Tengo varios")
#state.name
#data(state)

ui <- fluidPage(
        
        selectInput("state", "¿Cuál es tu estado favorito?", choices = c("Lima","Pueblo Libre", "San Borja"), multiple= FALSE),
        radioButtons("color", "¿Cuál es tu color favorito?", choices = colores, selected = "0" ),
        
        #Seleccionar múltiples alternativas
        checkboxGroupInput("color2", "¿Cuál es tu color favorito?", colores),
        
        #Sí/No
        checkboxInput("hijos", "¿Tienes hijos?", value = TRUE)
)

server <- function(input, output) {}

shinyApp(ui, server)


#**************************
#SUBIR ARCHIVOS
#**************************

ui <- fluidPage(
        fileInput("upload", NULL)
)

server <- function(input, output) {}

shinyApp(ui, server)


#**************************
#BOTONES DE ACCIÓN
#**************************

ui <- fluidPage(
        actionButton("click", "Click me!"),
        actionButton("drink", "Drink me!", icon = icon("cocktail"))
)

server <- function(input, output) {}

shinyApp(ui, server)


###########
##OUTPUTS y RENDER FUNCTIONS
###########

#**************************
#TEXTO
#**************************

ui <- fluidPage( 
        textOutput("text"),         #normal text
        verbatimTextOutput("code") #console output
)

server <- function(input, output) {
        
        output$text <- renderText("Hola, amigo!")
        
        output$code <- renderPrint( summary(1:10) )
}

shinyApp(ui, server)

#Tenga en cuenta que los {} solo son necesarios en las funciones de renderizado si necesita ejecutar varias líneas de código


#**************************
#TABLAS
#**************************

ui <- fluidPage(
        tableOutput("static"),
        dataTableOutput("dynamic")
)
server <- function(input, output) {
        
        output$static <- renderTable( head(mtcars) )
        
        output$dynamic <- renderDataTable( mtcars, options = list(pageLength = 15))
}

shinyApp(ui, server)


#**************************
#PLOTS
#**************************

ui <- fluidPage(
        plotOutput("plot", width = "400px")
)

server <- function(input, output) {
        output$plot <- renderPlot(plot(1:5), res = 96)
}

shinyApp(ui, server)


#**************************
#BOTONES DE ACCIÓN
#**************************

ui <- fluidPage(
        actionButton("clicks", 
                     "Botón")
)

server <- function(input, output) {
        
        observeEvent(input$clicks, {
                print(as.numeric(input$clicks))
        })
        
        
}

shinyApp(ui = ui, server = server)


#########################################
#BASIC REACTIVITY
########################################

#No tienes que decirle a Shiny cuando actualizar un output porque Shiny lo hace automáticamente


ui <- fluidPage(
        textInput("name", "¿Cuál es tu nombre?"),
        textOutput("greeting")
)
server <- function(input, output) {
        
        output$greeting <- renderText({
                
                paste0("Hola ", input$name, "!")
        })
}

shinyApp(ui, server)


#Con reactive expression
ui <- fluidPage(
        textInput("name", "¿Cuál es tu nombre?"),
        textOutput("greeting")
)
server <- function(input, output) {
        
        string <- reactive( paste0("Hola ", input$name, "!") )
        
        output$greeting <- renderText(string())
}

shinyApp(ui, server)


