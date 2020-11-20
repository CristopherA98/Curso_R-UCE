# install.packages("highcharter")

library(shiny)
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cantidad RadioBases Ecuador - Julio 2019"),
    sidebarLayout(
        sidebarPanel(
            em("Esta aplicación muestra la cantidad de RadioBases que los operadores de Ecuador 
            (Claro, Movistar y CNT) han desplegado desde Octubre del 2015 hasta Julio del 2019"),
            hr(),
            strong("Instrucciones:"),
            p("1. Escoja una opción de los grupos"),
            p("2. Escoja uno de los tipos de mediciones"),
            selectInput("group_in", "Seleccione un grupo:", 
                        c("Año","Operador")),
            selectInput("meas_in", "Seleccione la medición:", 
                        c("Promedio", "Total")),
            p("Se generarán tablas y gráficos según las opciones escogidas en los pasos 1 y 2"),
            hr(),
            p("En la pestaña 'Tabla General' se presenta la lista total"),
            hr(),
            p("En la pestaña 'Mapa' se presenta el total de Radiobases por provincia en el mapa de Ecuador")
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Reportes", br(), 
                                 tableOutput("data"), plotOutput("plot1")),
                        tabPanel("Tabla General", br(), 
                                 div(DTOutput("tabla"), style = "font-size: 80%")),
                        tabPanel("Mapa", br(),
                                 highchartOutput("mapa")
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # descarga de archivo 
    datos <- read.xlsx("http://www.arcotel.gob.ec/wp-content/uploads/2018/11/1.2-Radiobases-por-operador-y-tecnologia-nivel-provincial_Jul-2019.xlsx",
        sheet = "RBSxPARROQUIAHISTORICO", startRow = 12)
    
    # eliminación de filas y columnas que no se requieren
    datos <- datos[1:1044, -4]
    
    # columnas que son variables y que deben ser cambiadas
    RadioBaseEc <- datos %>% gather("tecnologia", "cantidad", -c(1:3))
    
    # cambio de nombre de las 3 primeras columnas
    colnames(RadioBaseEc)[1:3] <- c("provincia", "canton", "parroquia")
    
    # adición de la variable fecha
    RadioBaseEc$fecha <- Sys.Date()
    report.date <- as.Date("2015-09-01")
    i <- 1
    j <- 13572
    k <- 1
    while (j <= 298584) {
        RadioBaseEc$fecha[i:j] <- report.date + 31*k
        i <- i + 13572
        j <- j + 13572
        k <- k + 1
    }
    
    report.date <- as.Date("2017-07-01")
    i <- 298585
    j <- 314244
    k <- 1
    while (j <= 674424) {
        RadioBaseEc$fecha[i:j] <- report.date + 31*k
        i <- i + 15660
        j <- j + 15660
        k <- k + 1
    }
    
    # adición de la variable operador para CLARO
    RadioBaseEc$operador <- "OPERADOR"
    i <- 1
    j <- 5220
    while (j <= 298584) {
        RadioBaseEc$operador[i:j] <- "CLARO"
        i <- i + 13572
        j <- j + 13572
    }
    
    i <- 298585
    j <- 304848
    while (j <= 674424) {
        RadioBaseEc$operador[i:j] <- "CLARO"
        i <- i + 15660
        j <- j + 15660
    }
    
    # adición de la variable operador para MOVISTAR
    i <- 5221
    j <- 10440
    while (j <= 298584) {
        RadioBaseEc$operador[i:j] <- "MOVISTAR"
        i <- i + 13572
        j <- j + 13572
    }
    
    i <- 304849
    j <- 311112
    while (j <= 674424) {
        RadioBaseEc$operador[i:j] <- "MOVISTAR"
        i <- i + 15660
        j <- j + 15660
    }
    
    # adición de la variable operador para CNT
    i <- 10441
    j <- 13572
    while (j <= 298584) {
        RadioBaseEc$operador[i:j] <- "CNT"
        i <- i + 13572
        j <- j + 13572
    }
    
    i <- 311113
    j <- 314244
    while (j <= 674424) {
        RadioBaseEc$operador[i:j] <- "CNT"
        i <- i + 15660
        j <- j + 15660
    }
    
    # limpieza de nombres de la variable tecnologia
    RadioBaseEc$tecnologia <- gsub("(.00).*","\\1", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("(.50).*","\\1", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("(.*WS).*","\\1", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("LTE.1700","LTE.AWS", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("[(]","", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("GSM.851","GSM.850", RadioBaseEc$tecnologia)
    RadioBaseEc$tecnologia <- gsub("UMTS1900","UMTS.1900", RadioBaseEc$tecnologia)
    
    # cambio a factor a la variable tecnologia
    RadioBaseEc$tecnologia <- factor(RadioBaseEc$tecnologia, 
                                     levels = c("GSM.850", "GSM.1900", "UMTS.850",
                                                "UMTS.1900", "LTE.700", "LTE.850",
                                                "LTE.AWS", "LTE.1900"))
    
    # cambio de orden de las columnas
    RadioBaseEc <- RadioBaseEc[, c(1, 2, 3, 6, 7, 4, 5)]
    
    # separar el año
    RadioBaseEc <- separate(RadioBaseEc, fecha, c("año", "mes"))
    
    # tablas y reportes
    reporte1 <- RadioBaseEc %>% group_by(provincia, año, mes, operador, tecnologia) %>%
        summarize(suma = sum(cantidad))
    
    RadioBaseEc$año <- as.integer(RadioBaseEc$año)
    by_anio <- RadioBaseEc %>% group_by(año) %>% 
        summarize(total_radiobases = sum(cantidad),
                  media_radiobases = mean(cantidad))
    
    plot_by_anio_total <- by_anio %>% ggplot(aes(año, total_radiobases)) +
        geom_line(color = "blue") +
        xlab("Año") + ylab("Total de Radiobases") +
        ggtitle("Total Radiobases por Año") +
        theme(plot.title = element_text(hjust = 0.5))
    
    plot_by_anio_mean <- by_anio %>% ggplot(aes(año, media_radiobases)) +
        geom_line(color = "blue") +
        xlab("Año") + ylab("Promedio de Radiobases") +
        ggtitle("Promedio Radiobases por Año") +
        theme(plot.title = element_text(hjust = 0.5))
    
    by_operador <- RadioBaseEc %>% group_by(operador) %>% 
        summarize(total_radiobases = sum(cantidad),
                  media_radiobases = mean(cantidad))
    
    plot_by_operador_total <-  by_operador %>% ggplot(aes(operador, total_radiobases, 
                                                          fill = operador)) +
        geom_col() +
        xlab("Operador") + ylab("Total de Radiobases") +
        ggtitle("Total Radiobases por Operator") +
        theme(plot.title = element_text(hjust = 0.5))
    
    plot_by_operador_mean <-  by_operador %>% ggplot(aes(operador, media_radiobases, 
                                                         fill = operador)) +
        geom_col() +
        xlab("Operador") + ylab("Promedio de Radiobases") +
        ggtitle("Promedio de Radiobases por Operador") +
        theme(plot.title = element_text(hjust = 0.5))
    
    # mapa
    provincias <- RadioBaseEc %>% group_by(provincia) %>% 
        summarize(suma.t = sum(cantidad))
    
    mapdata <- get_data_from_map(download_map_data("countries/ec/ec-all"))
    data_ecuador <- mapdata %>% 
        select(PROVINCIA = `hc-a2`) %>% 
        mutate(X = c(195735, 17482, 5014, 15304, 7071, 16619, 18442, 31332,
                     19800, 14432, 2316, 5876, 4010, 12891, 20372, 52237,
                     15301, 2834, 226122, 2815, 23916, 6050, 3877, 	2811, 499))
    mapa1 <- hcmap("countries/ec/ec-all", data = data_ecuador, value = "X",
                   joinBy = c("hc-a2", "PROVINCIA"),
                   dataLabels = list(enabled = TRUE, format = '{point.name}'))  
    
    output$tabla <- renderDT({
        reporte1
    })
    
    output$mapa <- renderHighchart({
        mapa1
    })
    
    output$data <- renderTable({
        if(input$group_in == "Año") {
            by_anio
        }
        else {
            if (input$group_in == "Operador") {
                by_operador
            }
        }
    })
    
    output$plot1 <- renderPlot({
        if(input$group_in == "Año") {
            if(input$meas_in == "Total") {
                plot_by_anio_total
            }
            else {
                plot_by_anio_mean
            }
        }
        else {
            if (input$group_in == "Operador") {
                if(input$meas_in == "Total") {
                    plot_by_operador_total
                }
                else {
                    plot_by_operador_mean
                }
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)