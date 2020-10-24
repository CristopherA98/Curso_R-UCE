####################################
###       Curso R - UCE          ###
###          Clase 05            ###
####################################


# Visualización de datos  -------------------------------------------------

# Prerrequisitos

packages <- c('ggplot2','gifski','gganimate','tidyquant','tidyverse')
# install.packages(packages)

library(easypackages) #easypackages me permite cargar varios paquetes a la vez
libraries(packages)
data(iris)

# 1. Componentes de un gráfico --------------------------------------------

## 1.1. Fuentes de datos
ggplot(data = iris)
iris <- data.frame(iris)
iris <- tibble::as_tibble(iris)
iris
## 1.1. Geometría
ggplot(iris) + geom_histogram()
# Gráfico de dispersión
ggplot(iris,aes(x=Sepal.Width,y= Sepal.Length))+ 
        geom_point() 
# Gráfico con líneas de tendencia simulando regresión
ggplot(iris) + geom_histogram()
ggplot(iris,aes(x=Sepal.Width,y= Sepal.Length,color=Species))+ 
        geom_smooth()+
        geom_point()
# Gráfico separados dependiendo las categorías de una variable
ggplot(iris,aes(x=Sepal.Width,y= Sepal.Length,color=Species))+ 
        geom_smooth()+
        geom_point()+
        facet_wrap(~Species)
# Gráfico separados con escalas dependiendo a sus datos
ggplot(iris) + geom_histogram()
ggplot(iris,aes(x=Sepal.Width,y= Sepal.Length,color=Species))+ 
        geom_smooth()+
        geom_point()+
        facet_wrap(~Species,scales = 'free_y')
# Gráfico de cajas y bigotes
ggplot(iris) + geom_histogram()
ggplot(iris,aes(x=Sepal.Width,y= Sepal.Length,color=Species))+ 
        geom_boxplot()+
        theme(legend.position = "none") #posiion de la leyenda
# Histogramas
ggplot(data=iris,aes(x=Sepal.Length)) + 
        geom_histogram()#aes(y = ..density..)

ggplot(data=iris,aes(x=Sepal.Length)) + 
        geom_histogram(aes(y = ..density..))
        # geom_density(lwd=0.8)
                
# Histograma dependiendo las categorias de la variable
ggplot(data=iris,aes(x=Sepal.Length,fill=Species)) + 
        geom_histogram()

ggplot(data=iris,aes(x=Sepal.Length,fill=Species)) + 
        geom_histogram()+
        facet_wrap(~Species)
# Distribución de densidad
ggplot(data=iris,aes(x=Sepal.Length,color=Species)) + 
        geom_density()

ggplot(data=iris,aes(x=Sepal.Length)) + 
        geom_density(alpha=0.4,lwd=0.2, fill= "#08C25A")+
        geom_vline(aes(xintercept = mean(Sepal.Length)), color= "red")



# 2. Aplicación -----------------------------------------------------------

##  tidyquant

tickers <- c('MCD','SBUX')
adjusted <- tq_get(tickers,from ='2018-01-01',
                   to = '2020-10-12',
                   get = 'stock.prices')

adjusted %>% group_by(symbol) %>% slice(1)

ggplot(adjusted,aes(x=date,y=adjusted,color =symbol))+
        geom_line()+
        labs(title="Precio de cierre ajustado en {frame_along}",
             subtitle = "Periodo: 2010/01/01 - 2020/10/12 ",
             y="Precio Ajustado",
             x="Fecha",
             caption = "Fuente: Yahoo Finance\nElaboraci?n: @CristopherA98")+
        scale_y_continuous(breaks =seq(0,300,40))+
        scale_x_date(date_breaks = "4 month",
                     date_labels = "%b-%y")+
        scale_color_manual(values=c("#D86408","#C20868"))+
        transition_reveal(date)



ggplot(adjusted,aes(x=date,y=adjusted,color =symbol))+
        geom_line()+
        labs(title="Precio de cierre ajustado en {frame_along}",
             subtitle = "Periodo: 2018/01/01 - 2020/10/09 ",
             y="Precio Ajustado",
             x="Fecha",
             caption = "Fuente: Yahoo Finance\nElaboraci?n: @CristopherA98")+
        scale_y_continuous(breaks =seq(0,300,40))+
        scale_x_date(date_breaks = "6 month",
                     date_labels = "%b-%Y")+
        scale_colour_manual(values=c("#D86408","#C20868"),
                            name ="Empresa")+
        transition_reveal(date)



# FUENTES ADICIONALES -----------------------------------------------------

browseURL(url="https://ggplot2.tidyverse.org/",
          browser = getOption("bowser"))

# Página de paleta de colores

browseURL(url="https://htmlcolorcodes.com/es/",
          browser = getOption("bowser"))

