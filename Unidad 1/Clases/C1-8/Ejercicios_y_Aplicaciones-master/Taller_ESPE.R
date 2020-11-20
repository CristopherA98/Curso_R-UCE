
# install.packages("pracma")
# install.packages("xlsx")
# install.packages("fdth")
# install.packages("prettyR")
# install.packages("gmodels")
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("funModeling")
# install.packages("reshape")

library(pracma)
library(fdth)
library(prettyR)
library(ggplot2)
library(gmodels)
library(tidyverse)
library(gridExtra) 
library(funModeling)
library(reshape)
library(gridExtra)
# Ejercicio 1 -------------------------------------------------------------
#Creación de las variables
set.seed(1234)
altura <- round(runif(50,min = 1.5,max = 2),2)

set.seed(1234)
peso <-  round(runif(50,min = 50,max = 80),1)

hermanos <-sample(0:4, size=50, replace= T, prob= NULL )

set.seed(1234)
sexo <- factor(sample(1:2, size=50, replace= TRUE, prob= NULL ),
               levels = c(1,2),labels = c("Masculino","Femenino"))

set.seed(1234)
ciudad <- factor(sample(1:6, size=50, replace= TRUE, prob= NULL ),
                 levels = c(1:6),labels = c("Quito","Guayaquil","Cuenca",
                                            "Ambato","Riobamba","Manta"))
#Creación de la base de datos
datos.estudiantes <- data.frame(altura,peso,hermanos,sexo,ciudad)
describe(datos.estudiantes)

#Exportar la base en excel
write.csv2(datos.estudiantes, "datos.estudiantes.csv")


#### Análisis de variables cualitativas
attach(datos.estudiantes)
freq1 <- datos.estudiantes %>% 
  group_by(sexo) %>% 
  summarise(Frecuencia = n()) %>% 
  mutate(Frec.Relativa=Frecuencia/sum(Frecuencia))%>%
  mutate(Frec.Acumulada = cumsum(Frecuencia))%>%
  mutate(Frec.Acumulada = cumsum(Frecuencia))%>%
  mutate(Frec.Relativa_Acum=Frec.Acumulada/sum(Frecuencia))
freq1

freq2 <- datos.estudiantes %>% 
  group_by(ciudad) %>% 
  summarise(Frecuencia = n()) %>% 
  mutate(Frec.Relativa=Frecuencia/sum(Frecuencia))%>%
  mutate(Frec.Acumulada = cumsum(Frecuencia))%>%
  mutate(Frec.Acumulada = cumsum(Frecuencia))%>%
  mutate(Frec.Relativa_Acum=Frec.Acumulada/sum(Frecuencia))
freq2

#Gráficos   

freq(datos.estudiantes)

ggplot(freq1, aes(x="", y=Frec.Relativa, fill=sexo)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  geom_text(aes(label=scales::percent(Frec.Relativa)), vjust=-5.5, size=5)+
  labs(title="Gráfico Pie chart de la variable sexo")


posicion <- freq2 %>%
  arrange(desc(ciudad)) %>%
  mutate(lab.ypos = cumsum(Frec.Relativa) - 0.5*Frec.Relativa)

ggplot(posicion, aes(x="", y=Frec.Relativa, fill=ciudad)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y=lab.ypos,label = scales::percent(Frec.Relativa)), color = "black")+
  labs(title="Gráfico Pie chart de la variable ciudad de nacimiento")+
  theme_void()

#Tabla cruzada
table1 <- CrossTable(ciudad, sexo, prop.chisq = FALSE)


#### Análisis de variables cuantitativas

freq3 <- fdt(altura, start=1.5, end=2.0, h=0.1)
freq3
freq4 <- fdt(peso, start=50, end=80, h=5)
freq4
freq5 <- fdt(hermanos, start=0, end=5, h=1)
freq5


# Función para crear los gráficos 
plot1 <- function (data, bins = 10, path_out = NA) 
{
  wide_data = suppressMessages(melt(data))
  p = ggplot(data = wide_data, mapping = aes(x = value)) + 
    geom_histogram(bins = bins, na.rm = T) +
    facet_wrap(~variable, 
               scales = "free_x") + aes(fill = variable) + guides(fill = FALSE)+
    labs(title = "Histogramas de las variables cuantitativas")
  if (!is.na(path_out)) {
    export_plot(p, path_out, "histograms")
  }
  plot(p)
}

plot1 <- plot1(datos.estudiantes)

# Medidas de tendenncia central y dispersión
profiling_num(datos.estudiantes)

p1 <- ggplot(datos.estudiantes,aes(x="",y=altura))+
  geom_boxplot(fill= "#009E73")

p2 <- ggplot(datos.estudiantes,aes(x="",y=peso,fill="blue"))+
  geom_boxplot(fill= "#F0E442")


p3 <- ggplot(datos.estudiantes,aes(x="",y=hermanos,fill="green"))+
  geom_boxplot(fill= "#CC79A7")

grid.arrange(p1,p2,p3, widths= c(2,2))



# Ejercicio 2 -------------------------------------------------------------

# Se analiza una muestra de 25 pacientes, si la probabilidad de que tenga una determinada
# enfermedad es del 7%, calcule la probabilidad que:

### Literal a
# Ninguno de los pacientes tenga esa enfermedad
n2a <- 25
p2a <- 0.07
x2a <- 0
dbinom(x2a,n2a,p2a)
### Literal b
# De que entre 15 y 20 pacientes adolezcan de la enfermedad
n2b <- 25
p2b <- 0.07
x2b <- c(14,20)
pbinom(x2b,n2b,p2b)
pbinom(x2b,n2b,p2b)[2]-pbinom(x2b,n2b,p2b)[1]

### Literal c
# Graficar la distribución de probabilidad

x_bin <- seq(0,25, by=1) 
y_dbin <- dbinom(x_bin, size = 25, prob=0.07) 
distr_binom <- data.frame(x_bin, y_dbin)

#Visualización de la distribución binomial

ggplot(distr_binom, aes(x=x_bin, y=y_dbin)) + 
  geom_line(colour="blue")+
  ggtitle("Distribución binomial")+
  labs(x="X",
       y="Probabilidad")

# Ejercicio 3 -------------------------------------------------------------
# Supongamos que µ = 20, por lo que el numero esperado de entradas en nuestra página web en
# una hora es 20. Calcule la probabilidad de que:

### Literal a
# Nadie ingrese a la página web
u3a <- 20
x3a <- 0
dpois(x3a,u3a)

### Literal b
# De que haya entre 25 y 40 entradas
u3b <- 20
x3b <- c(24,40)
ppois(x3b,u3b)
ppois(x3b,u3b)[2]- ppois(x3b,u3b)[1]

### Literal c
# Graficar la distribución de probabilidad

x_pois <- seq(0,40, by=1) 
y_dpois <- dpois(x_pois, lambda = 20) 
distr_pois <- data.frame(x_pois, y_dpois)

ggplot(distr_pois, aes(x=x_pois, y=y_dpois)) + 
  geom_line(colour="blue")+
  ggtitle("Distribución Poisson")+
  labs(x="X",
       y="Probabilidad")

# Ejercicio 4 -------------------------------------------------------------
# Una población consta de 15 elementos, 10 de los cuales son aceptables. En una muestra de 4
# elementos, ¿cuál es la probabilidad de que exactamente 3 sean aceptables? Suponga que las
# muestras se toman sin reemplazo. 

n4 <- 15
m4 <- 10
k4 <- 4
x4 <- 3

dhyper(x4,m4 ,n4-m4, k4)
### Literal a
# Ninguno se aceptable
n4a <- 15
m4a <- 10
k4a <- 4
x4a <- 0
dhyper(x4a,m4a ,n4a-m4a, k4a)

### Literal b
# De que por lo menos 3 sean aceptables
n4b <- 15
m4b <- 10
k4b <- 4
x4b <- c(3:4)
sum(dhyper(x4b,m4b ,n4b, k4b))

### Literal c
# Graficar la distribución de probabilidad

x_hiper <- seq(0,4, by=1) 
y_dhiper <- dhyper(x_hiper,10,15-10,4) 
distr_hiper <- data.frame(x_hiper, y_dhiper)

ggplot(distr_hiper, aes(x=x_hiper, y=y_dhiper)) + 
  geom_line(colour="blue")+
  ggtitle("Distribución Hipergeométrica")+
  labs(x="X",
       y="Probabilidad")

# # Ejercicio 5 -------------------------------------------------------------
# El diámetro de los puntos producidos por una impresora matricial tiene distribución normal con
# un diámetro promedio de 0.002 pulgadas y una desviación estándar de 0.0004 pulgadas

### Literal a
# Cuál es la probabilidad de que el diámetro de un punto exceda 0,0026 pulgadas.
u5a <- 0.002
s5a <- 0.0004
x5a <- 0.0026
pnorm(x5a, u5a, s5a)

### Literal b
# Cual es la probabilidad de que el diámetro de un punto mida entre 0.0014 y 0.0026 pulgadas.
u5b <- 0.002
s5b <- 0.0004
x5b <- c(0.0014,0.0026)
pnorm(x5b, u5b, s5b)
pnorm(x5b, u5b, s5b)[2]-pnorm(x5b, u5b, s5b)[1]

### Literal c
# Grafique la distribución de probabilidad.

n<-seq(0.0014,0.0026,0.0001)  
distr_norm <- data.frame(n=n, 
                    distribucion=pnorm(n, mean=0.002, sd=0.0004)) 

#Dibujamos la gráfica
ggplot(distr_norm, aes(x=n, y=distribucion )) + 
  geom_line(colour="blue")  + 
  ggtitle ("Distribución de probabilidad normal")+
  labs(x="X",
       y="Probabilidad")



