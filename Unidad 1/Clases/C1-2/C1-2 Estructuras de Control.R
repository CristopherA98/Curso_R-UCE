####################################
###       Curso R - UCE          ###
###          Clase 02            ###
####################################



# 1. Estructuras de control -----------------------------------------------


## Información adicional
browseURL(url = "https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r",
          browser = getOption("browser")) # Datacamp
browseURL(url = "https://bookdown.org/jboscomendoza/r-principiantes4/if-else.html",
          browser = getOption("browser")) #bookdown
#----- 1.1 Operadores condicional if if else ifelse

### 1.1.1. if

# Ejemplo 1
a <-  5
b <-  10
if (a == b){
  print("a es igual a b")
}

### 1.1.2. ifelse

a <- 7
b <- 5
if (a == b){
  print("a es igual a b")
} else{
  print("a es diferente de b")
}

# OJO 
# La evaluacion lógica no es vectorizada.
x <- c(2, 3, -5, 6, -2, 8)
if (x > 0) {
  print("positivo")
} else {
  print("negativo")
}

### 1.1.3. ifelse
x <- c(2, 3, -5, 6, -2, 8)
ifelse(x > 0,"positivo","negativo")

#----- 1.2. Ejecucion repetitiva for, repeat, while

### 1.2.1. for

# Ejemplo 1

for (i in 1:4) {
  print(i)
}

# Ejemplo 2
x <- c(12, 22, 28, 41)
for (i in x){
  print(i+5)
}

### 1.2.2. repeat
i <- 1
repeat {
  if (i > 15) break
  else{
    cat('\n', i, "a?os de edad")
    i <- i + 1
  }
}

# Funcion cat
# Esta funci?n escribe texto y variables en la salida.
# La secuencia de escape "\n" produce una nueva linea e impide que la siguiente 
# salida del programa quede en la misma l?nea.

### 1.2.3. while

# Ejemplo 1

"Imprimamos solo los numeros menores a 5"
i <- 1
while (i<5) {
  i <- i+1
  print(i)
}

# 2. Familia apply --------------------------------------------------------

# Fuente de informacion 
browseURL(url = "https://www.guru99.com/r-apply-sapply-tapply.html", browser = getOption("browser")) 
browseURL(url = "http://adv-r.had.co.nz/Functionals.html", browser = getOption("browser")) 
browseURL(url = "https://www.datacamp.com/community/tutorials/r-tutorial-apply-family", browser = getOption("browser"))

## 2.1. apply
data(iris)
iris <- data.frame(iris)


apply(X = iris[,-5],MARGIN = 2,FUN = mean)
apply(X = iris[,-5],MARGIN = 2,FUN = sum)

data("airquality")
apply(X = airquality,MARGIN = 2,FUN = mean,na.rm=T)
sum(is.na(airquality$Ozone))

## 2.2. lapply

lapply(iris[,-5],FUN = mean)
lapply(iris, function(x) summary(x))

#sobre un vector
seq(from=0,to=100,by=10)

lapply(1:10, function(x) x^2)


## 2.2. sapply

min.sl <- sapply(iris[,-5],min)
min.sl #devuelve un vector

min.sl <-lapply(iris[,-5],min)
min.sl #devuleve una lista

avg <- function (x) {
  (min(x) + max(x)) / 2
}
resultado <- sapply (iris[,-5], avg)
resultado

## 2.2. tapply
tapply (iris$Sepal.Width, iris$Species, median)

by(iris[,1:4],iris$Species,colMeans)


# Aplicación

asa.data <- read.csv("Datasets/ASA_pasajeros.csv")

lapply(asa.data,class)  
lapply(asa.data$Pasajeros.nacionales,sum)

asa.data$Estado
unique(asa.data$Estado)

sum(asa.data$Pasajeros.nacionales)  
colnames(asa.data)
asa.pasajeros <- asa.data[,c("Pasajeros.nacionales" ,"Pasajeros.internacionales")]
lapply(asa.pasajeros,sum) 
total <- sapply(asa.pasajeros,sum)
sum(total)

attach(asa.data)
tapply(asa.data$Pasajeros.nacionales,asa.data$Estado,mean)
tapply(Pasajeros.nacionales,Estado,sum)
