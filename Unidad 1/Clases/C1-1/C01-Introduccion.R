####################################
###       Curso R - UCE          ###
###          Clase 01            ###
####################################

# R como calculadora ------------------------------------------------------

5+10

8/20

sin(10)


# Instalacion de librerías ------------------------------------------------

# Informacion paquetes basicos
search()
# Instalacion desde el repositorio CRAN
install.packages('tidyverse')
# Se indica si se deben instalar o no los 
# paquetes requeridos para que este funcione.
install.packages('tidyverse', dependencies=TRUE)

# Instalacion multiples paquetes
packages <- c("ggplot2","lubridate","readr","easypackages")
install.packages(packages)
# funciones apply
# sapply(X=packages, FUN=install.packages, dependencies=TRUE)

# Cargar las librerias
library(ggplot2)
library(easypackages)
libraries(packages)
# Cargar varios paquetes
packages <- c("ggplot2","lubridate","readr")
# funciones apply
# sapply(X=packages, FUN=library, character.only=TRUE)

# Conocer que paquetes están cargados
(.packages())


# Estructura de Datos Fundamental -----------------------------------------

#----- Vectores
a <- 250
typeof(a)
# Longitud (Número de elementos)
length(a)
# Para probar que una estructura de datos es un vector (atómico o lista) 
# se utiliza la función:
is.vector(a)

#----- Vectores atómicos
dbl_var <- c(1, 2, -1) 
int_var <- c(3L, 7L, 1L) # con el sufijo L se declaran números enteros
log_var <- c(FALSE, TRUE, F, T) # TRUE y FALSE (T, F) se usan para crear
chr_var <- c("Source", "Stat", "Lab")

# tercera componente de dbl_var
dbl_var[3]

c(1, c(2, c(3,4))) ; c(1, 2, 3, 4,5) # los vectores pueden ser anidados

# Dado un vector se puede determinar su tipo con typeof() 
# o verificar un tipo específico con una “is” función:
dbl_var <- c(1, 2, -1)
typeof(dbl_var)
is.double(dbl_var)

dbl_var <- c(1, 2, -1)
is.atomic(dbl_var)

int_var <- c(3L, 7L, 1L)
typeof(int_var)
is.integer(int_var)
is.atomic(int_var)

# is.numeric() retorna TRUE para vectores double e integer
dbl_var <- c(1, 2, -1)
is.numeric(dbl_var)

int_var <- c(3L, 7L, 1L)
is.numeric(int_var)

#----- Coercion
x <- c("ssl", 1, TRUE)
str(x)

x <- c(FALSE, TRUE, FALSE, TRUE) 
as.numeric(x) # TRUE coerciona a 1 y FALSE a 0
sum(x) # número total de TRUEs 
mean(x) # proporción de TRUEs

#----- Lista

x <- list(1:3, c("Source", "Stat", "Lab"), c(TRUE, FALSE), c(1.3, 4.5))
str(x)

is.atomic(x)

x <- list(1:3, c("Source", "Stat", "Lab"), c(TRUE, FALSE), c(1.3, 4.5))
typeof(x)

# probamos si x es una lista
is.list(x)

# pasamos a vector considerando las reglas de coerción
unlist(x)

# Una lista es un vector recursivo, es decir una lista puede contener 
# otra lista (diferencia principal con los vectores atómicos)

x <- list(list(list("Source", "Stat")))
str(x)

# son equivalentes. un vector no puede contener otro vector
c(1, c(2, c(3,4)))  ;  c(1, 2, 3, 4)

x <- list(1:3, c("Source", "Stat", "Lab"), nomb_obj=c(TRUE, FALSE), c(1.3, 4.5))
x[2]
x[[4]]
x$nomb_obj

#----- Matrices

mtx <- matrix (1:12,nrow=3, ncol=4, byrow=FALSE)
mtx

mtx <- matrix (1:12,nrow=3, ncol=4, byrow=TRUE)
mtx

# Elementos de una matriz 
# Columnas 2 y 4
mtx <- matrix (1:12, nrow=3, ncol=4, byrow=FALSE)

mtx[,c(2,4)]
mtx[1,3]

# Filas 2 y 3
mtx[c(2,3),]

#----- Array
array(1:12,c(2,3,2))

# Ejemplo más ilustrativo
x<-array(c(45,46,65,55,170,167,48,49,68,56,169,165),c(2,3,2))
dimnames(x)<-list(c("hombres","mujeres"),c("edad","peso","altura"),
                  c("Sur","Norte"))
x
# Elementos de un arrays
x[,,"Sur"]
x["hombres",,] 

#----- Dataframe

dbl_vec <- c(1, 2, 3)
chr_vec <- c("R", "S", "T")
log_vec <- c(TRUE, FALSE,TRUE)

# Cremos el dataframe
df <- data.frame(dbl_vec, chr_vec, log_vec)
df

# Elementos de un dataframe
nomb <- c("John", "Paul", "George", "Ringo")
nac <- c(1940, 1941, 1943, 1940)
instr <- c("guitar", "bass", "guitar", "drums")
df <- data.frame(nomb, nac, instr)

df[2, c(2,3)] #Mediante df[i, j] se obtiene la componente i, j del data frame.

df[2, 2] # componente 2, 2
df[3, 1] # componente 3, 1
df[3, ] # fila 3
df[ , 3]=="guitar" # columna 3 de df igual a "guitar"

