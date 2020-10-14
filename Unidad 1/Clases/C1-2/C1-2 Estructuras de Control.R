####################################
###       Curso R - UCE          ###
###          Clase 02            ###
####################################


# Estructuras de control --------------------------------------------------

#----- Operadores condicional if if else ifelse

### if

a <-  5
b <-  5
if (a == b){
  print("a es igual a b")
}

### ifelse

a <- 7
b <- 5
if (a == b){
  print("a es igual a b")
} else{
  print("a es diferente de b")
}

# OJO 
# La evaluacion l ´ ogica no es vectorizada.
x <- c(2, 3, -5, 6, -2, 8)
if (x > 0) {
  print("positivo")
} else {
  print("negativo")
}

### ifelse
x <- c(2, 3, -5, 6, -2, 8)
ifelse(x > 0,"positivo","negativo")

#----- Ejecución repetitiva for repeat while

### for

# Ejemplo 1
for (i in 1:4) {
  print(i)
}
# Ejemplo 2
x <- c(12, 22, 28, 41)
for (i in x){
  print(i+5)
}

### repeat
i <- 1
repeat {
  if (i > 5) break
  else{
    cat('\n', i, "años de edad")
    i <- i + 1
  }
}

### while

# Ejemplo 1
i <- 1
while (i<5) {
  i <- i+1
  print(i)
}

# Ejemplo 2

i <- 1
while (i<4) {
  print(i)
  i <- i+1
}


