####################################
###       Curso R - UCE          ###
###          Clase 03            ###
####################################


# Lectura y administración de datos ---------------------------------------

## Prerrequisitos

packages <- c('tidyverse','readxl','haven','easypackages','strings','writexl')
install.packages(packages)

library(easypackages) #easypackages me permite cargar varios paquetes a la vez
libraries(packages)



# 1. Importar base de datos -----------------------------------------------

## 1.1. Base de datos de tipos '.csv'
?read_csv2

data_asa <- read_csv("Datasets/ASA_pasajeros.csv")
str(data_asa)

## 1.2. Base de datos de tipo '.xls y .xlsx'
excel_sheets("Datasets/data_bikes/bikes.xlsx") # sirve para saber el nombre de las hojas dentro del archivo excel
bdd_bikes <- read_excel("Datasets/data_bikes/bikes.xlsx")
ejemplo <- read_excel("Datasets/data_bikes/bikes.xlsx",sheet = "Ejemplo")
str(ejemplo)

### Ahora si deseo cargar las distintas hojas del mismo archivo excel em
### dataframes diferentes.

## 1.2. Base de datos de tipo '.sav o SPSS'

bdd_personas <- read_sav("Datasets/SPSS_enemdu_201912/enemdu_persona_201912.sav")
str(bdd_personas)

## 1.2. Base de datos de tipo '.dta o Stata'

data_dta <- read_dta("Datasets/datos/original/Area - Caracteristicas generales (Personas).dta")

#### 1.3 Importar bases de datos en formato .RData
?load
load("Datasets/BiciQ.RData") %>% data.frame()
str(df1)


# 2. Manipulación de datos ------------------------------------------------

## 2.1. Abreviación de caracteres 

attach(data_asa)
nchar(`Codigo IATA`) # numero de caracteres que tiene cada elemento de la variables
substr(x = `Codigo IATA`,start = 1,stop = 2) # extrae los dos primeros caracteres de la variables

## 2.2. Patrones de busqueda 

### 2.2.1. Funcion grep

grep(pattern = 'la' , x = c('Hola','Lola','Nada','Todo'),value = T)

grep(pattern = "Ciudad",Descripcion ,value = T) %>%  unique()

### 2.2.1. Funcion paste

paste("El valor x es: ", 2.65)
paste("Curso", "R", "UCE", sep = "-") # Defino el separador del texto


## 2.3. Unir datos

### 2.3.1. Función rbind

# se agregam las filas debajo 

df1 <- data.frame(A= 1:3, B= letters[1:3])
df2 <- data.frame(A= 1:4, B= letters[1:4])
resultado <- rbind(df1,df2)
resultado

### 2.3.2. Función cbind

df1 <- data.frame(A= 1:3, B= letters[1:3])
df2 <- data.frame(X= 1:3, Y= letters[1:3])
resultado <- cbind(df1,df2)
resultado

### 2.3.3. Función merge y familia join (inner, outer, left, right)

### Unión interna (inner join)

bdd_1 <- data.frame(id=(1:5),costo=seq(100,104))
bdd_2 <- data.frame(id=seq(2,10,2),producto=letters[1:5])

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id") # usando funcion merge
bdd_inner

bdd_inner <- bdd_1 %>% inner_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner

### Unión externa (outer join)

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all = TRUE) # usando funcion merge
bdd_inner

bdd_inner <- bdd_1 %>% full_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner

### Left Join

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all.x = TRUE) # usando funcion merge
bdd_inner

bdd_inner <- bdd_1 %>% left_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner

### Right Join

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all.y = TRUE) # usando funcion merge
bdd_inner

bdd_inner <- bdd_1 %>% right_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner

### Función Match

# Definir semilla para reproducibilidad
set.seed(1235) 

df_base <- data.frame(Alumno = LETTERS[1:6], Matematicas = sample(x = 1:10, size = 6, replace = TRUE),
                      Lenguaje = sample(x = 1:10, size = 6, replace = TRUE))
df_base


df_modificatorio <- data.frame(Alumno = c("E", "C"), Matematicas = c(99, 88)) 
df_modificatorio


df_base$Alumno %in% df_modificatorio$Alumno #pregunto de la data base se encuentra contenido por la modificado

# Con la funcion match
# match
match(x= df_base$Alumno, table= df_modificatorio$Alumno,nomatch = 0)

# Indexar con %in% o match

df_base[df_base$Alumno %in% df_modificatorio$Alumno, ] #%in%
df_base[match(x = df_modificatorio$Alum, table = df_base$Alum, nomatch = 0), ] # match()

# 3. Aplicación -----------------------------------------------------------


bdd_bikes <- read_excel("Datasets/data_bikes/bikes.xlsx")
bdd_shop <- read_excel("Datasets/data_bikes/bikeshops.xlsx")
bdd_orders <- read_excel("Datasets/data_bikes/orderlines.xlsx")

## 3.1. Vista de los datos

glimpse(bdd_bikes)

## 3.2. Unir datos
?left_join

left_join(bdd_orders, bdd_bikes,by = c("product.id" = "bike.id"))


bike_final <- bdd_orders %>% 
              left_join(bdd_bikes,by = c("product.id" = "bike.id")) %>% 
              left_join(bdd_shop,by = c("customer.id" = "bikeshop.id")) 
bike_final %>%  glimpse()


## 3.3. Exportar archivos

# fs::dir_create("carpetas")

### 3.3.1. Excel 
bike_final %>% 
  write_xlsx("Datasets/data_bikes/bike_final.xlsx")

### 3.3.2. CSV
bike_final %>% 
  write_xlsx("Datasets/data_bikes/bike_final.csv")




# FUENTES ADICIONALES -----------------------------------------------------

browseURL(url = "https://www.datasciencemadesimple.com/join-in-r-merge-in-r/",
          browser = getOption("browser")) 

browseURL(url = "https://www.datasciencemadesimple.com/rbind-in-r/",
          browser = getOption("browser")) 

browseURL(url = "https://www.datasciencemadesimple.com/cbind-in-r/",
          browser = getOption("browser")) 









