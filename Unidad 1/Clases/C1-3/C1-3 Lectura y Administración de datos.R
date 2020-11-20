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
?read_csv

data_asa <- read_csv("Datasets/ASA_pasajeros.csv")
str(data_asa)

getwd()
setwd()

?read.csv
auto <- read.csv("Datasets/U1/auto-mpg.csv", 
                 header = TRUE, sep = ",",
                 na.strings = "",
                 stringsAsFactors = FALSE)

str(auto)
names(auto)

auto_no_header <- read.csv("Datasets/U1/auto-mpg-noheader.csv", header = FALSE)
head(auto_no_header, 4)

auto_no_sense <- read.csv("Datasets/U1/auto-mpg-noheader.csv")
head(auto_no_sense, 4)

auto_custom_header <- 
  read.csv("Datasets/U1/auto-mpg-noheader.csv",
           header = F, 
           col.names = c("numero", "millas_por_galeon", "cilindrada", 
                         "desplazamiento", "caballos_de_potencia", 
                         "peso", "aceleracion", "año", "modelo"
           )
  )
head(auto_custom_header, 2)

## 1.2. Base de datos de tipo '.xls y .xlsx'
?read_excel
excel_sheets("Datasets/data_bikes/bikes.xlsx") # sirve para saber el nombre de las hojas dentro del archivo excel
bdd_bikes <- read_excel("Datasets/data_bikes/bikes.xlsx",sheet = "Sheet1",skip  = 5,col_names = F)
ejemplo <- read_excel("Datasets/data_bikes/bikes.xlsx",sheet = "Ejemplo")
str(ejemplo)

### Ahora si deseo cargar las distintas hojas del mismo archivo excel em
### dataframes diferentes.
data_list <- excel_sheets("Datasets/data_bikes/bikes.xlsx")
list <- lapply(data_list, function(data_list) read_excel("Datasets/data_bikes/bikes.xlsx",
                                                         sheet = data_list))
names(list) <- data_list
list2env(list,envir = .GlobalEnv)

## 1.3. Base de datos de tipo '.sav o SPSS'

bdd_personas <- read_sav("Datasets/SPSS_enemdu_201912/enemdu_persona_201912.sav")
str(bdd_personas)

## 1.4. Base de datos de tipo '.dta o Stata'

data_dta <- read_dta("Datasets/datos/original/Area - Caracteristicas generales (Personas).dta")

## 1.5 Importar bases de datos en formato .RData
?load
load("Datasets/BiciQ.RData") %>% data.frame()
str(df1)


# 1.6 Lectura archivos XML 

# Scraping 
install.packages("XML")
library(XML)

url <- "Datasets/U1/cd_catalog.xml"

# Crea una posicion que localiza el documento 
xmldoc <- xmlParse(url) #XMLInternalDocument
# Recuperamos en nodo raiz / origen de origen 
rootnode <- xmlRoot(xmldoc)
rootnode[2]
# x es cada uno de los elementos del nodo raiz
cds_data <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue) )
View(cds_data)
cds.catalog <- data.frame(t(cds_data), row.names = NULL)
head(cds.catalog, 10)
cds.catalog[1:5,]

population_url <- "Datasets/U1/WorldPopulation-wiki.htm"
tables <- readHTMLTable(population_url)

most_populated <- tables[[6]]
head(most_populated, 3)

custom_table <- readHTMLTable(population_url, which = 6)


# 1.7 Lectura archivos JSON   
# array 

install.packages("jsonlite")
install.packages('curl')
library(jsonlite)
library(curl)


dat.1 <- fromJSON("Datasets/U1/students.json")
dat.2 <- fromJSON("Datasets/U1/student-courses.json")

head(dat.1, 3)
dat.1$Email

compras <- fromJSON("Datasets/U1/compras.json")



# 2. Manipulación de datos ------------------------------------------------

## 2.1. Abreviación de caracteres 

# attach(data_asa)
# nchar(`Codigo IATA`) # numero de caracteres que tiene cada elemento de la variables
# substr(x = `Codigo IATA`,start = 1,stop = 2) # extrae los dos primeros caracteres de la variables
# 
# ## 2.2. Patrones de busqueda 
# 
# ### 2.2.1. Funcion grep
# 
# grep(pattern = 'la' , x = c('Hola','Lola','Nada','Todo'),value = T)
# 
# grep(pattern = "Ciudad",Descripcion ,value = T) %>%  unique()

### 2.2.1. Funcion paste

# paste("El valor x es: ", 2.65)
# paste("Curso", "R", "UCE", sep = "-") # Defino el separador del texto


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

bdd_inner1 <- bdd_1 %>% inner_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner1

### Unión externa (outer join)

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all = TRUE) # usando funcion merge
bdd_inner

bdd_inner1 <- bdd_1 %>% full_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner1

### Left Join

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all.x = TRUE) # usando funcion merge
bdd_inner

bdd_inner1 <- bdd_1 %>% left_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner1

### Right Join

bdd_inner <- merge(x = bdd_1, y = bdd_2,by = "id",all.y = TRUE) # usando funcion merge
bdd_inner

bdd_inner1 <- bdd_1 %>% right_join(bdd_2,by = "id") # usando funcion inner_join
bdd_inner1

# ### Función Match
# 
# # Definir semilla para reproducibilidad
# set.seed(1235) 
# 
# df_base <- data.frame(Alumno = LETTERS[1:6], Matematicas = sample(x = 1:10, size = 6, replace = TRUE),
#                       Lenguaje = sample(x = 1:10, size = 6, replace = TRUE))
# df_base
# 
# 
# df_modificatorio <- data.frame(Alumno = c("E", "C"), Matematicas = c(99, 88)) 
# df_modificatorio
# 
# 
# df_base$Alumno %in% df_modificatorio$Alumno #pregunto de la data base se encuentra contenido por la modificado
# 
# # Con la funcion match
# # match
# match(x= df_base$Alumno, table= df_modificatorio$Alumno,nomatch = 0)
# 
# # Indexar con %in% o match
# 
# df_modificatorio[df_modificatorio$Alumn %in% df_base$Alumno, ] #%in%
# df_base[match(x = df_modificatorio$Alum, table = df_base$Alum, nomatch = 0), ] # match()

# 3. Aplicación -----------------------------------------------------------

bdd_bikes <- read_excel("Datasets/data_bikes/bikes.xlsx")
bdd_shop <- read_excel("Datasets/data_bikes/bikeshops.xlsx")
bdd_orders <- read_excel("Datasets/data_bikes/orderlines.xlsx")

## 3.1. Vista de los datos
str(bdd_bikes)
glimpse(bdd_bikes)

## 3.2. Unir datos
?left_join

d1 <- bdd_orders %>% left_join(bdd_bikes,by = c("product.id" = "bike.id"))


bike_final <- bdd_orders %>% 
              left_join(bdd_bikes,by = c("product.id" = "bike.id")) %>% 
              left_join(bdd_shop,by = c("customer.id" = "bikeshop.id")) 
bike_final %>%  glimpse()
glimpse(bike_final)

## 3.3. Exportar archivos

fs::dir_create("Datasets/Base Final")

### 3.3.1. Excel 
bike_final %>% 
  write_xlsx("Datasets/Base Final/bike_final.xlsx")

### 3.3.2. CSV
bike_final %>% 
  write_xlsx("Datasets/Base Final/bike_final.csv")

### 3.3.3. .sav

bike_final %>% write_sav("Datasets/Base Final/bike_final.sav")

# FUENTES ADICIONALES -----------------------------------------------------

browseURL(url = "https://www.datasciencemadesimple.com/join-in-r-merge-in-r/",
          browser = getOption("browser")) 

browseURL(url = "https://www.datasciencemadesimple.com/rbind-in-r/",
          browser = getOption("browser")) 

browseURL(url = "https://www.datasciencemadesimple.com/cbind-in-r/",
          browser = getOption("browser")) 









