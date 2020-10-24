
# Instrucciones -----------------------------------------------------------

# 1. Ejecutar las siguientes lineas de código

install.packages("devtools")
install.packages('usethis')
library(usethis)
library(devtools)
devtools::install_github("RamiKrispin/coronavirus")

# 2. Esperar hasta que salga el mensaje siguiente:

#----Enter one or more numbers, or an empty line to skip updates: 

# 3. Escribir "3" en la cosoloa y dar enter
# 4. cargar el data frame con la siguiente linea de código
library(coronavirus)
covid19 <- data.frame(coronavirus)

# 1. Aplicación -----------------------------------------------------------

library(tidyverse)
## 1.1. Estructura de la base de datos

covid19 <- tibble::as.tibble(covid19)
covid19

## 1.2. Manipulación de la data

### 1.2.1. Selección de las variables que voy analizar
bdd_covid <- covid19 %>% select(-province,-(lat:long))

### 1.2.2. Obtener el total de casos confirmados por país
unique(bdd_covid$type)
total <- bdd_covid %>% 
              filter(type == "confirmed") %>% 
              group_by(country) %>% 
              summarise(Confirmados = sum(cases)) %>% 
              arrange(-Confirmados) 
total %>% head(20)       

### 1.2.3. Tasa de mortalidad
bdd_final <- bdd_covid %>% 
                group_by(country, type) %>%
                summarise(Confirmados = sum(cases)) %>%
                pivot_wider(names_from = type, values_from = Confirmados) %>%
                arrange(- confirmed) %>%
                mutate(Tasa_recuperados = recovered / confirmed,
                                Tasa_mortalidad = death / confirmed) %>% 
                mutate(Tasa_recuperados = if_else(is.na(Tasa_recuperados), 0, Tasa_recuperados),
                       Tasa_mortalidad = if_else(is.na(Tasa_mortalidad), 0, Tasa_mortalidad)) %>% 
                mutate(Tasa_recuperados = paste(round(100 * Tasa_recuperados, 2), "%", sep = ""),
                       Tasa_mortalidad = paste(round(100 * Tasa_mortalidad, 2), "%", sep = ""))
                
bdd_final %>% head(20)
bdd_final %>% filter(country == "Ecuador")


