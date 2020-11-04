####################################
###       Curso R - UCE          ###
###          Clase 07            ###
####################################


# 1. Análisis de regresión -------------------------------------------------


# Prerrequisitos

paquetes <- c('tidyverse','GGally','openxlsx','MASS','hexbin','modelr','lmtest',
              'car','boot','leaps')

install.packages(paquetes)

library(easypackages)
libraries(paquetes)

# Lectura de la base de datos

data_venta_publicidad <- read.xlsx("Datasets/Venta_vs_Publicidad.xlsx", detectDates = TRUE)  
str(data_venta_publicidad)

data_venta_publicidad <- tibble::as.tibble( data_venta_publicidad) 
# Dispersión de los datos

data_venta_publicidad %>%
  ggplot(., aes(PUBLICIDAD_TOTAL, VENTA)) + 
  geom_point() 


# Correlaciones

ggpairs(data_venta_publicidad, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Estimación
mod_1 <- lm(VENTA ~ PUBLICIDAD_TOTAL, data = data_venta_publicidad)
summary(mod_1)

## Crea una tabla de datos base para predecir
grid <- data_venta_publicidad %>% 
  data_grid(PUBLICIDAD_TOTAL) 

## Agrega predicciones
grid <- grid %>% 
  add_predictions(mod_1, var = 'PREDIC') 

## Graficar
data_venta_publicidad %>%
  ggplot(., aes(x= PUBLICIDAD_TOTAL)) + 
  geom_point(aes(y= VENTA)) + 
  geom_line(aes(y= PREDIC), data = grid, colour = "red", size = 1) +
  ggtitle("Datos + predicción")

## Agregar residuales a datos

data_venta_publicidad <- data_venta_publicidad %>% 
  add_residuals(mod_1, var = 'RESIDUALES')

## Explorar los residuales
ggplot(data_venta_publicidad, aes(RESIDUALES)) + 
  geom_freqpoly()

## Gráfico qq
mod_1 %>% 
  ggplot(., aes(qqnorm(.stdresid)[[1]], .stdresid)) + 
  geom_point(na.rm = TRUE) + 
  geom_abline() + 
  xlab("Theoretical Quantiles") + 
  ylab("Standardized Residuals") + 
  ggtitle("Normal Q-Q")

# Analisis de los residuos

# H0 : Los residuos se ajustan a una distribución normal vs
# H1: Los residuos NO se ajustan a una distribución normal


# Si tuviésemos pocos datos
shapiro.test(data_venta_publicidad$RESIDUALES)

# Test KS
ks.test(data_venta_publicidad$RESIDUALES, "pnorm")

# Podemos utilizar los residuos studentizados
data_venta_publicidad$STUDRESIDUAL <- studres(mod_1) 

# Test KS
ks.test(data_venta_publicidad$STUDRESIDUAL, "pnorm")


## Explorar la varianza
ggplot(data_venta_publicidad, aes(PUBLICIDAD_TOTAL, RESIDUALES)) + 
  geom_ref_line(h = 0) +
  geom_point() + 
  ggtitle("Residuos")

# H0 : La varianza es constante vs
# H1: La varianza no es constante

## Prueba de homocedasticidad
bptest(mod_1)

# Correlacion
## Grafico ACF
acf( residuals( mod_1))

# H0: La autocorrelación de los residuos es 0 vs
# H1: La autocorrelación de los residuos es diferente de 0
dwtest(mod_1,alternative = "two.side")

## transformando el predictor

mod_1a <- lm(VENTA ~ log(PUBLICIDAD_TOTAL), data = data_venta_publicidad)
summary(mod_1a)

## Crea una tabla de datos base para predecir
grid <- data_venta_publicidad %>% 
  data_grid(PUBLICIDAD_TOTAL) 

## Agrega predicciones
grid <- grid %>% 
  add_predictions(mod_1a, var = 'PREDIC') 

## Graficar
data_venta_publicidad %>%
  ggplot(., aes(x= PUBLICIDAD_TOTAL)) + 
  geom_point(aes(y= VENTA)) + 
  geom_line(aes(y= PREDIC), data = grid, colour = "red", size = 1) +
  ggtitle("Datos + predicción")

bptest(mod_1a)

# Prediccion --------------------------------------------------------------


## Predecir con el primer modelo
predict(mod_1, newdata = data.frame(PUBLICIDAD_TOTAL= 2000))
grid %>% filter(PUBLICIDAD_TOTAL== 2000)
## Predecir con el modelo transformado
predict(mod_1a, newdata = data.frame(PUBLICIDAD_TOTAL= 2000))


# 2. Regresión lineal multiple  -------------------------------------------


