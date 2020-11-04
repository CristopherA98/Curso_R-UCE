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

# Prerrequisitos

paquetes <- c("tidyverse",'corrplot','GGally','MASS','leaps','gridExtra',
              'lmtest','car')
library(easypackages)
libraries(paquetes)

datos <- as.data.frame(state.x77)

datos <- datos %>% rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area) %>% 
         mutate(densidad_pob = habitantes * 1000 / area)

## 2.1. Relación entre las variables

round(cor(x = datos, method = "pearson"), 3)

corrplot::corrplot(cor(x = datos, method = "pearson")) # corrplot de las variables

ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# 2.2. Selección de los mejores predictores

# 2.2.1. Modelo con todos los predictores

full.model <- lm(esp_vida ~ ., data=datos)
summary(full.model)

model1 <- stepAIC(full.model, trace=TRUE, direction="backward")
model1$anova
summary(model1)

# Para obtener los intervalos de confianza

?confint
confint(model1,level = 0.95)

# 2.2.2. Método forward

names(datos)
model2 <- lm(esp_vida ~ 1, data=datos)
completo <- formula(y ~ habitantes + ingresos + analfabetismo + asesinatos 
                    + universitarios + heladas + area + densidad_pob , datos)

model2_final <- stepAIC(model2, trace=FALSE, direction="forward", scope=completo)
model2_final$anova
summary(model2_final)

# 2.2.3. Método both

model3 <- stepAIC(model2, trace=FALSE, direction="both", scope=completo)
model3$anova

# Obtener R2 ajustado

summary(model1)$adj.r.squared
summary(model2_final)$adj.r.squared


# 2.2.4. regsubsets 

model4 <- regsubsets(esp_vida~.,datos, nbest = 2, nvmax = 13)

summary(model4)
summary(model4)$which


model4_summary <- summary(model4)
data.frame(model4_summary$outmat)

cbind(model4_summary$which,
      model4_summary$bic,
      model4_summary$adjr2)

# Graficamente

par(mfrow=c(1, 2))
plot(model4, scale="adjr2", main=expression(R[Adj]^2))
plot(model4, scale="bic", main="BIC")

# valores excatos de R2 y BIC
summary(model4)$adjr2
summary(model4)$bic

# Ver cual es el mejor modelo

which(model4_summary$bic == min(summary(model4)$bic))
model4_summary$which[3, ]

which(model4_summary$adjr2 == max(summary(model4)$adjr2))
model4_summary$which[9, ]

# Otra forma de obtener el mejor modelo

# which.max(model4_summary$adjr2)
# model4_summary$which[which.max(model4_summary$adjr2), ]


## 2.3. SUpuestos del modelo

summary(model1)

plot1 <- ggplot(data = datos, aes(habitantes, model1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) 


plot2 <- ggplot(data = datos, aes(asesinatos, model1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) 

plot3 <- ggplot(data = datos, aes(universitarios, model1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) 


plot4 <- ggplot(data = datos, aes(heladas, model1$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) 
# Juntar los gráficos
grid.arrange(plot1, plot2, plot3, plot4)

# 2.3.1. Distribución normal de los residuos

par(mfrow = c(1,1))
qqnorm(model1$residuals)
qqline(model1$residuals)

ks.test(model1$residuals,"pnorm")

# media errores
mean(model1$residuals)

# 2.3.2. Varianzas constantes

ggplot(data = datos, aes(model1$fitted.values, model1$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) 

bptest(model1)

# 2.3.3. No autocorrelacion en los erroes

dwt(model1, alternative = "two.sided")
bgtest(model1)

# 2.3.3. Multicolinealidad

corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas))
         , type="upper", order="hclust", tl.col="black", tl.srt=45)

corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)),
         method = "number", type= "upper",tl.col = "black")

vif(model1)

# Identificación de posibles valores atípicos o influyentes

hatvalues(model1)

outlierTest(model1, cutoff=Inf, n.max=4)
influencePlot(model1)
influenceIndexPlot(model1, vars="Bonf", las=1)


# FUENTES ADICIONALES -----------------------------------------------------

browseURL(url="https://fhernanb.github.io/libro_regresion/",
          browser = getOption("browser"))
