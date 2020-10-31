####################################
###       Curso R - UCE          ###
###          Clase 06            ###
####################################


# Valores faltantes y atípicos (missing values & outliers) ----------------

packages <- c('tidyverse','ggplot2','visdat','caret','recipes',
              'mice','Hmisc','DMwR')

install.packages(packages)

library(easypackages)
libraries(packages)

# 1. Missing values -------------------------------------------------------

## 1.1. Detección

x <- c(1,2,5,8,9,NA)
is.na(x) # devuleve una respuesta de tipo buleana donde TRUE muestra la presencia de NA's
which(is.na(x)) # posición de los valores perdidos

data(algae)
glimpse(algae)
is.na(algae)
sum(is.na(algae))# suma de valores perdidos dentro de la data

algae %>%
  is.na() %>%
  reshape2::melt() %>%
  ggplot(aes(Var2, Var1, fill=value)) + 
  geom_raster() + 
  coord_flip() +
  scale_y_continuous(NULL, expand = c(0, 0)) +
  scale_fill_grey(name = "", 
                  labels = c("Present", 
                             "Missing")) +
  labs(title = "Valores perdidos",
       x="Observaciones",
       y="Varibales")+
  theme(axis.text.y  = element_text(size = 10))

### 1.1.1. Paquete Hmisc

sapply(algae,function(x){sum(is.na(x))}) # número de NA´s en la columnas
round((apply(apply(X = algae,MARGIN = 2,FUN = is.na )
             ,MARGIN=2,FUN=sum)/nrow(algae))*100,2)


# Eliminar las filas

data.cleaning <- algae[!is.na(algae$Chla),]
complete.cases(algae)
data.cleaning2 <- algae[complete.cases(algae),]


## Imputar por la media


mean(algae$Chla,na.rm = T)
algae$Chla_mean <- impute(algae$Chla, mean)

## Imputar con valor aleatorios

algae$Chla_random <- impute(algae$Chla,'random')

median(algae$Chla,na.rm = T)
algae$Chla_median <- impute(algae$Chla,median)
which(is.na(algae$Chla))# ver donde estan los NA's para ver lo cambios
algae[56,]

### 1.1.2. Paquete mice
imputed<-algae[,colSums(is.na(algae))>0]
imputed_Data <- mice(imputed, m=5, maxit = 8, method = 'pmm', seed = 500)

algae_mice <- complete(imputed_Data,2)
sapply(algae_mice,function(x){sum(is.na(x))})
algae_mice[56,]



### 1.1.1. Paquete DWwR
algae_knn<-knnImputation(algae)
sapply(algae_mice,function(x){sum(is.na(x))})


# 2. Outliers -------------------------------------------------------------

data (Ozone, package="mlbench")
glimpse(Ozone)
sapply(Ozone,function(x){sum(is.na(x))})

# Método IQR

outliers<-function(x){
  IQR <- quantile(x, probs=c(.25, .75), na.rm = T)
  ext <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (IQR[1] - H)] <- ext[1]
  x[x > (IQR[2] + H)] <- ext[2]
  return(x)}
# Solo variables numeric
numeric_cols <-names(Ozone)[sapply(Ozone, is.numeric)]
numeric_data<-Ozone[,names(Ozone)%in% numeric_cols]

Ozone_IQR<- data.frame(sapply(numeric_data,outliers))

ggplot(numeric_data,aes(x=V13))+
  geom_boxplot()

ggplot(Ozone_IQR,aes(x=V13))+
  geom_boxplot()

# Ojo la variable V13 no tiene NA´s por eso no necesito 'na.rm = T'
mean(numeric_data$V13)
mean(Ozone_IQR$V13)

# Imputación de datos

Ozone_IQR<-Ozone_IQR[,colSums(is.na(Ozone_IQR))>0]
Ozone_knn<-knnImputation(Ozone_IQR) # metodo knn
sapply(Ozone_knn,function(x){sum(is.na(x))})

# Vecino más cercano

outliers<-function(x){
  IQR <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (IQR[1] - H)] <- NA
  x[x > (IQR[2] + H)] <- NA
  return(x)}

numeric_cols<-names(Ozone)[sapply(Ozone, is.numeric)]
numeric_data<-Ozone[,names(Ozone)%in% numeric_cols]

outliers_missing <-data.frame(sapply(numeric_data,outliers))

# Gráfico empirico del tratamiento de los outliers

# Gráfico con data normal
ggplot(numeric_data,aes(x=V13))+
  geom_boxplot()

ggplot(outliers_missing,aes(x=V13))+
  geom_boxplot()

# Imputar valores NA
outliers_missing<-outliers_missing[,colSums(is.na(outliers_missing))>0]
Ozone_knn<-knnImputation(outliers_missing)
sapply(Ozone_knn,function(x){sum(is.na(x))})

mean(numeric_data$V13)
mean(Ozone_knn$V13,na.rm = T)

# FUENTES ADICIONALES -----------------------------------------------------

browseURL(url = "http://openaccess.uoc.edu/webapps/o2/bitstream/10609/64085/6/romancalafatiTFG0617memoria.pdf",
          browser = getOption("browser"))

