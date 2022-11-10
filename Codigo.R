#LizbethRamosLópez
#Abrimos y preparamos el archivo que vamos a utilizar
library(readxl)
#Partículas 10 micrómetros
PM10 <- read_excel("D:/7mo Semestre/Mineria de Datos/PF/2016PM10.xls", na = "-99")
PM10$FECHA <- as.Date(PM10$FECHA)
PM10 <- as.data.frame(PM10)
row.names(PM10) <- PM10$FECHA
PM10 <- PM10[-1]
sapply(PM10, function(x) sum(is.na(x)))
PM10 <- PM10[,-6]
PM10 <- PM10[,-7]
for(i in 1:7){
  PM10[,i] <- ifelse(is.na(PM10[,i]),
                     mean(PM10[,i], na.rm = TRUE),
                     PM10[,i]
  )
}


#Creamos una columna con el PM10 general para cada fecha
PM10$TOTAL = rowMeans (PM10[ , 1:7])

#calculamos IMECA para cada Observación
IMECA <- function(x){
  if(x > 0 && x <= 120)
    return(x*0.833)
  if(x > 121 && x <= 320)
    return((x*0.5)+40)
  if(x > 320)
    return(x*0.625)
} 
for(i in 1:61){
    PM10[i,8] <- IMECA(PM10[i,8])
}

#Clasificación
pairs(PM10)
cor(PM10)

#Creamos grupo de entrenamiento y uno de prueba
set.seed (123)
indice <- sample (2, nrow(PM10), replace = TRUE, prob = c(0.60, 0.40))
PM10.train <- PM10 [indice == 1,]
PM10.valid <- PM10 [indice == 2,]
#Creamos nuestro modelo respecto a todos nuestros predictores
regresion <- lm(PM10.train$TOTAL ~ .,data = PM10.train)
summary(regresion)

#Verificamos variables óptimas de regresión
library(leaps)
search <- regsubsets(PM10.train$TOTAL ~ .,data = PM10.train, nbest = 1, nvmax = dim(PM10.train)[2], method = "exhaustive")
sum <- summary(search)
sum$adjr2

#evaluamos la efectividad del modelo
library(lattice)
library(ggplot2)
library(caret)
RMSE(predict(regresion, PM10.valid), PM10.valid$TOTAL)

PM20 <- read.csv("D:/7mo Semestre/Mineria de Datos/PF/2020PM10.csv", na = "-99", header = TRUE)
row.names(PM20) <- PM20$FECHA
PM20 <- PM20[-1]
sapply(PM20, function(x) sum(is.na(x)))
PM20 <- PM20[,-6]
PM20 <- PM20[,-7]
PM20 <- na.omit(PM20)
predict(regresion, PM20)
