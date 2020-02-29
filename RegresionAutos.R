library(readr)
# install.packages("corrplot") # Nuevo
library(corrplot) # Para correlación
library(caret)  # Para dividir conjunto de datos

# install.packages("MASS") # NUEVO
library(MASS)
datos <- read.csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/RegresionRestaurante/regresionR/data/auto-mpg.csv")

head(datos) # Los primeros seis

summary(datos) # Antes de categorizar o factor

#SE BUSCA: cuantas millas se pueden predecir


datos$cylinders <- factor(datos$cylinders, levels = c(3,4,5,6,8),labels = c('3c', '4c','5c','6c','8c'))

summary(datos) # Después de categorizar o factor


cor(x=datos[,-c(1,3, 8,9)], method = "pearson")




pairs(x=datos[,-c(1,3,8,9)], lower.panel = NULL)



corrplot(corr = cor(x=datos[,-c(1,3,8,9)], method = "pearson"), method = "number")


#muestra del 70 % de los datos en el conjunto
set.seed(2018)
entrena <- createDataPartition(datos$mpg, p=0.7, list = FALSE)
entrena


nrow(entrena) # Cuantos datos de entrenamiento

head(datosentrenamiento)


modelo <- lm(mpg ~ ., data = datosentrenamiento)

modelo


summary(modelo)


#se grafican valores residuales
boxplot(modelo$residuals)
datosentrenamiento <- datos[entrena, -c(1,8,9)]

datosvalidacion <- datos[-entrena, -c(1,8,9)]

# Veremos que no son los mis datos los de entrenamiento y los datos de validación
head(datos)