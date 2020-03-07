library(readr)
library(dplyr)
library(ggplot2)
library(reshape) # Para renombrar columnas
library(caret) # Para particiones
library(corrplot) # Para correlaciones visuales

datos <- read_csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/MachineLearning/datos/melb_data.csv")
head(datos)

datos


tail(datos)

str(datos)

summary(datos)

# MODELO Price Vs Rooms + Distance
modelo <- lm(Price ~ Rooms + Distance, datos)
modelo


summary(modelo)


pairs(datos[,c('Price','Rooms')])



datos.Num <- select(datos, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 
head(datos.Num)


str(datos.Num)



mediana.BA <- median(datos.Num$BuildingArea, na.rm = TRUE) # summary(datos.Num$BuildingArea)[3], como otra alternativa
mediana.YB <- median(datos.Num$YearBuilt, na.rm = TRUE)    # summary(datos.Num$YearBuilt)[3], , como otra alternativa
mediana.C <- median(datos.Num$Car, na.rm = TRUE)    # summary(datos.Num$Car)[3], , como otra alternativa