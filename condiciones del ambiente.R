#install.packages("MASS")

library(readr)
# install.packages("corrplot") # Nuevo
library(corrplot) # Para correlación
library(caret)  # Para dividir conjunto de datos
# install.packages("MASS") # NUEVO
library(MASS)

oxidoNit <- c(0.90, 0.91, 0.96, 0.89, 1.00, 1.10, 1.15, 1.03, 0.77, 1.07, 1.07, 0.94, 1.10, 1.10, 1.10, 0.91, 0.87, 0.78, 0.82, 0.95)
humedad <- c(72.4, 41.6, 34.3, 35.1, 10.7, 12.9, 8.3, 20.1, 72.2, 24.0, 23.2, 47.4, 31.5, 10.6, 11.2, 73.3, 75.4, 96.6, 107.4, 54.9)
temperatura <- c(76.3, 70.3, 77.1, 68.0, 79.0, 67.4, 66.8, 76.9, 77.7, 67.7, 76.8, 86.6, 76.9, 86.3, 86.0, 76.3, 77.9, 78.7, 86.8, 70.9)
presion <- c(29.18, 29.35, 29.24, 29.27, 29.78, 29.39, 29.69, 29.48, 29.09, 29.60, 29.38, 29.35, 29.63, 29.56, 29.48, 29.40, 29.28, 29.29, 29.03, 29.37)

length(oxidoNit)
length(temperatura)
length(humedad)
length(presion)


datos <- data.frame(oxidoNit, humedad, temperatura, presion)
datos


summary(datos)

str(datos)

plot(datos)


cor(x=datos, method = "pearson")


pairs(x=datos, lower.panel = NULL)


corrplot(corr = cor(x=datos, method = "pearson"), method = "number")


modelo <- lm(oxidoNit ~ ., data = datos)

modelo