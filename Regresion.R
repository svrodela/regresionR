library(ggplot2) # Para gráficas
library(stats) # Para regresion lineal
restaurantes <- c(1:10)
estudiantes <- c(2,6,8,8,12,16,20,20,22,26)
ventas <- c(58,105,88,118,117,137,157,169,149,202)

datos <- data.frame(restaurantes,estudiantes,ventas)
datos