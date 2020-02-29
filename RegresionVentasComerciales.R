library(ggplot2) # Para gráficas
library(stats) # Para regresion lineal

semanas <- c(1:10)
comerciales <- c(2,5,1,3,4,1,5,3,4,2)
ventas <- c(50,57,41,54,54,38,63,48,59,46)

datos <- data.frame(semanas,comerciales,ventas)
datos

plot(datos$comerciales, datos$ventas,
     xlab = "Comerciales",
     ylab = "Ventas $",
     main = "Diagrama de dispersión")

#Diagrama de dispersión con ggplot2
ggplot(datos, aes(comerciales, ventas))   +   geom_point()

#modelo de regresión lineal
modelo <- lm(ventas ~ comerciales, data = datos)
modelo


#se dibuja la linea de tendencia sobre el conjunto de datos
y_predict <- predict(modelo, datos)

ggplot() + geom_point(data = datos, aes(x = comerciales, y = ventas), size = 0.9) +
  geom_line(aes( x = datos$comerciales, y = y_predict), color = "red") +
  xlab("Comerciales") + 
  ylab("Ventas") + 
  ggtitle("Linea de tendencia sobre Conjunto de Datos")


#interpretación del modelo

summary(modelo)

predict.ventas <-predict(modelo,data.frame(comerciales=c(3.5,4.5)))

predict.ventas<-modelo$coefficients[1]+modelo$coefficients[2]*c(3.5,4.5)

