library(rpart)      # Arboles
library(rpart.plot) # Visualizar y represenar árboles
library(caret)      # Para llevar a cabo particiones de conjuntos de datos en caso de...
library(dplyr)      # Para select, filter, mutate, arange ....
library(readr)      # Para leer datos
library(ggplot2)    # Para grafica mas vistosas
library(reshape)    # Para renombrar columnas


install.packages("rpart")

datos <- read_csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/MachineLearning/datos/melb_data.csv")
head(datos)

tail(datos)

summary(datos)

str(datos)

datos.Num <- select(datos, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 
head(datos.Num)


mediana.BA <- median(datos.Num$BuildingArea, na.rm = TRUE) # summary(datos.Num$BuildingArea)[3], como otra alternativa
mediana.YB <- median(datos.Num$YearBuilt, na.rm = TRUE)    # summary(datos.Num$YearBuilt)[3], , como otra alternativa
mediana.C <- median(datos.Num$Car, na.rm = TRUE)    # summary(datos.Num$Car)[3], , como otra alternativa



head(datos.Num, 10) # Los primeros 10, se observan NAs




datos.Num<- datos.Num %>%
  mutate (BuildingArea = ifelse(is.na(BuildingArea), mediana.BA, BuildingArea))

datos.Num <- datos.Num %>%
  mutate (YearBuilt = ifelse(is.na(YearBuilt), mediana.YB, YearBuilt)) 

datos.Num <- datos.Num %>%
  mutate (Car = ifelse(is.na(Car), mediana.C, Car)) 


head(datos.Num, 10) # # Los primeros 10, YA NO se observan NAs




set.seed(2020) # Semilla
entrena <- createDataPartition(datos.Num$Price, p=0.7, list = FALSE)
head(entrena)






head(datos.Num[-entrena,])


nrow(datos.Num[-entrena,])


# Ver los primeros seis datos con sólo variables numéricas
head(datos.Num)


# Ahora a determinar conjuntos de datos de entrenamiento y luego head()
datos.Entrena <- datos.Num[entrena,]
head(datos.Entrena)





summary(datos.Entrena)


# y conjunto de datos de validación y luego head()
datos.Valida <- datos.Num[-entrena,]
head(datos.Valida)





summary(datos.Valida)




set.seed(2020) # Semilla

arbol <- rpart(formula = Price  ~ ., data = datos.Entrena)
arbol



#imprime el arbol
prp(arbol, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8,  shadow.col = "gray")

plotcp(arbol)

#PODAR EL ARBOL




arbol.Recortado <- prune(arbol, cp = 0.01417645)

prp(arbol.Recortado, type = 2, nn = TRUE, 
    fallen.leaves = TRUE, faclen = 4,
    varlen = 8,  shadow.col = "gray")


summary(datos.Valida)



nuevo <- data.frame(cbind(c(0),c(3), c(2.7), c(2), c(2),c(1), c(250), c(146), c(1980), c(4019)))

colnames(nuevo) <- c("Price", "Rooms", "Distance", "Bedroom2", "Bathroom", "Car", "Landsize", "BuildingArea", "YearBuilt", "Propertycount")

nuevo


prediccion.price <- predict(arbol, newdata = nuevo
)
# La predicción para la casa 1
datos.Valida[1,]



#imprime la prediccion
prediccion.price[1]