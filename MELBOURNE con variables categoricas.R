# Regresión mútiple con datos de melbourne
library(readr)
library(dplyr)
library(ggplot2)
library(reshape) # Para renombrar columnas
library(caret) # Para particiones
library(corrplot)

getwd()
datos <- read_csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/MachineLearning/datos/melb_data.csv")

head(datos)
tail(datos)

# Crear conjunto de datos entrenamiento y valdiación
set.seed(2020) # Semilla
entrena <- createDataPartition(datos$Price, p=0.7, list = FALSE)
head(entrena)
nrow(entrena)

# Los registros que no estén en entrena
head(datos[-entrena,])
nrow(datos[-entrena,])


# Ver los primeros seis datos con sólo variables numéricas
head(datos)

# Ahora a determinar conjuntos de datos de entrenamiento y luego head()
datos.Entrena <- datos[entrena,]
head(datos.Entrena)
summary(datos.Entrena)

# y conjunto de datos de validación y luego head()
datos.Valida <- datos[-entrena,]
head(datos.Valida)
summary(datos.Valida)


# Asegurarse que se tienen los datos categóricos character 
# como tipo factor
# [1] "Suburb"        "Address"       "Rooms"        
# [4] "Type"          "Price"         "Method"       
# [7] "SellerG"       "Date"          "Distance"     
# [10] "Postcode"      "Bedroom2"      "Bathroom"     
# [13] "Car"           "Landsize"      "BuildingArea" 
# [16] "YearBuilt"     "CouncilArea"   "Lattitude"    
# [19] "Longtitude"    "Regionname"    "Propertycount"

# Seleccionar ciertas variables de interés
datos.Entrena <- select(datos.Entrena, Suburb,Address,SellerG,Date,Postcode,CouncilArea, Lattitude, Longtitude,Regionname )
modelo <- lm(data = datos.Entrena, formula = Price ~ .)
modelo



datos.Num <- select(datos, Price, Rooms, Distance, Bedroom2, Bathroom, Car, Landsize, BuildingArea, YearBuilt, Propertycount) 
head(datos.Num)
#actualizar librerya dplyr n caso de error


str(datos.Num)


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







correlaciones <- cor(datos.Num)
correlaciones


corrplot(correlaciones, method = "number")