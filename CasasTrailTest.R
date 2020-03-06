library(readr)
library(dplyr)
library(ggplot2)
library(reshape) # Para renombrar columnas
#install.packages("reshape")

entrena <- read_csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/MachineLearning/datos/train.csv")

# Los datos de validación
valida <- read.csv("C:/Users/Sergio/Documents/DIPLOMADO IOT DATA CIENCE/MODULO 5 MACHINE LEARNING/Proyecto/MachineLearning/datos/test.csv")

head(entrena)
head(valida)


str(entrena)

summary(entrena)


nrow(entrena) # Registros

ncol(entrena) # Columnas



# Se visualiza como una distribución normal
ggplot(entrena, aes(x=SalePrice)) + geom_histogram()


sapply(entrena, is.numeric) # Cuáles son ?


cualesNumericas <- which(sapply(entrena, is.numeric))
nombresColumnas <- names(cualesNumericas)
nombresColumnas


# Ver el diccionario de datos ???

# Conjunto 
entrenaNumericas <- select(entrena, nombresColumnas)
entrenaNumericas


correlaciones <- data.frame(cor(entrenaNumericas))
entrenaCol.Correl.Price <- data.frame(cbind("variable" = rownames(correlaciones),"correlacion"= correlaciones$SalePrice)) 
entrenaCol.Correl.Price



entrenaCol.Correl.Price <- arrange(entrenaCol.Correl.Price, desc(correlacion))
entrenaCol.Correl.Price


head(entrenaCol.Correl.Price) 



ggplot(entrenaNumericas, aes(x=OverallQual, y=SalePrice)) +
  geom_point(color="darkred") + geom_smooth(method = "lm")



ggplot(entrenaNumericas, aes(x=GrLivArea, y=SalePrice)) +
  geom_point(color="darkgreen") + geom_smooth(method = "lm")



ggplot(entrena, aes(x=GarageCars, y=SalePrice)) +
  geom_point(color="red") + geom_smooth(method = "lm")




ggplot(entrenaNumericas, aes(x=GarageArea, y=SalePrice)) +
  geom_point(color="red") + geom_smooth(method = "lm")


ggplot(entrenaNumericas, aes(x=TotalBsmtSF, y=SalePrice)) +
  geom_point(color="orange") + geom_smooth(method = "lm")




ggplot(entrenaNumericas, aes(x="1stFlrSF", y=SalePrice)) +
  geom_point(color="purple") + geom_smooth(method = "lm")


entrenaNumericas = rename(entrenaNumericas, c(`1stFlrSF`="lstFlrSF"))  # opcion 2, renombra la variable


modelo1 <- lm(formula = SalePrice ~ OverallQual + 
                GrLivArea + GarageCars + GarageArea +
                TotalBsmtSF +  + FullBath + lstFlrSF +
                TotRmsAbvGrd + YearBuilt + YearRemodAdd, entrenaNumericas)
summary(modelo1)


entrenaNumericas = rename(entrenaNumericas, c(`2ndFlrSF`="tndFlrSF"))  # renombra la variable
entrenaNumericas = rename(entrenaNumericas, c(`3SsnPorch`="tSsnPorch"))  # renombra la variable

modelo2 <- lm(formula = SalePrice ~ ., data = entrenaNumericas)
modelo2


summary(modelo2)


modelo3 <- lm(formula = SalePrice ~ LotArea + 
                OverallQual + OverallCond + YearBuilt +
                BsmtFinSF1 + lstFlrSF + tndFlrSF +
                BedroomAbvGr + TotRmsAbvGrd + GarageCars +ScreenPorch, entrenaNumericas )
modelo3

summary(modelo3)




install.packages("MASS")




#quita variables de forma automatica stepAIC

library(MASS)

step.modelo2 <- stepAIC(modelo2, direction = "backward")
summary (modelo2)









