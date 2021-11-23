rm(list=ls())
setwd("C:/hcgalvan/Repositorios/UBA-R-programming-CsDatos/Datos")

library(GGally)
library(MASS)
#setwd("R")
datos <- read.table("peak.txt", header = TRUE)
attach(datos)

## Item 1
ggpairs(datos[,-c(1)])
cor(datos[,-c(1)])

## las covariables que podrían ayudar a explicar la variable Y serían 
## las que tienen una correlación mayor a 0.5 x1, x2, x4, x5, x7
## si tuviera que elegir una sería la de mayor correlación con Y
## en este caso x4

## Item 2
ajuste_lineal <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9, x = TRUE, y = TRUE)
EMC <- ajuste_lineal$coefficients
summary(ajuste_lineal)
## a UN NIVEL DE SIGNIFICACIÓN 0.05 solo hay evidencia suficiente para afirmar
## que no es 0 la variable x8, y a un NIVEL DE SIGNIFICACIÓN 0.1
## evidencia suficiente de que no son nulas las variables x8 y x9
## la significacion del test viene dada por el test (x1,x2,...,x9) = (0,0,...,0)
## vs algún xi es distinto de 0. Dado que el p-valor de dicho test es 9.74x10^-6<<0.05
## hay evidencia suficiente para afirmar que la regresión es significativa


##Item 3
estimacion <- ajuste_lineal$fitted.values
ajuste_lineal$coefficients

X <- model.matrix(ajuste_lineal)
matrizP <- X%*%solve(t(X)%*%X)%*%t(X)

##ERROR TRANCA
r <- (y -ajuste_lineal$fitted.values)

S <- summary(ajuste_lineal)$sigma
rEstandar <- r/(S*sqrt(1 - diag(matrizP)))

##Validamos los supuestos observando que los errores estandarizados
##son una nube aleatoria al rededor del cero.

plot(estimacion,rEstandar, col="red", pch=19)
abline(h = 0)


##Item 4

matrixDatosLn <- apply(datos, MARGIN = 2 ,FUN = log)
datosLn <- data.frame(matrixDatosLn)
attach(datosLn)

'
En la matriz de correlación nos muestra que hay mayor correlacion en las primeras
5 xs
No tendríamos que ver tanto x8 y x9 y concentrarnos en las primeras 5 y la x7 (si tomamos
las variables con correlación al menos 0.5)
x7 baja de 0.66 a 0.53

conclusión: dejariamos de usar x7 ya que antes estaba en el límite y 
ahora siguió bajando. Además consideraríamos incluir a x3 en el modelo.
'

ggpairs(datosLn[,-c(1)])
cor(datosLn[,-c(1)])

colnames(datosLn) <- colnames(datos)

##Item 5

'
Pasamos de tener 2 variables significativas con un nivel de significación 0.1,
ahora tenemos un modelo con 4 variables significativas con un nivel de 
significaicon 0.05.

Significativamente distintos de 0 son x1, x6, x8 y x9

Eliminaríamos las que no mencionamos arriba

Es significativo ya que tenemos un p-value del test F = 2.2e-16 << 0.05
'

ajuste_lineal <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
EMC <- ajuste_lineal$coefficients
summary(ajuste_lineal)

##Item 6

estimacion <- ajuste_lineal$fitted.values
ajuste_lineal$coefficients

XLn <- model.matrix(ajuste_lineal)
matrizPLn <- X%*%solve(t(X)%*%X)%*%t(X)

##ERROR TRANCA
r <- (y -ajuste_lineal$fitted.values)

S <- summary(ajuste_lineal)$sigma
rEstandar <- r/(S*sqrt(1 - diag(matrizPLn)))

'
La distribución de los errores estandarizados se concentran más al rededor
del 0 en una franja de (-1,1), salvo un outlier.
'

plot(estimacion,rEstandar, col="red", pch=19)
abline(h = 0)


##Item 7

## a)

x<-cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9)
modelos<-regsubsets(y~x,data = datos, method = "exhaustive", nvmax = 9)
summary(modelos)$adjr2
summary(modelos)
## viendo ambos summary el modelo 3 es el que tiene mayo R^2 ajustado
## y esta formado por las variables x4, x8 y x9

## b)

##funcion que calcula el MSE
MSE <- function(y, prediccion){
  return(mean((y - prediccion)^2))
}

##funcion que calcula los valores predichos
predic <- function(modelo, data, id){
  form<- as.formula(modelo$call[[2]])
  matriz<- model.matrix(form, data)
  coefi<- coef(modelo, id = id)
  variables<- names(coefi)
  predic<- matriz[,variables]%*%coefi
  return(predic)
}
set.seed(1)
train<- sample(c(TRUE, FALSE), nrow(datos[,-c(1)]), rep = TRUE, prob = c(0.7, 0.3))
test<- (!train)
modelo_2<- regsubsets(y~.,data = datos[train,-c(1)], method = "exhaustive", nvmax = 9)
summary(modelo_2)

##calculo un vector que contiene los MSE de validacion de cada modelo
ECM<- array(NA, 9)
for(i in 1:9)
{ 
  predichos<- predic(modelo_2, datos[test, -c(1)], i)
  ECM[i]<- MSE(y[test], predichos)  
}

plot(ECM)
##el modelo de menor error cuadratico medio de validacion es el que tiene una sola variable
##es decir, el modelo que con intercept y la variable x4

