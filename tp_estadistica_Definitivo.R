rm(list=ls())
setwd("C:/hcgalvan/Repositorios/UBA-R-programming-CsDatos/Datos")
library(GGally)
library(MASS)


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
ajuste_lineal <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
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
La distribución de los errores estandarizados se concentran más alrededor
del 0 en una franja de (-1,1), salvo un outlier.
'
plot(estimacion,rEstandar, col="red", pch=19)
abline(h = 0)


##Item 7
## a)
modelo<-regsubsets(y~.,data = datos[,-1], method = "forward", nvmax = 9)
summary(modelo)$adjr2
summary(modelo)
## viendo ambos summary el modelo 3 es el que tiene mayor R^2 ajustado = 0.79
## y esta formado por las variables x4, x8 y x9. Esto nos dice que las variables
##x4, x8 y x9 explican a y aproximadamente en un 80%

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
modelo_2<- regsubsets(y~., data = datos[train,-c(1)], method = "forward", nvmax = 9)
summary(modelo_2)
##calculo un vector que contiene los ECM de validacion de cada modelo
ECM<- array(NA, 9)
for(i in 1:9)
{ 
  predichos<- predic(modelo_2, datos[test, -c(1)], i)
  ECM[i]<- MSE(y[test], predichos)  
}

plot(ECM)
##el modelo de menor error cuadratico medio de validacion(184180.2) es el que tiene una sola variable
##es decir, el modelo que cuenta con intercept y la variable x4

### 8 ##########################################
library(ISLR)
library(plotmo)  
library(glmnet)

#Creo la matriz de diseño y el vector de respuestas de testeo
logx <- model.matrix(log(y)~.,log(datos[,-1]))[,-1]
logy <- log(y)
logy.test <-log(y[test])

# Calculo LASSO sobre la muestra de entrenamiento
##usando la misma división de datos que usamos en 7 b)
grid<- 10^seq(10,-2,length=100)
lasso.mod <- glmnet (logx[train ,], logy[train], alpha=1, lambda=grid) 

#LASSO - Ploteamos y calculamos el lambda de un desvio standar  
set.seed(1)
cv.out <- cv.glmnet(logx[train,], logy[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.1se
coef(lasso.mod, s = cv.out$lambda.1se)

##Se observa que fueron eliminadas las variables x5 y x7

# Calculamos el ECM sobre la muestra de validación
lasso.pred <- predict(lasso.mod,s=cv.out$lambda.1se, newx=logx[test,])
ECM.cv <- mean((lasso.pred-logy.test)^2)

##El ECM de validación cruzada = 0.2 resulta ser mucho más chico que en el modelo del item 7b)

####De los diferentes modelos que vimos en el tp elegimos el del punto 8
####dado que, si bien tiene 7 variables se observa un ECM de validación muy pequeño
####lo que nos garantiza un alto grado de predicción
####Aunque tendríamos en cuenta el modelo del item 7 b) dado que cuenta con solo 3 variables
####y tiene R^2 ajustado cercano al 80%

### Item 9
##Realizamos un bootstrap no paramétrico
B<- 1000
n<- length(datos[,1])
m<- length(datos[1,])-1
est.coef<- matrix(NA, B, m)
for(i in 1:B){
  index.row<- sample((1:n), n, replace = TRUE)
  datos.boot<- datos[index.row,]
  est.coef[i,]<- coef(lm(y~., data = datos.boot[,-1]))
} 

##Hacemos un test de normalidad para el coeficiente estimado de x4
hist(est.coef[,5], freq = FALSE, breaks = 200, main="Densidad del coeficiente de x4 del EMC", xlab="")
lines(density(est.coef[,5], kernel="gaussian"), width = 100, col="red", lwd=2)
qqnorm(est.coef[,5])
boxplot(est.coef[,5])

##El histograma junto con el gráfico de la densidad nos hacen pensar que la distribución es normal
##En el boxplot se observa una fuerte simetría en el cajón central  
##Podemos observar con el QQplot una buena correlación con la normal
##De esta manera comprobamos la normalidad del coeficiente de la variable x4 del EMC








