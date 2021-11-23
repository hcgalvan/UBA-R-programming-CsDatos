rm(list=ls())
setwd("C:/hcgalvan/Repositorios/UBA-R-programming-CsDatos/Datos")

library(GGally)
datos <- read.table("peak.txt", header = TRUE)
attach(datos)

## Item 1
ggpairs(datos[,-c(1)])
cor(datos[,-c(1)])

## las covariables que podrían ayudar a explicar la variable Y serían 
## las que tienen una correlación mayor a 0.5 x1, x2, x4, x5, x7
## si tuviera que elegir una sería la de mayor correlación con Y
## en este caso x4

ggplot(datos,aes(x= x4, y =  y))+
  geom_point()
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
La distribución de los errores estandarizados se concentran más al rededor
del 0 en una franja de (-1,1), salvo un outlier.
'

plot(estimacion,rEstandar, col="red", pch=19)
abline(h = 0)

## 7.

--------------------------------------
# Ajuste lineal de datos, variable dependiente "y" resto variables como covariables, 
# luego vemos como resulta los valores con summary
ggpairs(datos)
# 2)
reg <- lm(y~., data=datos)
summary(reg)
#Intercept
summary(reg)$coeff[1,1]
# El modelo propuesto será: y = ??0 + ??1x8 + e
# El modelo ajustado será: y = 1255.04 + 452.78x8

##################################################
# 7)  Mediante el procedimiento Stepwise, seleccionar el mejor modelo:
stepwiseF_RAjustado <- function (tabla, orden){ 
  intercept <- lm(y~1) 
  formula <- "y~" 
  columnas <- colnames(datos)[-c(1,11)] 
  modelo <- NULL 
  primero <- TRUE 
  for(i in 1:orden){ 
    queda <- "x1" 
    RAjustado_final <- 0 
    for(param in columnas){ 
      if(param %in% formula){ 
        print(param) 
        next 
      } 
      if(primero){ 
        modelo <- lm(as.formula(paste(formula,param))) 
      }else{ 
        modelo <- lm(as.formula(paste(formula,"+",param))) 
      } 
      RAjustado <- summary(modelo)$adj.r.squared 
      if(RAjustado_final < RAjustado){ 
        queda <- param 
        RAjustado_final <- RAjustado 
      } 
    } 
    primero <- FALSE 
    formula <- paste(formula,"+",queda) 
  } 
  return(modelo) 
} 

summary(stepwiseF_RAjustado(datos,5))

#####################################################
# MODELO ALTERNATIVO DE LA FUNCION ARMADA MANUALMENTE
#####################################################
#defino el modelo de solo intercept
intercept_only <- lm(y~1, data=datos)

#defino el modelo con todos los predictores
all <- lm(y~., data=datos)

#Realizo regresion stepwise hacia adelante (forward)
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=TRUE)

#Visualizo los resultados de la regresion utilizada
forward$anova

#Visualizo el modelo final
forward$coefficients

### Visualizo el ajuste
summary(forward)$adj.r.squared

### Graficar el ajuste
plot(forward, main="Forward", pch=19, cex=1, which=1)
################################################
### 8 ##########################################
#cargo librerias
library(ISLR)
library(plotmo)     # para graficar
library(glmnet)

# ajuste_lineal <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
# EMC <- ajuste_lineal$coefficients
# summary(ajuste_lineal)

# Ajustamos minimos cuadrados con todas las covariables
summary(lm(y~.,data=datos))

#Creamos la muestra de entrenamiento y de validacion
set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(datos),rep=TRUE)
test <- (!train )

#Creo la matriz de diseño y vector de respuestas 
x <- model.matrix(y~.,datos)[,-1]
y <- datos$y
y.test=y[test]

# Calculo LASSO
grid<- 10^seq(10,-2,length=100)
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid) #por default estandariza variables

## Veo los coeficientes
dim(coef(lasso.mod))
lasso.mod$lambda

#Creo la matriz de diseño y vector de respuestas 
x <- model.matrix(y~.,datos)[,-1]
y <- datos$y
y.test=y[test]

#LASSO - Utilizamos ploteamos cross validation
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha =1)
plot(cv.out)
bestlam <- cv.out$lambda.min
log(bestlam)

#Tmb tenemos el log de la regla de un desvio standard, 

log(cv.out$lambda.1se)
coef(lasso.mod,s=cv.out$lambda.1se)

# Vemos que coeficientes son eliminados y cuales no
summary(coef(lasso.mod,s=cv.out$lambda.1se))

# Aqui utilizamos para realizar predecir
lasso.pred=predict(lasso.mod,s=cv.out$lambda.1se,newx=x[test,])
mean((lasso.pred-y.test)^2)
# 
summary(coef(lasso.mod,s=cv.out$lambda.1se))
# ENTONCES, COMO CONCLUSION A TRAVES DEL MODELO DE REGULARIZACIÓN LASSO
# Vemos que las variables x1, x6 y x9 no fueron eliminadas
# NO ES MUY DIFERENTE AL PUNTO 5, SOLAMENTE FUE ELIMNADO EL X8


# CALCULAMOS MSE en la muestra de validacion cruzada???
coef(lasso.mod,s=cv.out$lambda.min)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)