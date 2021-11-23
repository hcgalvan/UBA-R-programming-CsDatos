### Vuelo a considerar el problema de las lanas contemplando que 
### admito que la esperanza y la varianza no coincidan considerando
### E(Y_i)=mu_i y Var(Y_i)= phi mu_i


## Leo los datos
require(datasets)
telas <- warpbreaks
attach(telas)

## Ajusto un modelo con interaccion
ajuste<-glm(breaks~wool*tension,family="poisson", data=telas)
summary(ajuste)

## Evaluo la bondad del ajuste globalmente
1-pchisq(182.31,48)

## Miro como dan los mu.hat_i respecto de los  (Y_i-mu.hat_i)^2 
res<- breaks-ajuste$fitted.values
plot(ajuste$fit,res*res,ylim=c(0,45))
lines(ajuste$fit,ajuste$fit,col="red")

## Ahora estimo a phi
dphi<-sum(resid(ajuste,type="pearson")*resid(ajuste,type="pearson"))/summary(ajuste)$df.residual

## Tengo en cuenta este estimador de phi para evaluar la significacion de los parametros
## y del ajuste global

summary(ajuste,dispersion=dphi)

1-pchisq(182.31/dphi,48)

## Vuelvo a mirar el grafico mu.hat_i respecto de los  (Y_i-mu.hat_i)^2 
## incoprprando la estimacion de phi

plot(ajuste$fit,res*res/dphi,ylim=c(0,45))
lines(ajuste$fit,ajuste$fit,col="red")




