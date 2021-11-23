datos<-read.table("prostata.txt",header = TRUE)
reg<-lm(lpsa~.,data=datos)
summary(reg)
names(reg)
names(summary(reg))

unos<-rep(1,nrow(datos))
unos

X<-as.matrix(cbind(unos,datos[,1:8]))
head(X)

#estimacion de los beta

betas_sombrero<-solve(t(X)%*%X)%*%t(X)%*%datos$lpsa
cbind(reg$coefficients,betas_sombrero)
reg$coefficients==betas_solve

#estimacion de la varianza de los errores

s2<-sum(reg$residuals^2)/(nrow(X)-ncol(X))
summary(reg)$sigma^2
s2

#############################################################################
#### Para generar muestras pseudo-aleatorias
rm(list=ls())

set.seed(123)
ene=50
Nrep=1000
coef4<- c()
coef14<- c()

for (i in 1:Nrep)
{   x1=rnorm(ene,0,1)
eps=rnorm(ene,0,4)

y=1+10*x1+eps
coef4[i]<- lm(y~x1)$coef[2]
}

set.seed(123)
ene=50


for (i in 1:Nrep)
{   x1=rnorm(ene,0,1)
eps=rnorm(ene,0,1/4)

y=1+10*x1+eps
coef14[i]<- lm(y~x1)$coef[2]
}

boxplot(coef4,coef14)

###########################################
set.seed(999)

pp<- 1:20
pp
sample(pp)
sample(pp,12)


