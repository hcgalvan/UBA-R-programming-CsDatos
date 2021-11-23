#veamos como resulta el ejercicio 2

cargamento<-read.csv("glakes.csv")

#modelo sin intercept
w<-c()

for( i in 1:nrow(cargamento))
{
  datos<-cargamento[-i,]
  reg<-lm(Time~Tonnage-1,data=datos)
  predicho<-reg$coefficients[1]*cargamento[i,2]
  w[i]<-(cargamento[i,3]-predicho)^2
}
W1<-sum(w)

#modelo con intercept
w<-c()
for( i in 1:nrow(cargamento))
{
  datos<-cargamento[-i,]
  reg<-lm(Time~Tonnage,data=datos)
  predicho<-reg$coefficients[1]+reg$coefficients[2]*cargamento[i,2]
  w[i]<-(cargamento[i,3]-predicho)^2
}
W2<-sum(w)

#modelo 3
w<-c()
for( i in 1:nrow(cargamento))
{
  datos<-cargamento[-i,]
  y<-log(datos[,3])
  x<-datos[,2]^(0.25)
  reg<-lm(y~x)
  predicho<-exp(reg$coefficients[1]+
                  reg$coefficients[2]*(cargamento[i,2])^(0.25))
  w[i]<-(cargamento[i,3]-predicho)^2
}
W3<-sum(w)


#modelo 4
w<-c()
for( i in 1:nrow(cargamento))
{
  datos<-cargamento[-i,]
  y<-log(datos[,3])
  x1<-datos[,2]^(0.5)
  x2<-datos[,2]^(0.25)
  reg<-lm(y~x1+x2)
  predicho<-exp(reg$coefficients[1]+
                  reg$coefficients[2]*(cargamento[i,2])^(0.5)+
                  reg$coefficients[3]*(cargamento[i,2])^(0.25))
  w[i]<-(cargamento[i,3]-predicho)^2
}
W4<-sum(w)


W<-c(W1,W2,W3,W4)
plot(1:4,W,pch=20,col="darkblue")



#Ya que estamos en un caso que puede graficarse
#veamos como son los diferentes ajustes...

reg1<-lm(Time~Tonnage-1,data=cargamento)
reg2<-lm(Time~Tonnage,data=cargamento)
y<-log(cargamento[,3])
x1<-cargamento[,2]^(0.5)
x2<-cargamento[,2]^(0.25)
reg3<-lm(y~x2)
x<-seq(min(cargamento$Tonnage),max(cargamento$Tonnage),length=100)
predicho3<-exp(reg3$coefficients[1]+
                 reg3$coefficients[2]*x^(0.25))
reg4<-lm(y~x1+x2)
predicho4<-exp(reg4$coefficients[1]+reg4$coefficients[2]*x^(0.5)+
                 reg4$coefficients[3]*x^(0.25))

plot(cargamento$Tonnage,cargamento$Time,pch=20,col="darkblue", 
     xlab= "Peso", ylab="Tiempo",
     ylim=c(0,150))

lines(cargamento$Tonnage,reg1$fitted.values,col="chocolate",lty=6,lwd=1.5)
lines(cargamento$Tonnage,reg2$fitted.values,col="firebrick",lty=6,lwd=1.5)
lines(x,predicho3,col="deepskyblue4",lty=6,lwd=1.5)
lines(x,predicho4,col="forestgreen",lty=6,lwd=1.5)

#recordemos que el 3 (azul) es el que mejor nos dió



#Ahora una ayuda para arrancar con el ejercicio 3. 
#Queremos simular datos, y simular muchas muestras
#para poder hacer muchas estimaciones de beta 
#y con ello poder entender algunos escenarios posibles


set.seed(27)
n<-100
x1<-runif(n)
x2<-runif(n)
eps<-rnorm(n,0,0.5)
y<-4+2*x1-3*x2+eps


#no puedo graficar todo junto, grafico de a pares
datos<-as.data.frame(cbind(x1,x2,y))
library(GGally)
ggpairs(datos)


#ahora quiero realizar un ajuste 
reg<-lm(y~x1+x2)
summary(reg)
beta_som<-reg$coefficients
beta_som
#bastante cerca...

#ahora, como sería tener N=1000 muestras y por lo tanto 1000 estimaciones de beta

set.seed(27)

N<-1000
Betas<-matrix(0,nrow = N,ncol = 3)
for(i in 1:N)
{
  x1<-runif(n)
  x2<-runif(n)
  eps<-rnorm(n,0,0.5)
  y<-4+2*x1-3*x2+eps
  reg<-lm(y~x1+x2)
  Betas[i,]<-reg$coefficients
}

colnames(Betas)<-c("b_0","b_1","b_2")
boxplot(Betas,col=c("darkblue", "chocolate", "forestgreen"))
abline(h=4,lty=2,col="skyblue")
abline(h=2,lty=2,col="skyblue")
abline(h=-3,lty=2,col="skyblue")

boxplot(Betas[,1])$out

qplot(Betas[,1],geo)


#uno más, para repasar (o aprender) construcción de funciones


#crear una función que, dado un listado de personas, elija 3 al azar, y 
#usarla para elegir 3 personas de la lista (considerada en orden)

elijo<-function(v,n)# v es el vector de posiciones, n el tamaño de la muestra
{
  e<-sample(v,n,rep=FALSE)
  return(e)
}

elijo(1:21,3)


#crear una funcion que, dados los coeficientes a,b,c de aX^2+bX+c
#devuelva las raices
