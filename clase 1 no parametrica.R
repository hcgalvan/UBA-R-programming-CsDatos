library(ggplot2)
library(gridExtra)

bufalo<-scan("buffalo.txt")
datos<-data.frame("nieve"= bufalo)

# Histogramas
#1
hist(bufalo, col="deepskyblue")

par(mfrow=c(2,2))
hist(bufalo, col="deepskyblue",breaks =seq(20,130,10))
hist(bufalo, col="deepskyblue",breaks =seq(22,132,10))
hist(bufalo, col="deepskyblue",breaks =seq(24,134,10))
hist(bufalo, col="deepskyblue",breaks =seq(26,136,10)) #\\(0.0)//
par(mfrow=c(1,1))

#2
hist(bufalo, col="deepskyblue",breaks =c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130))
par(mfrow=c(1,1))

# con ggplot2
g1<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(20,130,10)) + 
  theme_light()


g2<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(22,132,10)) + 
  theme_light()

g3<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(24,134,10)) + 
  theme_light()

g4<-ggplot(datos, aes(X1)) + 
  geom_histogram(aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.5, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(26,136,10)) + 
  theme_light()
grid.arrange(g1, g2, g3,g4, nrow = 2)

#3 Estimemos la probabilidad

estimamos<-function(x,h,Datos)
{
  p_est<-sum(Datos<=(x+h) & Datos>=(x-h))/length(Datos)
  return(p_est)
}

x<-sort(bufalo) #los ordeno solo porque cuando los grafico quiero que me funcione bien el comando lines


h<-10
est1<-c()
for(i in 1:length(bufalo))
{
  est1[i]<-estimamos(x[i],h,bufalo)
  
}

h<-20
est2<-c()
for(i in 1:length(bufalo))
{
  est2[i]<-estimamos(x[i],h,bufalo)
  
}

h<-30
est3<-c()
for(i in 1:length(bufalo))
{
  est3[i]<-estimamos(x[i],h,bufalo)
  
}

plot(x,est3, col=col3, type="l", lwd=2)
lines(x,est1, col=col1, type="l", lwd=2)
lines(x,est2, col=col2, type="l", lwd=2)

estimamos(50,10,bufalo)


# Estimadores basados en nucleos

# Estimacion Parzen
uniforme<-function(u)
{
  ifelse(u>-1 & u<1,1,0)/2
}


#para nucleos
f_sombrero<-function(x,k,datos,h) #datos= Xi
{
  s<-0
  for(i in 1:length(datos))
  {
    c<-k((x-datos[i])/h)
    s<-s+c
  }
  f<-s/(length(datos)*h)
  return(f)
}  


densidad.est.parzen<-function(x,h,z) # x: datos, z:valor donde exaluo la f
{
  f_sombrero(z,uniforme,x,h)
}


nuevos<-seq(25,126.4,length=200)
h<-10
f_estimada1<-densidad.est.parzen(datos$nieve,h,nuevos)
f_est<-data.frame("x"=nuevos,  "estimada1"=f_estimada1)

densidad.est.parzen(bufalo,10, bufalo[1])

ggplot(f_est,aes(x=x,y=estimada1))+
  geom_line(col="steelblue")+
  theme_light()


h<-20
f_est$estimada2<-densidad.est.parzen(datos$nieve,h,nuevos)

h<-30
f_est$estimada3<-densidad.est.parzen(datos$nieve,h,nuevos)


ggplot(f_est)+
  geom_histogram(data=datos,aes(x=nieve,y = ..density..),binwidth = 2,alpha=0.3, 
                 fill = "mediumpurple3", color = "black",
                 breaks =seq(25,126.4,10)) + 
  geom_line(aes(x=x,y=estimada1),col="steelblue",lwd=1.5)+
  geom_line(aes(x=x,y=estimada2),col="firebrick",lwd=1.5)+
  geom_line(aes(x=x,y=estimada3),col="olivedrab4",lwd=1.5)+
  theme_light()



#Con density. Density evalua sobre una grilla equiespaciada
h<-5

#help(density)  kernel = c("gaussian", "epanechnikov", "rectangular",
 #                         "triangular", "biweight",
  #                        "cosine", "optcosine"),

hist(datos$nieve, freq = FALSE,ylim=c(0,0.02))
h<-10
lines(density(datos$nieve,kernel = "gaussian",window=h),col="yellowgreen",lwd=2)
lines(density(datos$nieve,kernel = "epanechnikov",window=h),col="firebrick",lwd=2)
lines(density(datos$nieve,kernel = "rectangular",window=h),col="steelblue",lwd=2)



# Uso mi función que da lo mismo que el density, pero la puedo evaluar en los puntos que yo quiero.
gauss<-function(u)
{
  k<-exp(-(u^2)/2)/sqrt(2*pi)
  return(k)
}

estimada5<-f_sombrero(sort(datos$nieve),gauss,datos$nieve,10)


# Otros nucleos
epa<-function(u)
{
  ifelse(abs(u) < 1,3/4*(1-u^2),0)
} 


