#B
Sa<-read.table("salchichas_A.txt", header=TRUE)
Sb<-read.table("salchichas_B.txt", header=TRUE)
Sc<-read.table("salchichas_C.txt", header=TRUE)

#graficar uno al lado del otro
par(mfrow=c(1,3))
hist(Sa$CALORIAS.A, col="chocolate", freq=FALSE)
hist(Sb$CALORIAS.B, col="chocolate", freq=FALSE)
hist(Sc$CALORIAS.C, col="chocolate", freq=FALSE)
par(mfrow=c(1,1))


# lo mismo con los boxplot
par(mfrow=c(1,3))
boxplot(Sa$CALORIAS.A, col="chocolate", ylim=c(80,200))
boxplot(Sb$CALORIAS.B, col="chocolate", ylim=c(80,200))
boxplot(Sc$CALORIAS.C, col="chocolate", ylim=c(80,200))
par(mfrow=c(1,1))

#o 
boxplot(Sa$CALORIAS.A,Sb$CALORIAS.B,Sc$CALORIAS.C, col=colores[4:6])

# con ggplot2
library(ggplot2)

#para superponerlos tienen que estar en el mismo data frame y no pueden medir diferente...
salchichas<-cbind(Sa[1:17,],Sb,Sc)
ggplot(salchichas) + 
  geom_histogram(aes(x=CALORIAS.A,y = ..density..),alpha=0.5, bins=8,
                 fill = col1, color = "black") + 
  geom_histogram(aes(x=CALORIAS.B,y = ..density..),alpha=0.5, bins=8,
                 fill = col2, color = "black") + 
  geom_histogram(aes(x= CALORIAS.C,y = ..density..),alpha=0.5, bins=8,
                 fill = col3, color = "black") + 
  theme_light()

# si no hago lo mismo que antes y los pongo al lado usando grid.arrange

library(gridExtra)
g1<-ggplot(Sa) + 
  geom_histogram(aes(x=CALORIAS.A,y = ..density..),alpha=0.5, bins=8,
                 fill = col1, color = "black") +   theme_light()
g2<-ggplot(Sb) + 
  geom_histogram(aes(x=CALORIAS.B,y = ..density..),alpha=0.5, bins=8,
                 fill = col2, color = "black") +   theme_light()
g3<-ggplot(Sc) + 
  geom_histogram(aes(x=CALORIAS.C,y = ..density..),alpha=0.5, bins=8,
                 fill = col3, color = "black") +   theme_light()

grid.arrange(g1, g2, g3, ncol=3, nrow =1)


#tengo una mejor!!

clase<-c(rep("A", 20), rep("B", 17), rep("C", 17))
Calorias<-data.frame("Calorias"=c(Sa$CALORIAS.A,Sb$CALORIAS.B,Sc$CALORIAS.C),"Tipo"=clase)

ggplot(Calorias, aes(x=Calorias, fill=Tipo)) + 
  geom_histogram(aes(x=Calorias,y = ..density..),alpha=0.5, color="black")+
  theme_light()

ggplot(Calorias, aes(y=Calorias, fill=Tipo)) + 
  geom_boxplot()+
  theme_light()


#para graficar una funcion cualquiera
x<-seq(-50, 50, length=10000)
plot(x,cos(x),type="l")
lines(x, sin(x), col="chocolate")
points(5:10,cos(5:10), col="firebrick", pch=15)
lines(x, dnorm(x))
