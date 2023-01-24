################################################################################
#################  TALLER ESTAD?STICA B?SICA PARA ECONOMETR?A  #################
################################################################################
# Nombre : Gianfranco D. Chamorro R.
rm(list = ls())

install.packages("data.table")
library(data.table)
library(ggplot2)

#  LEY DE LOS GRANDES N?MEROS 

# 5 LANZAMIENTOS
vector = vector(length = 5)
muestra = sample(c(1,2,3,4,5,6),5,replace = T,prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
for (i in 1:5){
  vector[i]=mean(muestra[1:i])
}
i=1:5 #index
ggplot()+geom_line(aes(i,vector),col="blue",size=1.1)+geom_hline(yintercept = 3.5,col="red",linetype="solid")+
  ggtitle("TDLGN")+xlab("Lanzamientos")+ylab(expression(Promedio))+labs( caption = "Elaboraci?n propia") 

# 10 LANZAMIENTOS
vector = vector(length = 10)
muestra = sample(c(1,2,3,4,5,6),10,replace = T,prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
for (i in 1:10){
  vector[i]=mean(muestra[1:i])
}
i=1:10 #index
ggplot()+geom_line(aes(i,vector),col="blue",size=1.1)+geom_hline(yintercept = 3.5,col="red",linetype="solid")+
  ggtitle("TDLGN")+xlab("Lanzamientos")+ylab(expression(Promedio))+labs( caption = "Elaboraci?n propia") 

# 100 LANZAMIENTOS
vector = vector(length = 100)
muestra = sample(c(1,2,3,4,5,6),100,replace = T,prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
for (i in 1:100){
  vector[i]=mean(muestra[1:i])
}
i=1:100 #index
ggplot()+geom_line(aes(i,vector),col="blue",size=1.1)+geom_hline(yintercept = 3.5,col="red",linetype="solid")+
  ggtitle("TDLGN")+xlab("Lanzamientos")+ylab(expression(Promedio))+labs( caption = "Elaboraci?n propia") 

# 1000 LANZAMIENTOS
vector = vector(length = 1000)
muestra = sample(c(1,2,3,4,5,6),1000,replace = T,prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
for (i in 1:1000){
  vector[i]=mean(muestra[1:i])
}
i=1:1000 #index
ggplot()+geom_line(aes(i,vector),col="blue",size=1.1)+geom_hline(yintercept = 3.5,col="red",linetype="solid")+
  ggtitle("TDLGN")+xlab("Lanzamientos")+ylab(expression(Promedio))+labs( caption = "Elaboraci?n propia") 

# 10000 LANZAMIENTOS
vector = vector(length = 10000)
muestra = sample(c(1,2,3,4,5,6),10000,replace = T,prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
for (i in 1:10000){
  vector[i]=mean(muestra[1:i])
}
i=1:10000 #index
ggplot()+geom_line(aes(i,vector),col="blue",size=1.1)+geom_hline(yintercept = 3.5,col="red",linetype="solid")+
  ggtitle("TDLGN")+xlab("Lanzamientos")+ylab(expression(Promedio))+labs( caption = "Elaboraci?n propia") 

#      TEOR?A L?MITE CENTRAL 

datos <- fread("D:/JR-ECONOMETRICS/Talleres/TallerEconometría2023/Sesion2/toy_dataset.csv")
datosmean <- mean(datos$Income)
datossd <- sd(datos$Income)
p <- ggplot(datos) 
p + aes(x = Income) + geom_histogram()

d <- density(datos$Income, adjust = 1.8)
plot(d, main = "Densidad de la variable Ingreso")

muestras <- c()
for(i in 1:2000){
  tmp <- sample(datos$Income, size = 200)
  tmp <- as.data.table(tmp)
  tmp[, grupo := i] 
  l <- list(muestras, tmp)
  muestras <- rbindlist(l) 
}

distribucionmedias <- muestras[, mean(tmp), by = grupo]
setnames(distribucionmedias, "V1", "promedio")
muestrasmean <- mean(distribucionmedias$promedio)
d <- density(distribucionmedias$promedio, adjust = 1.8)
plot(d, main = "Densidad de la variable Ingreso(Muestra)")

par(mfrow = c(2,1), col.axis = "white", col.lab = "white", tck = 0, mai = c(0.3, 0.3, 0.3, 0.3))
for (i in c(1,10)){
  n <- 200*i
  dgrid <- density(distribucionmedias$promedio[1:n], adjust = 1.8)
  plot(dgrid, main = paste0("Histograma de promedio de ", n, " muestras"))
  polygon(dgrid, col="gray", border = "white")
  lines(dgrid, col ="red", lwd = 3)
}
