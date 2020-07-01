## importar data
deuda<-read.csv("D:/UIGV/UIGV VII/Econometria/SCRIPTS_ECONOMETRIA_III/BaseDatos_Capitulo_2/CAP2_MCO.csv",header=TRUE, sep=",")
View(pbi)
attach(pbi)

## analisis exploratorio

mean(y)
median (y)
sd(y)
var(y)
# o
summary(y)
# split.screen(c(1,2)) parte la pantalla en dos
split.screen(c(1,2))
hist(y)

# screen(2) coloca el siguiente resulatado en el segundo screen

screen(2)
boxplot(y)

# REPITIENDO PARA LAS OTRAS VARIABLES
#### x1
summary(x2,x3)

# split.screen(c(1,2)) parte la pantalla en dos
split.screen(c(1,2))
hist(x2)
# borra la memoria del screen
close.screen(all = TRUE) 

# screen(2) coloca el siguiente resulatado en el segundo screen

screen(2)
boxplot(x2)

#### x2
# borra la memoria del screen
close.screen(all = TRUE) 
# split.screen(c(1,2)) parte la pantalla en dos
split.screen(c(1,2))
hist(x2)
# screen(2) coloca el siguiente resultado en el segundo screen
screen(2)
boxplot(x2)

##Gráfica dispersion cruzada se suman mas variables pero cuantitativas###
cor(deuda)
pairs(~y+x2+x3)
#otro
scatter1<-plot(y~x2)
fit<-lm(y~x2)
abline(fit)
## ESTIMACION POR mco
# esta creando una matriz donde 1 es el intercepto, las xs var dependientes
X<-cbind(1,x2,x3)
y1<-cbind(y)
trX<-(t(X))
X_X<-trX %*% X
X_X
install.packages("mass")

# hallando el determinante de la matriz
library(MASS)

det(X_X)
invX_X<-(ginv(X_X))
invX_X

Xy<-trX %*% y1
Xy
beta<-invX_X %*% Xy
beta
# comprobando
modelo<-lm(y~x2+x3)
summary(modelo)
## ESTIMACION POR MRM

tiendas<-read.csv("D:/UIGV/UIGV VII/Econometria/SCRIPTS_ECONOMETRIA_III/BaseDatos_Capitulo_3/cap3_datoss.csv" , header=TRUE, sep=";" )
View(tiendas)
install.packages("tidyverse")
library(tidyverse)
library(paqueteadp)
data(bienestar_la)
