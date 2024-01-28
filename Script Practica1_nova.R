## ----setup, include=FALSE--------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(digits=7) 


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
y<-c(0,1,2,3,5,6,7,8,9,10,50)
mean(y)
#El nom dels arguments no fa falta escriure'ls si utilitzem el mateix ordre que estan 
#en la funció:
mean(y,trim=0.10) 
mean(y,0.10) #consell es escriure els nom dels arguments per recordar que era. 


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
3+4
3-4
3*4
3/4


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
x<-10
x

y<-2
y

x+y

x/y

pi


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
log(x) #natural log

log(x,2) #base 2 log

log10(x) #base 10 log


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
sin(2)
cos(2)
tan(2)


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
3^4
exp(2) #Power of number e
sqrt(2) #Square root
2^0.5


## ----echo=TRUE, message=TRUE, warning=TRUE,fig.width=10,fig.height=10------------------------------------------
x=sqrt(2)
x
options(digits=10) #Internal number of digits 
x
ceiling(x) #Upper integer rounding
floor(x) #Lower integer rounding[1] 
trunc(x) # Truncate
round(x,digits=3) #Rounding


## --------------------------------------------------------------------------------------------------------------
#Load data and create a  data.frame. 
nutricio=read.table("nutricio.txt",header=TRUE,sep="\t") 


## --------------------------------------------------------------------------------------------------------------
head(nutricio) #return the first parts of an Object (the six first rows)
tail(nutricio) #return the last part of an object (the six last rows)



## --------------------------------------------------------------------------------------------------------------
write.table(nutricio,file="nutricio_new.txt",sep="\t",row.names=F)

#save a file in format txt


## --------------------------------------------------------------------------------------------------------------
head(nutricio)
nutricio$Sexe
nutricio[2,1] #fist element indicate rows, second columns


## --------------------------------------------------------------------------------------------------------------
nuticio.dones<-subset(nutricio,Sexe==1) #Logical expression indicating rows to keep. 
head(nuticio.dones)
head(nutricio[nutricio$Sexe==1,]) #Select those rows where Sexe=1. The same as subset 
nuticio.consumEnergiasup2500<-subset(nutricio,Energia.cat=="Si") #Logical expression 
#when the variables is a labelled factor.
head(nuticio.consumEnergiasup2500)


## --------------------------------------------------------------------------------------------------------------
nutricio$imc.cat<-cut(nutricio$imc,breaks=c(0,18.5,25,31.25)) 

nutricio$imc.cat[1:10] #show the 10 first elements 

#Define intervals using min and max of the imc variable

nutricio$imc.cat=cut(nutricio$imc,breaks=c(min(nutricio$imc),18.5,25,max(nutricio$imc)))
nutricio$imc.cat


## --------------------------------------------------------------------------------------------------------------
nutricio[95,]  #visualitzar la fila 95 del data.frame nutricio

nutricio$imc.cat<-cut(nutricio$imc,breaks=c(min(nutricio$imc),18.5,25,max(nutricio$imc)),
                     include.lowest = TRUE)

nutricio[95,]

#labels for the levels of the resulting category
nutricio$imc.cat<-cut(nutricio$imc,breaks=c(min(nutricio$imc),18.5,25,max(nutricio$imc)),
                     include.lowest = TRUE,labels=c("Baix","Normal","Sobrepes"))

nutricio$imc.cat[1:10] #show the 10 first elements 


## --------------------------------------------------------------------------------------------------------------
is.factor(nutricio$Sexe)
nutricio$Sexe<-factor(nutricio$Sexe,labels=c("Home","Dona")) 
is.factor(nutricio$Sexe)
#utilitzem argument labels per etiqueta els valors.
nutricio$A.Fisica<-factor(nutricio$A.Fisica,labels=c("Baixa","Moderada","Alta"))
is.factor(nutricio$A.Fisica)


## --------------------------------------------------------------------------------------------------------------
is.factor(nutricio$Energia.cat)


## ----message=FALSE---------------------------------------------------------------------------------------------
#install.packages("mosaic") # En cas de no estar instal·lada
library(mosaic)  #Carregar  el paquet 


## --------------------------------------------------------------------------------------------------------------
tally(~ Sexe, data = nutricio)                            # freqüències absolutes
tally(~ Sexe, data = nutricio, format = "proportion")     # freqüències relatives (proporció)
tally(~ Sexe, data = nutricio, format = "percent")        # freqüències relatives (percentatge)



## --------------------------------------------------------------------------------------------------------------
summary(nutricio$Energia)


## --------------------------------------------------------------------------------------------------------------
getOption("digits")


## --------------------------------------------------------------------------------------------------------------
options(digits=8) #Nombre màxim de digits dels valor numèrics
getOption("digits")
summary(nutricio$Energia) #max(3,8-3)=5 digits

options(digits=9) #Nombre màxim de digits dels valor numèrics
summary(nutricio$Energia) #max(3,9-3)=6 digits


## --------------------------------------------------------------------------------------------------------------
min(nutricio$Energia)
mean(nutricio$Energia)
max(nutricio$Energia)


## --------------------------------------------------------------------------------------------------------------
sd(nutricio$Energia)


## --------------------------------------------------------------------------------------------------------------
var(nutricio$Energia)


## --------------------------------------------------------------------------------------------------------------
quantile(nutricio$Energia,probs = c(0.20,0.80))


## ----fig.width=5,fig.height=5----------------------------------------------------------------------------------
mytab<-table(nutricio$Sexe)
barplot(mytab,col="blue")
barplot(mytab,col="blue",ylab="Freq.Absoluta",xlab="Génere")
barplot(prop.table(mytab)*100,col="orange",ylab="Percentatge",xlab="Génere")


## ----fig.height=4, fig.width=4---------------------------------------------------------------------------------
install.packages("ggformula")
library(ggformula)
gf_bar(~ Sexe, data = nutricio)
gf_props(~ Sexe, data =nutricio)
gf_percents(~ Sexe, data = nutricio)



## ----fig.height=4, fig.width=4---------------------------------------------------------------------------------
hist(nutricio$Energia,xlab="Energia (kcal)")


## ----fig.height=4, fig.width=4---------------------------------------------------------------------------------
boxplot(nutricio$Energia)



## ----fig.height=4, fig.width=4---------------------------------------------------------------------------------
gf_histogram(~ Energia, data = nutricio)
gf_boxplot(~ Energia, data = nutricio) 
gf_boxplot(~ Energia, data = nutricio) + coord_flip()  #coord_flip(): invertir els eixos


## --------------------------------------------------------------------------------------------------------------
x=c(5,7,9,13,-4,8)  #numeric
x
y=c("A","B","C") #alfa-numeric (is a factor)
y


## --------------------------------------------------------------------------------------------------------------
1:5
seq(from=1, to=10, by=2)
rep(1,5)
z=c(1,2)
rep(z,5)
rep(z,each=5)


## --------------------------------------------------------------------------------------------------------------
x[2]
x[2:4]
x[c(1,3)]
x
x>8  #logical expression
x==8 #logical expression 


## --------------------------------------------------------------------------------------------------------------
2+x
2*x
x^2
y<-1:6
y
x
x+y
x*y


## --------------------------------------------------------------------------------------------------------------
x
sum(x)
x>8 #Logical condition
sum(x>8) #Only sum elements which are greater than 8 (satifies the condition)
prod(x)
length(x)


## --------------------------------------------------------------------------------------------------------------
z=c(rep("A",3),rep("B",3))
y<-c(rep(1,3),rep(2,3))
y

dades=data.frame(x,y,z)
dades
dades$x
dades[2,1] #fist element indicate rows, second columns

names(dades)

names(dades)=c("X","Y","Z") #names of the columns
dades

row.names(dades)


## --------------------------------------------------------------------------------------------------------------
Y<-c(rep(1,3),rep(2,3))
Y
is.factor(dades$Z)
is.factor(dades$Y)
is.factor(dades$X)

dades$Y
W=factor(dades$Y,labels=c("grup1","grup2"))
W


