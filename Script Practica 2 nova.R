## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------------
#Carregar les dades.
nutricio=read.table("nutricio_P2.txt",header=T,sep="\t") 
#read.table() reads a file in table format
#header=TRUE file contains the names of the variables
#sep=the field separator character. "\t" separetor is the tabulator. 
#En el fitxer de dades, no fa falta possa tot el path de l'ubicació del fitxer 
#per què el va a buscar el en directori de treball. 

head(nutricio) #return the firts parts of an Object (the six first rows)



## -----------------------------------------------------------------------------------------------
is.factor(nutricio$Sexe)
nutricio$Sexe<-factor(nutricio$Sexe,labels=c("Home","Dona")) 
is.factor(nutricio$Sexe)

#utilitzem argument labels per etiqueta els valors.
nutricio$A.Fisica<-factor(nutricio$A.Fisica,labels=c("Baixa","Moderada","Alta"))
is.factor(nutricio$A.Fisica)


## -----------------------------------------------------------------------------------------------

nutricio$imc<-nutricio$Pes/(nutricio$Talla^2)
head(nutricio)

nutricio$imc<-round(nutricio$imc,digits=1)


## -----------------------------------------------------------------------------------------------

nutricio$Energia.cat <- ifelse(nutricio$Energia < 2500, "Inf_2500kcal", "SupIg_2500kcal")    
head(nutricio)



## -----------------------------------------------------------------------------------------------
nutricio$a <- ifelse(nutricio$A.Fisica == "Baixa" | nutricio$imc >25, 1, 0)
#head(nutricio)

nutricio$b <- ifelse(nutricio$Sexe == "Dona" & nutricio$A.Fisica == "Alta" , 1, 0)
#head(nutricio)


## -----------------------------------------------------------------------------------------------
mytab<-table(nutricio$A.Fisica ,nutricio$Sexe) 
#Files : primer argument (A.Fisica),   Columnes: segon argument (Sexe)
mytab

#Percentatge per fila. Utilitzar la funció prop.table.
#en la funció prop.table definir argument margin
#margin=1, percentatge per fila
#margin=2 percentatge per columnes
#margin=NULL percentatge respecte el total. Opció per defecte

prop.table(mytab,1)*100  #en la funció prop.table definir argument margin
round(prop.table(mytab,1)*100,digits=2)

#Percentatge per columnes Utilitzar la funció prop.table.
prop.table(mytab,2)*100
round(prop.table(mytab,2)*100,digits=2)

#Percentatge respecte el total d'individus.
prop.table(mytab)*100
round(prop.table(mytab)*100,digits=2)




## -----------------------------------------------------------------------------------------------
library(mosaic)  #Carregar el paquet mosaic
#Freqüències Absolutes
tally(A.Fisica ~ Sexe,data=nutricio)  
# freqüències relatives (proporció) respecte la variable que hem posat després de la tilda)
tally(A.Fisica ~ Sexe,data=nutricio,format = "proportion")  
# freqüències relatives (percentatge)  respecte la variable que hem posat després de la tilda)
tally(A.Fisica ~ Sexe,data=nutricio,format = "percent")     


## -----------------------------------------------------------------------------------------------
by(nutricio$Energia, nutricio$Sexe,summary)


## -----------------------------------------------------------------------------------------------
getOption("digits")


## -----------------------------------------------------------------------------------------------
options(digits=8) #Nombre màxim de digits dels valor numèrics
getOption("digits")
by(nutricio$Energia, nutricio$Sexe,summary)

options(digits=9) #Nombre màxim de digits dels valor numèrics
by(nutricio$Energia, nutricio$Sexe,summary)


## -----------------------------------------------------------------------------------------------
by(nutricio$Energia, nutricio$Sexe,mean)
by(nutricio$Energia, nutricio$Sexe,var)



## ----fig.width=6,fig.height=6-------------------------------------------------------------------
boxplot(Energia~ Sexe,data=nutricio,col = "lightgray")
boxplot(Energia~ Sexe,data=nutricio,col = c("lightgray","blue"))


## -----------------------------------------------------------------------------------------------
gf_boxplot(Energia~ Sexe,data=nutricio)


## ----fig.width=6,fig.height=6-------------------------------------------------------------------
hist(nutricio[nutricio$Sexe=="Dona","Energia"],main="Consum Energia Dones")
hist(nutricio[nutricio$Sexe=="Home","Energia"],main="Consum Energia Homes")


## -----------------------------------------------------------------------------------------------
par(mfcol = c(1, 2))
hist(nutricio[nutricio$Sexe=="Dona","Energia"],main="Consum Energia Dones",xlab="Energia")
hist(nutricio[nutricio$Sexe=="Home","Energia"],main="Consum Energia Homes",xlab="Energia")


## -----------------------------------------------------------------------------------------------
gf_histogram(~Energia|Sexe,data=nutricio)

