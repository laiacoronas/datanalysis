## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------------------
#Lectura de les dades
dades<-read.table("serotonina2.txt",header=TRUE,sep="\t") 
#Definició de factors.
dades$subject <- factor(dades$subject) #No fa falta assignar etiquetes. 
dades$stimulus <-factor(dades$stimulus,labels=c("Estressant","Relaxant"))
dades$stimulus <-factor(dades$stimulus,levels=c("Relaxant","Estressant"))


## -----------------------------------------------------------------------------------------
#En R, el símbol != indica diferent 
dades.relaxant.0.15<-subset(dades,stimulus=="Relaxant"& time!=30) 
#Només els subjectes que han rebut estímul relaxant
#i descartem els que temps igual a 30 minuts
dades.estressant.0.15<-subset(dades,stimulus=="Estressant"& time!=30 ) 
#Només els subjectes que han rebut estímul extressant. 
#i descartem els que temps igual a 30 minuts

#Comprovació. En la base de dades relaxant, si fem taula descriptiva  de la variable estímul.
#Tots haurien de ser Relaxant
table(dades.relaxant.0.15$stimulus)
#i en la mateixa base de dades si dem taula descriptiva  de la variable time 
#tots haurien de ser del temps 0 minuts o 15 minuts
table(dades.estressant.0.15$time)
#igual pel grup extressant 
table(dades.estressant.0.15$stimulus)
table(dades.estressant.0.15$time)
#canvio el nombre de digits per obtenir més decimals amb la comanda by. 
options(digits=8) 



## -----------------------------------------------------------------------------------------
#Descritpiva 
by(dades.relaxant.0.15$serotn,dades.relaxant.0.15$time,summary)
by(dades.estressant.0.15$serotn,dades.estressant.0.15$time,summary)

t.test(serotn~time,data=dades.relaxant.0.15,paired=TRUE)  
#les dades de serotonina han d'estar ordenades per individu en cada temps. És a dir, 
#en el moment 0 time, dades ordenades per individus 1,2,3,4,5,6,7....
#en el moment 15 time, dades ordenades per individus 1,2,3,4,5,6,7.....
head(dades.relaxant.0.15)
t.test(serotn~time,data=dades.estressant.0.15,paired=TRUE)


## -----------------------------------------------------------------------------------------
dades.estressant<-subset(dades,stimulus=="Estressant")
dades.relaxant<-subset(dades,stimulus=="Relaxant")


## -----------------------------------------------------------------------------------------
by(dades.estressant$serotn,dades.estressant$time,summary)
by(dades.relaxant$serotn,dades.relaxant$time,summary)


## -----------------------------------------------------------------------------------------
interaction.plot(dades$time,dades$stimulus,dades$serotn,col=c('black','blue'),xlab=c("Temps en minuts")
                   ,ylab=c("Mean Serotonina"),trace.label=c("Estímul"))



## -----------------------------------------------------------------------------------------
#En el grup d'individus relaxant
p1.lm<-lm(serotn ~time,data=dades.relaxant)
summary(p1.lm)

#En el grup d'individus estressant
p2.lm<-lm(serotn ~time,data=dades.estressant)
summary(p2.lm)


## -----------------------------------------------------------------------------------------
p3.lm<-lm(serotn~time*stimulus,data=dades)
summary(p3.lm)


## -----------------------------------------------------------------------------------------
#representació de serotonina en funció del temps per cada individu(group), separat per estímul. 
#El temps el poso com a factor perquè en el gràfic només surtin les etiquetes dels temps observats.
library(lattice)
par(mfrow=c(2, 1))
xyplot(serotn ~ as.factor(time)|stimulus, group = subject, data = dades, type = "b",xlab="Temps", ylab="Serotonina")



## -----------------------------------------------------------------------------------------
library(nlme)
fm.plm <- lmList(serotn ~ time | subject, dades)
plot(intervals(fm.plm))



## -----------------------------------------------------------------------------------------
library(nlme)
lme_fit_1 <- lme(serotn~time*stimulus,random=~1|subject,data=dades)
VarCorr(lme_fit_1)
varcomp   <- VarCorr(lme_fit_1)  #VarCorr: extreu les components de la variància


## -----------------------------------------------------------------------------------------
var.residual<-as.numeric(varcomp[2,1])
var.intercept<-as.numeric(varcomp[1,1])

correlacio<-var.intercept/(var.intercept+var.residual)
#Correlació entre les mesures del mateix individu. 
paste('El coeficient de correlació intraclasse és igual a ',correlacio)


## -----------------------------------------------------------------------------------------
round(summary(lme_fit_1)$tTable,digits=4)

