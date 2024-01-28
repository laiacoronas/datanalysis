## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# RESIDUAL ES INTRAGRUPS I TECNIC INTERGRUP


## --------------------------------------------------------------------------------
#Lectura de les dades
#dades<-read.table("adenovirus.txt",header=TRUE,sep="\t") 
dades <- adenovirus
head(dades)
#Definició del factor.
dades$tecnic <- factor(dades$tecnic) #No fa falta assignar etiquetes. 


## --------------------------------------------------------------------------------
options(digits=8) 


## --------------------------------------------------------------------------------
summary(dades$tecnic) #repliques per tècnic
summary(dades$resp)  #descriptiva global




## --------------------------------------------------------------------------------
boxplot(resp ~ tecnic,
        data=dades,main='Boxplot by Technician',col='blue')


## --------------------------------------------------------------------------------
p1.aov  <- aov(resp ~ tecnic, dades) #Calcula la taula d'anàlisi de la variància
tablep1 <- anova(p1.aov)
tablep1


## --------------------------------------------------------------------------------
bartlett.test(resp~tecnic,data=dades)


## --------------------------------------------------------------------------------
shapiro.test(p1.aov$residuals)
qqnorm(p1.aov$residuals,main='QQ-plot') 
qqline(p1.aov$residuals)


## --------------------------------------------------------------------------------
#A partir de la taula ANOVA.

#Components de la variancia
taulaAnova <- anova(p1.aov)
MSE <-  taulaAnova[2,3] #Guardo la mitjana quadràtica intragrups, 
#que és l'estimació de la variància dels errors
#Estimació  de la variància de l'efecte aleatori tècnic
sigmaA <- (taulaAnova[1,3]-MSE)/4


## --------------------------------------------------------------------------------
#install.packages("nlme") # En cas de no tenir-la instal.lada
library(nlme) #Carrega la lliberia. 
ex1.lme <- lme(resp ~ 1, random = ~ 1 | tecnic, data = dades,method = "REML")
varcomp   <- VarCorr(ex1.lme)  #VarCorr: extreu les components de la variància
#directament
varcomp


## --------------------------------------------------------------------------------
p.tecnics<-round(sigmaA/(sigmaA+MSE)*100,4)
paste('Technician variance component',p.tecnics,'%')
