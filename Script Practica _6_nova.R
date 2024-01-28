## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
#Lectura de les dades
dades<-read.table("tempsReaccio.txt",header=TRUE,sep="\t") 
head(dades)
#Definició del factor.
dades$estimul <- factor(dades$estimul,labels=c("Auditiu","LLuminos"))
dades$alcoholemia<-factor(dades$alcoholemia,labels=c("Alt","Nul"))


## ------------------------------------------------------------------------
#Descriptiva  en funció estímul
by(dades$TR,dades$estimul,summary)

#Descriptiva  en funció alcoholemia
by(dades$TR,dades$alcoholemia,summary)




## ------------------------------------------------------------------------
#Descriptiva en funció estimuls i alcoholemia

by(dades$TR,interaction(dades$alcoholemia,dades$estimul),summary)
by(dades$TR,interaction(dades$alcoholemia,dades$estimul),sd)


## ------------------------------------------------------------------------
interaction.plot(dades$estimul,dades$alcoholemia,dades$TR,col=c('black','blue'),xlab=c("Estimul")
                   ,ylab=c("Mean TR segons"),trace.label=c("Alcoholemia"))

boxplot(TR ~ alcoholemia*estimul,
        data=dades,main='Boxplot nivells alcholomia i estimul auditiu',col='orange')


## ------------------------------------------------------------------------
p1.aov  <- aov(TR ~ alcoholemia*estimul, dades) #Calcula la taula d'anàlisi de la variància
tablep1 <- anova(p1.aov)
tablep1


## ------------------------------------------------------------------------
bartlett.test(TR~interaction(alcoholemia,estimul),data=dades)


## ------------------------------------------------------------------------
shapiro.test(p1.aov$residuals)
qqnorm(p1.aov$residuals,main='QQ-plot') 
qqline(p1.aov$residuals)


## ------------------------------------------------------------------------
#Comparacions amb el mètode de Tukey.
#Efecte princial alcoholemia
TukeyHSD(p1.aov,which="alcoholemia") 
#Efecte principal estímul
TukeyHSD(p1.aov,which="estimul") 


## ------------------------------------------------------------------------
TukeyHSD(p1.aov,which="alcoholemia:estimul") 

