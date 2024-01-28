## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------
#Lectura de les dades
dades<-read.table("etanol.txt",header=TRUE,sep="\t",dec=",")

head(dades)
#Definició del factor.
dades$tractament <- factor(dades$tractament,labels=c("Sol.Salina","Etanol 1g/kg","Etanol 2g/kg"))
dades$camada <- factor(dades$camada)


## ------------------------------------------------------------------------

summary(dades$camada)

#Descriptiva  en funció estímul
by(dades$vol,dades$tractament,summary)
boxplot(vol ~ tractament,data=dades)

#Descriptiva  en funció camada
by(dades$vol,dades$camada,summary)




## ------------------------------------------------------------------------
p1.aov  <- aov(vol ~ tractament+camada, dades) #Calcula la taula d'anàlisi de la variància
tablep1 <- anova(p1.aov)
tablep1


## ------------------------------------------------------------------------
p2.aov  <- aov(vol ~ tractament, dades) #Calcula la taula d'anàlisi de la variància
tablep2 <- anova(p2.aov)
tablep2


## ------------------------------------------------------------------------
bartlett.test(vol~tractament,data=dades)


## ------------------------------------------------------------------------
shapiro.test(p1.aov$residuals)
qqnorm(p1.aov$residuals,main='QQ-plot') 
qqline(p1.aov$residuals)


## ------------------------------------------------------------------------
#install.packages("nlme") # En cas de no tenir-la instal.lada
library(nlme) #Carrega la lliberia. 
ex1.lme <- lme(vol ~ tractament, random = ~ 1 | camada, data = dades,method = "REML")
varcomp   <- VarCorr(ex1.lme)  #VarCorr: extreu les components de la variància
#directament
 varcomp


## ------------------------------------------------------------------------
v.residual<-as.numeric(varcomp[2,1])
var.camada<-as.numeric(varcomp[1,1])
#Per tant, el percentatge de la variabilitat provocat per la camada.

p.camada<-round(var.camada/(var.camada+v.residual)*100,4)
paste('Percentatge de la variabilitat degut a camada',p.camada,'%')


## ------------------------------------------------------------------------
intervals(ex1.lme)[[2]]


## ------------------------------------------------------------------------
mean(dades$vol)
model.tables(p1.aov,type = 'effects',cterms='tractament')


## ------------------------------------------------------------------------
model.tables(p1.aov,type = 'means',cterms='tractament')


## ------------------------------------------------------------------------
library(agricolae)
HSD.test(p1.aov, "tractament",console=T)
TukeyHSD(p1.aov,which="tractament")

