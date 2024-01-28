## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)



## ------------------------------------------------------------------------
dades<-read.table("colesterol2.txt",sep="\t",header=TRUE)
head(dades)
dades$hist_familiar<-factor(dades$hist_familiar,labels=c("No", "Si"))
levels(dades$hist_familiar) #La categoria de referència és la primera per tant, ja ens va bé. 
dades$imc_cat<-factor(dades$imc_cat, labels=c("IMC<30","IMC>=30"))
levels(dades$imc_cat)
dades$actividad_fisica<-factor(dades$actividad_fisica,labels=c("Baixa","Moderada","Vigorosa"))
levels(dades$actividad_fisica)


## ------------------------------------------------------------------------
correlacio<-cor.test(dades$colesterol, dades$edat)
correlacio
library(car)
scatterplot(colesterol~edat, data=dades, regLine=TRUE, smooth=FALSE)


## ------------------------------------------------------------------------
var.test(colesterol~hist_familiar,data=dades)
res.mit.fami<-t.test(colesterol~hist_familiar,data=dades,var.equal=TRUE)
res.mit.fami

var.test(colesterol~imc_cat,data=dades)
res.mit.imc<-t.test(colesterol~imc_cat,data=dades,var.equal=TRUE)
res.mit.imc


## ------------------------------------------------------------------------
by(dades$colesterol,dades$actividad_fisica,summary)
res.aov<-anova(aov(colesterol~actividad_fisica, data=dades))
res.aov
Fstat<-res.aov$`F value`[1]
pvalue<-res.aov$`Pr(>F)`[1]


## ------------------------------------------------------------------------
res.mult<-lm(colesterol~edat+imc_cat+hist_familiar+actividad_fisica,data=dades)
summary(res.mult)



## ------------------------------------------------------------------------
library(car)
Anova(res.mult,tipus=2)


## ------------------------------------------------------------------------
res.mult1<-lm(colesterol~edat+imc_cat+hist_familiar+actividad_fisica+edat*imc_cat,data=dades)
summary(res.mult1)


## ------------------------------------------------------------------------
plot(res.mult1)


## ------------------------------------------------------------------------
 shapiro.test(residuals(res.mult1))



## ------------------------------------------------------------------------
boxplot(rstandard(res.mult1))
plot(fitted(res.mult1), rstandard(res.mult1), xlab="Fitted Values", ylab="Standardized residuals")

