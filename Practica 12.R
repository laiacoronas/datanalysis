## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)



## ------------------------------------------------------------------------
dades<-read.table("cancer.txt",header=TRUE, sep="\t")
head(dades)
dades$cancer<-factor(dades$cancer,labels=c("Control", "Cas"))
dades$dieta<-factor(dades$dieta,labels=c("Pobra","Rica"))
dades$pred.Disp<-factor(dades$pred.Disp,labels=c("No Pred.Disp","Pred.Disp"))


## ------------------------------------------------------------------------
mytab<-table(dades$pred.Disp,dades$cancer)
mytab
prop.table(mytab,margin=1)  #margin per indicar si volem els marginals per files(margin=1) o columnes (margin=2) 


## ------------------------------------------------------------------------
res.chi<-chisq.test(mytab)
res.chi
#printar els valors esperats, per veures si es 
#compleixen les condicions aplicació del test
res.chi$expected
#calcular l'odds ratio.
fisher.test(mytab)


## ------------------------------------------------------------------------
res.glm1<-glm(formula = cancer ~ pred.Disp, family = binomial(logit), data = dades)
summary(res.glm1)


## ------------------------------------------------------------------------

#coeficients de regressió i interval de confiança
coef(res.glm1)
confint(res.glm1)
#odds ratio i el seu interval de confiança
exp(coef(res.glm1))
exp(confint(res.glm1))


## ------------------------------------------------------------------------
mytab.2<-table(dades$dieta,dades$cancer)
mytab.2
prop.table(mytab.2,margin=1)  #margin per indicar si volem els marginals per files(margin=1) o columnes (margin=2) 


## ------------------------------------------------------------------------
res.chi.2<-chisq.test(mytab.2)
res.chi.2
#printar els valors esperats
res.chi.2$expected
#calcular l'odds ratio.
fisher.test(mytab.2)


## ------------------------------------------------------------------------
res.glm2<-glm(formula = cancer ~ dieta, family = binomial(logit), data = dades)
summary(res.glm2)


## ------------------------------------------------------------------------
exp(coef(res.glm2))
exp(confint(res.glm2))


## ------------------------------------------------------------------------
res.glm3<- glm(cancer ~ pred.Disp +dieta, family=binomial(logit), data=dades)
summary(res.glm3)


## ------------------------------------------------------------------------
exp(coef(res.glm3))
exp(confint(res.glm3))


## ------------------------------------------------------------------------
res.glm4<- glm(cancer ~ pred.Disp +dieta +pred.Disp:dieta, family=binomial(logit), data=dades)
summary(res.glm4)

