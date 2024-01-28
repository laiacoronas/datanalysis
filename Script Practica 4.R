## --------------------------------------------------------------------------------------------
#Lectura de les dades
dades<-read.table("dosis_hipertensio.txt",header=TRUE,sep="\t") 
#Definició del factor.
dades$dosis <- factor(dades$dosis,labels=c("D1","D2","D3"))


## --------------------------------------------------------------------------------------------
summary(dades$dosis) #repliques per tractament
summary(dades$resp)  #descriptiva global

#descriptiva per nivells del factor
by(dades$resp, dades$dosis,summary)

#Desviació típica
by(dades$resp, dades$dosis,sd)




## --------------------------------------------------------------------------------------------
boxplot(resp ~ dosis,
        data=dades,main='Boxplot by dosis',col='blue')


## --------------------------------------------------------------------------------------------
p1.aov  <- aov(resp ~ dosis, dades) #Calcula la taula d'anàlisi de la variància
tablep1 <- anova(p1.aov)
tablep1


## --------------------------------------------------------------------------------------------
bartlett.test(resp~dosis,data=dades)


## --------------------------------------------------------------------------------------------
shapiro.test(p1.aov$residuals)
qqnorm(p1.aov$residuals,main='QQ-plot') 
qqline(p1.aov$residuals)


## --------------------------------------------------------------------------------------------
results.Tukey<-TukeyHSD(p1.aov) #Calcula els intervals de confiança de les diferències
#entre les mitjanes dels nivells del factor i els p valors ajustats de les comparacions dos a dos.

plot(results.Tukey) 

#El mateix però amb la llibereria agricolae
#Aquest lliberia no està instal.lada en el R, s'ha d'instal.lar amb la següent comanda.

#install.packages("agricolae")  

library("agricolae")
#Aquesta funció a diferència de l'anterior ens dona més informació del test de HSD 
#Podem veure el valor crític de la distribució del rang estudentitzat 
#i la diferència mínima significativa

HSD.test(p1.aov,"dosis",group=T,console=T) #console=T, argument perque ens mostri els resultats





## --------------------------------------------------------------------------------------------
mean(dades$resp)
mt <- model.tables(p1.aov,type = 'effects')
mt
var<-tablep1$`Mean Sq`[2]  #serveix només per guardar el resultat

