## --------------------------------------------------------------------------------------------
#Lectura de les dades
dades<-read.table("dosis_hipertensio.txt",header=TRUE,sep="\t") 
#Definici� del factor.
dades$dosis <- factor(dades$dosis,labels=c("D1","D2","D3"))


## --------------------------------------------------------------------------------------------
summary(dades$dosis) #repliques per tractament
summary(dades$resp)  #descriptiva global

#descriptiva per nivells del factor
by(dades$resp, dades$dosis,summary)

#Desviaci� t�pica
by(dades$resp, dades$dosis,sd)




## --------------------------------------------------------------------------------------------
boxplot(resp ~ dosis,
        data=dades,main='Boxplot by dosis',col='blue')


## --------------------------------------------------------------------------------------------
p1.aov  <- aov(resp ~ dosis, dades) #Calcula la taula d'an�lisi de la vari�ncia
tablep1 <- anova(p1.aov)
tablep1


## --------------------------------------------------------------------------------------------
bartlett.test(resp~dosis,data=dades)


## --------------------------------------------------------------------------------------------
shapiro.test(p1.aov$residuals)
qqnorm(p1.aov$residuals,main='QQ-plot') 
qqline(p1.aov$residuals)


## --------------------------------------------------------------------------------------------
results.Tukey<-TukeyHSD(p1.aov) #Calcula els intervals de confian�a de les difer�ncies
#entre les mitjanes dels nivells del factor i els p valors ajustats de les comparacions dos a dos.

plot(results.Tukey) 

#El mateix per� amb la llibereria agricolae
#Aquest lliberia no est� instal.lada en el R, s'ha d'instal.lar amb la seg�ent comanda.

#install.packages("agricolae")  

library("agricolae")
#Aquesta funci� a difer�ncia de l'anterior ens dona m�s informaci� del test de HSD 
#Podem veure el valor cr�tic de la distribuci� del rang estudentitzat 
#i la difer�ncia m�nima significativa

HSD.test(p1.aov,"dosis",group=T,console=T) #console=T, argument perque ens mostri els resultats





## --------------------------------------------------------------------------------------------
mean(dades$resp)
mt <- model.tables(p1.aov,type = 'effects')
mt
var<-tablep1$`Mean Sq`[2]  #serveix nom�s per guardar el resultat

