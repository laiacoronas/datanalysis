
###Definir directori de treball.
#Directori on està la base de dades i on volem guardar els resultats. 

#a. Utilitzar  comanda: 
# setwd(C://.....)

#b.Fer-ho per menú: 
#Session/Set working directory/Choose directory/.....#

## ------------------------------------------------------------------------
#Lectura de les dades
hipertensio<-read.table("tt_hta.txt",header=TRUE,sep="\t")

#Definició del factor.
hipertensio$drug <- factor(hipertensio$drug,labels=c("B","P"))


## ------------------------------------------------------------------------
summary(hipertensio$drug)
summary(hipertensio$resp)


## ------------------------------------------------------------------------
by(hipertensio$resp, hipertensio$drug,summary)

#Podem demanar la desviació típica
by(hipertensio$resp, hipertensio$drug,sd)

#O els percentils 20% i 80%
by(hipertensio$resp, hipertensio$drug,quantile,probs=c(0.20,0.80))



## ------------------------------------------------------------------------
boxplot(resp ~ drug, 
        hipertensio,main='Boxplot drugs',col='orange')


## ------------------------------------------------------------------------
shapiro.test(hipertensio$resp[hipertensio$drug=="B"]) 
shapiro.test(hipertensio$resp[hipertensio$drug=="P"]) 


## ------------------------------------------------------------------------
var.test(resp~drug,data=hipertensio)


## ------------------------------------------------------------------------
t.test(resp~drug,data=hipertensio,var.equal = FALSE)



