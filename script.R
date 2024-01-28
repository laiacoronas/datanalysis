library(readxl)

#PREPROCESSAT DE LES DADES

#carreguem les dades del campus
dades <- read_excel("/Users/laiacoronassala/Desktop/treball deiad/Matriu alumnes 2013-20-23 v4_DN.xlsx")
#agafem només aquelles variables que estudiarem
dades_netes = dades[c("kidmed", "qharo", "gpaqcat", "mstumoth", "livetogh", "sex", "weightdec", "heightdec", "CAR_DN" )]
#transformem sex en un factor, 1: home i 2: dona
dades_netes$sex <- factor(dades_netes$sex, labels = c("home", "dona"))
data <- dades_netes #per comoditat
#categoritzem adherència a la dieta medoterrània
intervals_kidmed <- c(-Inf, 4, 8, Inf)
etiquetes_kidmed <- c("pobre", "millorable", "òptima")
data$kidmed <- cut(data$kidmed, breaks = intervals_kidmed, labels = etiquetes_kidmed, include.lowest = TRUE, right = FALSE)
#convertim l'activitat física en un factor, ja està categoritzada
data$gpaqcat <- factor(data$gpaqcat, labels = c("poca","moderada","vigorosa"))
#estudis màxims de la mare a categòrica
intervals_mstumoth <- c(-Inf, 4, 6, Inf)
etiquetes_mstumoth <- c("sense estudis", "superior", "universitaris")
data$mstumoth <- cut(data$mstumoth, breaks = intervals_mstumoth, labels = etiquetes_mstumoth, include.lowest = TRUE, right = FALSE)
#recodifiquem la variable convivència
data$livetogh <- ifelse(
  data$livetogh >= 6 & data$livetogh <= 11 | data$livetogh == 13, "Viu amb pares/avis",
  ifelse(data$livetogh >= 1 & data$livetogh <= 5 | data$livetogh == 12 | data$livetogh >= 14 & data$livetogh <= 17, "No viu amb pares/avis", NA)
)
levels_desired <- c("Viu amb pares/avis", "No viu amb pares/avis")
labels_desired <- c("Viu amb pares/avis", "No viu amb pares/avis")
data$livetogh <- factor(data$livetogh, levels = levels_desired, labels = labels_desired)

#creem una columna que sigui IMC seguint la fórmula i categoritzem en tres classes diferents
data$IMC <- (data$weightdec)/((data$heightdec*0.01)^2)
data$IMC_cat <- ifelse(
  data$IMC < 18, "Baix pes",
  ifelse(data$IMC >= 18 & data$IMC < 25, "Normo pes", 
         ifelse(data$IMC >= 25, "Obesitat", NA))
)
levels_desired <- c("Baix pes", "Normo pes", "Obesitat")
labels_desired <- c("Baix pes", "Normo pes", "Obesitat")
data$IMC_cat <- factor(data$IMC_cat, levels = levels_desired, labels = labels_desired)

# ANÀLISIS UNIVARIANT DE LES VARIABLES

#de les categòriques

kidmed <- summary(data$kidmed)
kidmed
prop.table(kidmed)*100

gpa <- summary(data$gpaqcat)
gpa
prop.table(gpa)*100

sex <- summary(data$sex)
sex
prop.table(sex)*100

imc <- summary(data$IMC_cat)
imc
prop.table(imc)*100

convivencia <- summary(data$livetogh)
convivencia
prop.table(convivencia)*100

estudis <- summary(data$mstumoth)
estudis
prop.table(estudis)*100

# de les quantitatives

risc_obesitat <- sum(!is.na(data$qharo))
risc_obesitat
mean(data$qharo)
sd(data$qharo)

caro <- sum(!is.na(data$CAR_DN))
caro
mean(data$CAR_DN)
sd(data$CAR_DN)

# ANÀLISIS BIVARIANT DE LES VARIABLES INDEPENDENTS AMB LA DEPENDENT

#qualitatiu dos nivells

var.test(CAR_DN~sex,data=data)
shapiro.test(data$CAR_DN[data$sex=="home"])
shapiro.test(data$CAR_DN[data$sex=="dona"]) 
#com que tenim normalitat i homogeneïtat de variànces fem un t-test
t.test(CAR_DN~sex,data=data,var.equal = TRUE)

wilcox.test(CAR_DN ~ sex, data = data)

var.test(CAR_DN~livetogh,data=data)
shapiro.test(data$CAR_DN[data$livetogh=="Viu amb pares/avis"])
shapiro.test(data$CAR_DN[data$livetogh=="No viu amb pares/avis"]) 
t.test(CAR_DN~livetogh,data=data,var.equal = TRUE)
wilcox.test(CAR_DN ~ livetogh, data = data)
#qualitatiu de tres nivells

p1.aov <- aov(CAR_DN ~ kidmed, data)
tablep1 <- anova(p1.aov)
tablep1
model.tables(p1.aov,type = 'means',cterms='kidmed')
library(agricolae)
HSD.test(p1.aov, "kidmed",console=T)
TukeyHSD(p1.aov,which="kidmed")
residuos <- residuals(tablep1)
shapiro.test(residuals(p1.aov))
kruskal.test(CAR_DN ~ kidmed, data = data)

p2.aov <- aov(CAR_DN ~ gpaqcat, data)
tablep2 <- anova(p2.aov)
tablep2
model.tables(p2.aov,type = 'means',cterms='gpaqcat')
library(agricolae)
HSD.test(p2.aov, "gpaqcat",console=T)
TukeyHSD(p2.aov,which="gpaqcat")
shapiro.test(residuals(p2.aov))
kruskal.test(CAR_DN ~ gpaqcat, data = data)

p3.aov <- aov(CAR_DN ~ IMC_cat, data)
tablep3 <- anova(p3.aov)
tablep3
model.tables(p3.aov,type = 'means',cterms='IMC_cat')
library(agricolae)
HSD.test(p3.aov, "IMC_cat",console=T)
TukeyHSD(p3.aov,which="IMC_cat")
shapiro.test(residuals(p3.aov))
p4.aov <- aov(CAR_DN ~ mstumoth, data)
tablep4 <- anova(p4.aov)
tablep4
kruskal.test(CAR_DN ~ IMC_cat, data = data)

model.tables(p4.aov,type = 'means',cterms='mstumoth')
library(agricolae)
HSD.test(p4.aov, "mstumoth",console=T)
TukeyHSD(p4.aov,which="mstumoth")
shapiro.test(residuals(p4.aov))
kruskal.test(CAR_DN ~ mstumoth, data = data)
#model de regressió lineal simple (variable quantitativa)

cor.test(data$CAR_DN,data$qharo)


library(ggplot2)

# Crea un boxplot para la variable dependiente con color azul claro
ggplot(data, aes(x = 1, y = CAR_DN)) +
  geom_boxplot(fill = "lightblue") +  # Color azul claro
  labs(, y = "Carotenoides") +
  theme_minimal()

