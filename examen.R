#part 1

data <- vitaminaD
head(data)
options(digits=8)
summary(data$vitD)
by(data$vitD, data$municipi,summary)

p1.aov <- aov(vitD ~ municipi, data) 
tablep1 <- anova(p1.aov)
tablep1

shapiro.test(p1.aov$residuals)
bartlett.test(vitD~municipi,data=data)

library(nlme)
ex1.lme <- lme(vitD ~ 1, random = ~ 1 | municipi, data = data,method = "REML") 
varcomp <- VarCorr(ex1.lme)
varcomp


#part 2

data <- UCI
head(data)
data$sta <- factor(data$sta, labels = c("viu","mort"))
data$genere <- factor(data$genere, labels = c("dona","home"))
data$irc <- factor(data$irc, labels = c("no","si"))

mytab<-table(data$sta ,data$genere)
prop.table(mytab,1)*100
options(digits=8)
by(data$edat, data$sta,summary)

data$edat.cat <- ifelse(data$edat < 50, "jove","gran")
data$edat.cat <- factor(data$edat.cat, levels = c("jove","gran"), labels = c("jove","gran"))

mytab2<-table(data$edat.cat,data$sta) 
mytab2
res.chi<-chisq.test(mytab2) 
res.chi

res.glm1<-glm(formula = sta ~ edat.cat, family = binomial(logit), data = data) 
summary(res.glm1)
coef(res.glm1)
exp(coef(res.glm1))

mytab3<-table(data$irc,data$sta) 
mytab3
res.chi<-chisq.test(mytab3) 
res.chi

res.glm2<-glm(formula = sta ~ edat.cat+irc, family = binomial(logit), data = data) 
summary(res.glm2)
coef(res.glm2)
exp(coef(res.glm2))
exp(confint(res.glm2))

res.glm3<-glm(formula = sta ~ edat.cat+irc+edat.cat:irc, family = binomial(logit), data = data)
summary(res.glm3)
