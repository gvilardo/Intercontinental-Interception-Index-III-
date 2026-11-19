# Article: Vilardo, G., Corley J.C., Liebhold A.M., Brockerhoff E.G., Turner R.M., Yamanaka T., Estay S.A., Lantschner V.M. 2026. Species traits shape the intercontinental introduction patterns of bark and ambrosia beetles.

#We fitted a generalized linear model (GLM) to analyze how the III varied among the intercepted species based on the number of biogeographic regions where the species occurs (including native and non-native regions), feeding habit, host breadth and host taxa.

library(tidyverse)
library("RColorBrewer")
require(doBy)
library(fitdistrplus)
library(car)

#data: Presion_propagulos_publicacion
datos<-as.data.frame(Presion_propagulos_publicacion)

?fitdist

# Response variable (III) is numeric and continues
#Response variable distribution 

unif=fitdist(datos$Prop_press,"unif")
beta=fitdist(datos$Prop_press,"beta")
norm=fitdist(datos$Prop_press,"norm")
lnorm=fitdist(datos$Prop_press,"lnorm")

# plot distrubutions
par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex=1)
cdf=cdfcomp(list(unif, beta, norm,lnorm),addlegend=T,main="",
            legendtext=c("unif","beta", "norm","lnorm"), plotstyle = "ggplot")
cdf

#lognormal has the best fit

#correlation between explaining variables
?vif
vif(m1)

ggplot(datos, aes(x = Feeding_Haack, fill = Host_taxa)) +
  geom_bar(position = "stack") +
  labs(
    x = "Feeding habit",
    y = "Number of species",
    fill = "Host taxa"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

ggplot(datos, aes(x = Feeding_Haack, fill = Host_breadth)) +
  geom_bar(position = "stack") +
  labs(
    x = "Feeding habit",
    y = "Number of species",
    fill = "Host breadth"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

#From these plots we explored overlapping between explaining variables. 

# VIF suggests no colinearity problems

#model

#exclude species with missing information and widespread
datos<-datos %>% filter(Native_reg != "widespread")

datos<-datos %>% filter(Feeding_Haack != "oth")

datos <- datos %>%
  mutate(across(where(is.character), ~ na_if(.x, "NA")))

m1=lm(log(Prop_press)~ Feeding_Haack+ Host_taxa+ Host_breadth + as.numeric(N_regions), na.action = na.exclude, data=datos)

summary(m1)

#The summary provides partial sums of squares (SS), that is, the effect of a given variable, 
#after all other variables have been included in the model. If p are not significant, it is because they do not explain anything that has not already been accounted for by the other variables.


anova(m1,test="Chisq")
#Anova provides sequential sums of squares in the order variables were include in the model

#Confidence intervals
confint(m1)
exp(confint(m1))

# coefs in the response variable scale
exp(coef(m1))

#Goodness of fit

summary(m1)$adj.r.squared

#Residual standar error
summary(m1)$sigma

#root mean squared error
rmse <- sqrt(mean(residuals(m1)^2, na.rm = TRUE))
rmse

#Residual analysis

pred_response=predict(m1, type="response")
residuos=resid(m1, type="pearson")

plot(pred_response,residuos)
abline(a=0, b=0, col="violet", lw=5)

qqnorm(resid(m1), ylab="Cuant. teoricos", xlab="Cuant Obs resid",main="Lognormal")
qqline(resid(m1),lwd=1.5, col="red")

# Extream residuals identification
which(abs(rstudent(m1)) > 3)

summary(m1)
1446/321
#There is still over dispersion


