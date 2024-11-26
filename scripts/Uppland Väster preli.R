rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(vegan)
library(MuMIn)
library(visreg)
library(pscl)
library(psych)

#Jag satte ihop datafilen ganska snabbt igår på båten (till Åland). 
#Det är alltså en åtgärdsyta per rad – de har olika ID-beteckning i Uppland och Västerbotten och jag har behållit dessa för att kunna spåra tillbaka till rådata och fältanteckningar men du kan ju ge nån ny beteckning om det behövs. 
#Min tanke är alltså att vi i en första snabbanalys kan kolla antal groddplantor (kolumn I) och antal totalt (kolumn M) mot faktorn antal utanför ytan totalt (kolumn AB) och år sedan åtgärd (kolumn D), kontrollerat för sträcka (kolumn B). 
#Hör av dig om du har frågor, bara inte nu på måndag då jag fortfarande är bortrest. 

# Create database without trees and with the new sites
raw <- read.csv2(here("Lupingrävningar Uppland Västerbotten", "Lupingrävning Uppland och Västerbotten Data för analys 2023-07-21.csv" ))

# Select relevant columns & drop empty rows (temp)
db <- raw %>% 
  select(Område, Sträcka.nr, ID.nr, År.sedan.åtgärd, Groddplantor, Alla, Inom.utanför.TRV) %>% 
  mutate(Område= as.factor(Område)) %>% 
  mutate(Sträcka.nr = as.factor(Sträcka.nr)) 
db

# Look at the data

dotchart(db$Groddplantor, xlab="Groddplantor", ylab="Observation")
100*sum(db$Groddplantor == 0)/nrow(db)
dotchart(db$Alla, xlab="Alla", ylab="Observation")
100*sum(db$Alla == 0)/nrow(db)
dotchart(db$Inom.utanför.TRV, xlab="Inom.utanför.TRV", ylab="Observation")
100*sum(db$Groddplantor == 0)/nrow(db)

# preliminary models: groddplantor ----
mod1 <- glmmTMB(Groddplantor ~ Område + År.sedan.åtgärd + Inom.utanför.TRV + (1|Sträcka.nr),  family = "poisson", data = db)
summary(mod1)
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1) # overdispersed
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") #
visreg(mod2, scale="response")


# try neg-binom
mod2 <- glmmTMB(Groddplantor ~ År.sedan.åtgärd + Inom.utanför.TRV + (1|Sträcka.nr), family = nbinom1, data = db)
summary(mod2)
mod_dharma1 <- mod2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") #seems to be underdispersed
visreg(mod2, scale="response")

# try zero-inflated poisson
mod3 <- glmmTMB(Groddplantor ~ scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr), zi=~Inom.utanför.TRV, family= "poisson", data = db)
summary(mod3)
mod_dharma1 <- mod3 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") #seems to be underdispersed
visreg(mod3, scale="response")

# error indicates collinearity, check:
pairs.panels(db[,-4], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE, 
             scale = TRUE # show correlation ellipses
)

pairs(db[,5:7], pch = 19)

# zero-inflated neg-binom
mod4 <- glmmTMB(Groddplantor ~ Område + scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr), zi=~Inom.utanför.TRV, family= nbinom1, data = db)
summary(mod4)
mod_dharma1 <- mod4 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
AICc(mod4)
visreg(mod4, scale="response")


# preliminary models: alla plantor ----
mod1 <- glmmTMB(Alla ~ Område + scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr),  family = "poisson", data = db)
summary(mod1)
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1) # overdispersed
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") #
visreg(mod2, scale="response")


# try neg-binom
mod2 <- glmmTMB(Alla ~  Område + scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr), family = nbinom1, data = db)
summary(mod2)
mod_dharma1 <- mod2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
#AICc utan område:

mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") 

# try adding obs as random effect to deal with overdispersion
db$obs <- seq(1:nrow(db))
db

mod3 <- glmmTMB(Alla ~  scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr/obs), family = nbinom1, data = db)
summary(mod3)
mod_dharma1 <- mod3 %>% simulateResiduals(n=1000)
plot(mod_dharma1)

plotResiduals(mod3, db$År.sedan.åtgärd, xlab = "År sedan åtgärd", main=NULL)
#plotResiduals(mod3, db$Område, xlab = "Område", main=NULL)
plotResiduals(mod3, db$Inom.utanför.TRV, xlab = "År sedan åtgärd", main=NULL)




# try zero-inflated poisson
mod4 <- glmmTMB(Alla ~ Område + scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr), zi=~Inom.utanför.TRV, family= "poisson", data = db)
summary(mod4)
mod_dharma1 <- mod4 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
mod_dharma1 %>% testDispersion()
mod_dharma1 %>% testZeroInflation() 
testDispersion(mod_dharma1, alternative = "less") #seems to be underdispersed
visreg(mod3, scale="response")

# try zer-inflated neg-binom
mod5 <- glmmTMB(Alla ~ Område + scale(År.sedan.åtgärd) + scale(Inom.utanför.TRV) + (1|Sträcka.nr), zi=~Inom.utanför.TRV, family= nbinom1, data = db)
summary(mod5)
mod_dharma1 <- mod5 %>% simulateResiduals(n=1000)
plot(mod_dharma1)

