rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(vegan)
library(MuMIn)
library(visreg)


# Create database without trees and with the new sites
älgnäs <- read.csv2(here("data", "älgnäs lupin datafil 2022-2023.csv" ))

# Select relevant columns & drop empty rows (temp)
db <- älgnäs %>% 
  select(Year, ID, ID2, Langd, Bredd.innerslant, Bredd.ytterslant, Djup.cm, 
         Inom.Med.hjartblad.2022, Inom.Antal.Stora.2022, Inom.antal.Sma.2022, 
         Inom.Med.hjartblad, Inom.Antal.Stora, Inom.antal.Sma, Utanfor.langdriktning.Med.hjartblad, 
         Utanfor.langdriktning.antal.Stora, Utanfor.langdriktning.antal.Sma, Utanfor.i.bredd.Med.hjartblad, 
         Utanfor.i.bredd.antal.Stora, Utanfor.i.bredd.Små, Utanfor.TrVs.omrade.Med.hjartblad, 
         Utanfor.TrVs.omrade.antal.Stora, Utanfor.TrVs.omrade.antal.Sma) %>% 
  mutate(Inom.Antal.Stora.2022= as.integer(Inom.Antal.Stora.2022)) %>% 
  mutate(Inom.antal.Sma.2022 = as.integer(Inom.antal.Sma.2022)) %>% 
  mutate(Djup.cm = as.factor(Djup.cm)) %>% 
  drop_na(Langd) %>% 
  drop_na(Inom.Antal.Stora.2022) %>% 
  drop_na(Inom.antal.Sma.2022)
db

# Look at the data
# 2022
summary(db$Inom.Med.hjartblad.2022)
summary(db$Inom.Antal.Stora.2022)
hist(db$Inom.Antal.Stora.2022)
summary(db$Inom.antal.Sma.2022)
hist(db$Inom.antal.Sma.2022)
# 2023 inom försöksytan
summary(db$Inom.Med.hjartblad)
hist(db$Inom.Med.hjartblad)
summary(db$Inom.Antal.Stora) 
summary(db$Inom.antal.Sma)
hist(db$Inom.antal.Sma)

# 2023 utanför försöksytan
summary(db$Utanfor.langdriktning.antal.Stora)
hist(db$Utanfor.langdriktning.antal.Stora)


# preliminary models
mod1 <- glmmTMB(Inom.Med.hjartblad ~ Inom.Antal.Stora.2022 + Inom.antal.Sma.2022 + Djup.cm +
                           Utanfor.TrVs.omrade.antal.Stora, family = "poisson", data = db)
summary(mod1)
mod_dharma1 <- mod1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mod1, scale="response")

# try neg-bin
mod2 <- glmmTMB(Inom.Med.hjartblad ~ Inom.Antal.Stora.2022 + Inom.antal.Sma.2022 + Djup.cm +
                  Utanfor.TrVs.omrade.antal.Stora, family = nbinom1, data = db)
summary(mod2)
mod_dharma1 <- mod2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mod2, scale="response")


mod2<- glmmTMB(Inom.antal.Sma ~ Inom.Antal.Stora.2022 + Inom.antal.Sma.2022 + Djup.cm +
                 Utanfor.TrVs.omrade.antal.Stora, family = "poisson", data = db)
summary(mod2)
visreg(mod2, scale="response")

# har 30 cm ytor bestånd utanför? kolla andelen ytor med bestånd utanför
# zero-inflated negative binomial?
# add plot-id as random effect to deal with overdispersion?

mod2 <- glmmTMB(Inom.antal.Sma ~ Inom.Antal.Stora.2022 + Inom.antal.Sma.2022 + Djup.cm +
                  Utanfor.TrVs.omrade.antal.Stora, family = nbinom1, data = db)
