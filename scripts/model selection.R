rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(DHARMa)
library(vegan)
library(MuMIn)
library(visreg)
library(effects)
library(ggpubr)
library(car)
library(interactions)
library(emmeans)
#library(MASS)
#library(ggeffects)
#library(effects)

# Create database without trees and with the new sites
raw <- read.csv2(here("data", "raw data clean.csv" ))

# Select relevant columns and prepare dataset
db <- raw %>% 
  dplyr::select(ID., ID, Point.digging, Area, Digging.depth, Uneven.digging.depth, Seedlings.2023, Big.Small.2023,
         Total.outside.2023, Big.small.2022, Surroundings, Orientation, Djup.cm) %>% 
  mutate(ID = as.factor(ID)) %>% 
  mutate(Point.digging = as.factor(Point.digging)) %>% 
  mutate(Uneven.digging.depth = as.factor(Uneven.digging.depth)) %>% 
  mutate(Surroundings = as.factor(Surroundings)) %>% 
  mutate(Orientation = as.factor(Orientation)) %>% 
  mutate(Djup.cm = as.factor(Djup.cm)) %>% 
  mutate(seed.bin = ifelse(Seedlings.2023>0, 1,0))
str(db)

area.plot <- ggplot(db, aes(x=fct_inorder(ID), y=Area, color= Point.digging)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank()) 
area.plot

dotchart(db$Area)


# Analysis groddplantor as binomial (finns= 1, ej finns=0)
mod.groddplantor <- glmmTMB(seed.bin ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                            family = binomial, data = db)
summary(mod.groddplantor)
AICc(mod.groddplantor) 
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantor))

# remove those extreme values
db.NOext <- db %>% 
  filter(!ID %in% c("5", "30A", "30C", "30B"))
db.NOext

mod.groddplantorx <- glmmTMB(seed.bin ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                            family = binomial, data = db.NOext)
summary(mod.groddplantorx)
AICc(mod.groddplantorx) 
mod_dharma1 <- mod.groddplantorx %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantorx))

# Analys för bara ytor med groddplantor:
db.seedlings <- db %>% 
  filter(Seedlings.2023>0)
db.seedlings 

mod.groddplantor2 <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                             family = nbinom2, data = db.seedlings)
summary(mod.groddplantor2)
AICc(mod.groddplantor2) 
mod_dharma1 <- mod.groddplantor2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantor2))

# Ta de extrema värden bort
# plot it
dotchart(db.seedlings$Seedlings.2023)
ggplot(db.seedlings, aes(x=log10(Seedlings.2023), y=Total.outside.2023, color= Area, label=ID)) + geom_point() +
  geom_text(hjust=0, vjust=0)

db.seedlingsINCOM <- db.seedlings %>% 
  filter(!ID %in% c("5", "30A", "30C", "30B"))
db.seedlingsINCOM

mod.groddplantor3 <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                             family = nbinom2, data = db.seedlingsINCOM)
summary(mod.groddplantor3)
AICc(mod.groddplantor3) 
mod_dharma1 <- mod.groddplantor3 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantor3))


# Orginal analysis
mod.groddplantorOR <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                             family = nbinom2, data = db)
summary(mod.groddplantorOR)
AIC(mod.groddplantorOR) 
mod_dharma1 <- mod.groddplantorOR %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantorOR))

# Orginal analysis without the "outliers"

mod.groddplantorOR2 <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                              family = nbinom2, data = db.NOext)
summary(mod.groddplantorOR2)
AIC(mod.groddplantorOR2) 
mod_dharma1 <- mod.groddplantorOR2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantorOR2))

# OLD: -----


# Analys för stora ytor
db.ytor <- db %>% 
  filter(Point.digging == "No")
db.ytor

mod.groddplantorNB <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area, 
                            family = nbinom1, data = db)
summary(mod.groddplantorNB)
AIC(mod.groddplantorNB) 
mod_dharma1 <- mod.groddplantorNB %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantorNB))

# With all areas and area as an explanatory variable
mod.groddplantorA <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area, 
                            family = nbinom2, data = db)

summary(mod.groddplantorA)
AIC(mod.groddplantorA)
mod_dharma1 <- mod.groddplantorA %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantorA))

#Same model but with lme4 cause of vif
# activate MASS package : gives very different results compared to glmmTMB
mod.groddplantor.0 <- glm.nb(Seedlings.2023 ~ Big.small.2022 + Digging.depth + Total.outside.2023, 
                              data = db.ytor)
summary(mod.groddplantor.0)
vif(mod.groddplantor)

# test correlation
cor.test(db.ytor$Total.outside.2023, db.ytor$Big.small.2022, method = c("pearson"))
cor.vars <- ggplot(db.ytor, aes(x=log10(Total.outside.2023+1), y=log(Big.small.2022+1), color=Digging.depth)) + geom_point()
cor.vars


# Since Big.small.2022 and Total.outside.2023 are correlated,need to choose one of them to have in the model. Using AIC
mod.groddplantor2 <- glmmTMB(Seedlings.2023 ~ Digging.depth + Big.small.2022,
                            family = nbinom2, data = db.ytor)
summary(mod.groddplantor2)
AIC(mod.groddplantor2) # 433.5273
mod_dharma1 <- mod.groddplantor2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1) # zero inflated
ggplot(db.ytor, aes(x=Big.small.2022, y=log10(Seedlings.2023+1), color= Digging.depth)) + geom_point()


mod.groddplantor3 <- glmmTMB(Seedlings.2023 ~ Digging.depth + Total.outside.2023,
                             family = nbinom2, data = db.ytor)
summary(mod.groddplantor3)
AIC(mod.groddplantor3) # 400.7656
mod_dharma1 <- mod.groddplantor3 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1) # not zero inflated
ggplot(db.ytor, aes(x=Total.outside.2023, y=log10(Seedlings.2023+1), color= Digging.depth)) + geom_point()

# plot
plot(allEffects(mod.groddplantor3))


mod.groddplantor4 <- glmmTMB(Seedlings.2023 ~ scale(Digging.depth) + scale(Total.outside.2023) + scale(Big.small.2022),
                             family = nbinom2, data = db.ytor)
summary(mod.groddplantor4)
AIC(mod.groddplantor4) # 395.4745
mod_dharma1 <- mod.groddplantor4 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1) # not zero inflated
ggplot(db.ytor, aes(x=Total.outside.2023, y=log10(Seedlings.2023+1), color= Digging.depth)) + geom_point()

# plot
visreg(mod.groddplantor4, scale="response")
plot(allEffects(mod.groddplantor4))

mod.groddplantor5 <- glmmTMB(Seedlings.2023 ~ Digging.depth + Total.outside.2023 + Big.small.2022,
                             family = nbinom2, data = db.ytor)
summary(mod.groddplantor5)
AIC(mod.groddplantor5) # 
mod_dharma1 <- mod.groddplantor5 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1) # not zero inflated
ggplot(db.ytor, aes(x=Total.outside.2023, y=log10(Seedlings.2023+1), color= Digging.depth)) + geom_point()

# plot
visreg(mod.groddplantor4, scale="response")
