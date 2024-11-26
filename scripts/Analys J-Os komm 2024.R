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

# Analysis groddplantor as binomial (finns= 1, ej finns=0)
mod.groddplantor <- glmmTMB(seed.bin ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                            family = binomial, data = db)
summary(mod.groddplantor)
AICc(mod.groddplantor) 
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantor))

# Analysis of only plots with seedlings (remove all zeroes)

db.poisson <- db %>% 
  filter(Seedlings.2023 > 0)
db.poisson
# 32 observations. Explore.
#correlation:
corr <- cor(db.poisson$Total.outside.2023, db.poisson$Big.small.2022, method= "pearson")
corr #r=0.30 to 0.50; moderate correlation

fig.1 <- ggplot(db.poisson, aes(x=Big.small.2022, y= Total.outside.2023, color= Djup.cm))+ geom_point() 
  #geom_smooth(alpha=0.3, method="lm") + ggtitle("ytor med bara 1 eller mer groddplantor, pearson correlation = 0.4314794")
fig.1

fig.2 <- ggplot(db.poisson, aes(x=Total.outside.2023, y= Seedlings.2023, color= Djup.cm))+ geom_point()
fig.2

fig.3 <- ggplot(db.poisson, aes(x=Big.small.2022, y= Seedlings.2023, color= Djup.cm))+ geom_point()
fig.3

mod.groddplantor2 <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                            family = nbinom2, data = db.poisson)
summary(mod.groddplantor2)
AICc(mod.groddplantor2) 
mod_dharma1 <- mod.groddplantor2 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testOutliers(mod.groddplantor2)
plot(allEffects(mod.groddplantor2))



