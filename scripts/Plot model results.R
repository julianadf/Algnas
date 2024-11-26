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
library(sjPlot)
library(ggeffects)

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

# Analys groddplantor: FINAL MODELS!!
mod.groddplantor <- glmmTMB(seed.bin ~  Digging.depth + Total.outside.2023 + Big.small.2022 + Area + (1|ID.), 
                            family = binomial, data = db)
summary(mod.groddplantor)
AICc(mod.groddplantor) 
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
plot(allEffects(mod.groddplantor))
car::Anova(mod.groddplantor, type= "III")
tab_model(mod.groddplantor)

# Plot for publication
# plot the only significant variable:
figure2 <- visreg(mod.groddplantor, "Total.outside.2023", scale="response", partial = FALSE, 
                  rug = 0, line = list(col = "black"), gg=TRUE) + 
  labs(x="Number of lupines adjacent to the plot", y="P(seedling)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x.bottom = element_text(size=22, family="serif"), axis.text.y.left = element_text(size=22, family="serif"),
        axis.title.y = element_text(size=24, family="serif"), axis.title.x = element_text(size=24, family="serif")) 
figure2

# extract the probability values
effect.ED <- effects::effect(term= "Total.outside.2023",  mod= mod.groddplantor)
summary(effect.ED)
x_ED <- as.data.frame(effect.ED)

# the others, just in case:

figure2a <- visreg(mod.groddplantor, "Digging.depth", scale="response", partial = FALSE, rug = 0, line = list(col = "black"), gg=TRUE) + 
  labs(x="Excavation depth (cm)", y="P(success)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=22, family="serif"), axis.text.y.left = element_text(size=22, family="serif"),
        axis.title.y = element_text(size=24, family="serif"), axis.title.x = element_text(size=24, family="serif")) 
figure2a


figure2c <- visreg(mod.groddplantor, "Big.small.2022", scale="response", partial = FALSE, rug = 0, line = list(col = "black"), gg=TRUE) + 
  labs(x="Number of lupines previous year", y="P(success)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=22, family="serif"), axis.text.y.left = element_text(size=22, family="serif"),
        axis.title.y = element_text(size=24, family="serif"), axis.title.x = element_text(size=24, family="serif")) 
figure2c


figure2d <- visreg(mod.groddplantor, "Area", scale="response", partial = FALSE, rug = 0, line = list(col = "black"), gg=TRUE) + 
  labs(x="Area of the plot", y="P(success)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5), axis.text.x.bottom = element_text(size=22, family="serif"), axis.text.y.left = element_text(size=22, family="serif"),
        axis.title.y = element_text(size=24, family="serif"), axis.title.x = element_text(size=24, family="serif")) 
figure2d
