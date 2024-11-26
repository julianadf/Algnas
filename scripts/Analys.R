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
library(MASS)
#library(ggeffects)
#library(effects)

# Create database without trees and with the new sites
raw <- read.csv2(here("data", "raw data clean.csv" ))

# Select relevant columns and prepare dataset
db <- raw %>% 
  dplyr::select(ID, Point.digging, Area, Digging.depth, Uneven.digging.depth, Seedlings.2023, Big.Small.2023,
                Total.outside.2023, Big.small.2022, Surroundings, Orientation, Djup.cm) %>% 
  mutate(ID = as.factor(ID)) %>% 
  mutate(Point.digging = as.factor(Point.digging)) %>% 
  mutate(Uneven.digging.depth = as.factor(Uneven.digging.depth)) %>% 
  mutate(Surroundings = as.factor(Surroundings)) %>% 
  mutate(Orientation = as.factor(Orientation)) %>% 
  mutate(Djup.cm = as.factor(Djup.cm)) %>% 
  mutate(plantorUtanför = ifelse(Total.outside.2023 == 0, "Nej", "Ja"))
str(db)

# Data exploration
dotchart(db$Area, xlab="Area (square meters)", ylab="Observation number") # yta 47 verkar fel men det var det inte
histogram(db$Area)
ggqqplot(db$Area)

area.plot <- ggplot(db, aes(x=fct_inorder(ID), y=Area, color= Point.digging)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank()) 
area.plot

ggplot(db, aes(y=log10(Seedlings.2023), x= Digging.depth, color=Digging.depth)) + geom_point()
# plot number of seedlings vs. dug area
cor.test(x=db$Area, y=db$Seedlings.2023, method = "pearson")
abline(lm(db$Seedlings.2023 ~ , data = mtcars), col = "blue")

dotchart(db$Digging.depth, xlab="Grävdjup", ylab="Sample area = observation number")
histogram(db$Digging.depth)

dotchart(db$Seedlings.2023, xlab="Antal groddplantor 2023", ylab="Sample area = observation number")
histogram(log10(db$Seedlings.2023)+1)

dotchart(db$Total.outside.2023, xlab="Total plantor utanför grävytan 2023", ylab="Sample area = observation number")
histogram(db$Total.outside.2023)

corr.seedling.out <- ggplot(db, aes(y= Total.outside.2023, x=Seedlings.2023, color= Point.digging)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank()) 
corr.seedling.out

test <- ggplot(db, aes(x=log10(Total.outside.2023+1), y=log(Big.small.2022+1)))+geom_point()
test

dotchart(db$Big.Small.2023, xlab="Stora + Små plantor inom ytan 2023", ylab="Sample area = observation number")
histogram(db$Big.Small.2023)

gg.depth <- ggplot(db, aes(x=Uneven.digging.depth, y= Seedlings.2023)) + geom_point()

dotchart(db$Big.small.2022, xlab="Stora + Små inom ytan 2022", ylab="Sample area = observation number")
histogram(db$Big.small.2022)

surroun.comp <- list(c("Garden", "Agricultural"), c("Garden", "Forest"), c("Garden", "Garden/Forest"), 
                     c("Agricultural", "Garden/Forest"), c("Agricultural", "Garden/Forest"), 
                     c("Garden/Forest", "Forest"))

gg.surround <- ggplot(db, aes(x=Surroundings, y=log10(Total.outside.2023+1))) + geom_boxplot() +
  ylab("log(Antal blomsterlupin plantor utanför ytan)+1")
gg.surround + stat_compare_means(comparisons = surroun.comp, method = "t.test", label="p.signif")


gg.surround2 <- ggplot(db, aes(x=Surroundings, y=log10(Seedlings.2023+1))) + geom_boxplot() +
  ylab("log10(Antal groddplantor inom ytan)+1")
gg.surround2 + stat_compare_means(comparisons = surroun.comp, method = "t.test", label="p.signif")


# Analys för punktgrävningar - går inte
db.punkt <- db %>% 
  filter(Point.digging == "Yes")
db.punkt

# Analys för stora ytor
db.ytor <- db %>% 
  filter(Point.digging == "No")
db.ytor

# Analys groddplantor: FINAL MODEL!!
mod.groddplantor <- glmmTMB(Seedlings.2023 ~ Big.small.2022 + Digging.depth + Total.outside.2023,
                            family = nbinom2, data = db.ytor)
summary(mod.groddplantor)
AIC(mod.groddplantor)
# nbinom2 fits the data better, and DHARMa doesnt complain about the residual vs. predicted either.
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testZeroInflation(mod_dharma1)
visreg(mod.groddplantor, scale="response")
plot(allEffects(mod.groddplantor))
vif(mod.groddplantor)

# To be able to calcultate VIF cause glmmTMB doesnt work ----
mod.groddplantorVIF <- MASS::glm.nb(Seedlings.2023 ~ Big.small.2022 + Digging.depth + Total.outside.2023, 
                              data = db.ytor, init.theta = 0.128)
summary(mod.groddplantorVIF)
mod_dharma1 <- mod.groddplantorVIF %>% simulateResiduals(n=1000)
plot(mod_dharma1)
vif(mod.groddplantorVIF)
# ----

db.ut <-  db %>% 
  filter(Total.outside.2023 == 0)
db.ut

mod.groddplantor2 <- glmmTMB(Seedlings.2023 ~ Big.small.2022 + Digging.depth  + Surroundings,
                            family = nbinom2, data = db.ut)
summary(mod.groddplantor2)
AIC(mod.groddplantor)

# look if deeper dug areas tend to be next to gardens
garden.seeds <- ggplot(db.ytor, aes(x=Surroundings, y=Seedlings.2023, color=Djup.cm))+ geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank()) 
garden.seeds

# look for influence of orientation
seeds.orientation <- ggplot(db.ytor, aes(x=Orientation, y=Seedlings.2023))+ geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank()) 
seeds.orientation

# Analys stora plantor
mod.storaplantor <- glmmTMB(Big.Small.2023 ~ Big.small.2022 + Djup.cm + Total.outside.2023 + Surroundings,
                            family = nbinom2, data = db)
summary(mod.groddplantor)
AIC(mod.groddplantor)
# nbinom2 fits the data better, and DHARMa doesnt complain about the residual vs. predicted either.
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
visreg(mod.groddplantor, scale="response")
plot(allEffects(mod.groddplantor))


gg.stora <- ggplot(db, aes(x=Djup.cm, y=Big.Small.2023, color=Point.digging)) + geom_point()
gg.stora


# Analys groddplantor: FINAL MODEL!! ----
cor.test(db.ytor$Total.outside.2023, db.ytor$Big.small.2022, method = c("pearson"))
cor.vars <- ggplot(db.ytor, aes(x=log10(Total.outside.2023+1), y=log(Big.small.2022+1), color=Surroundings)) + geom_point()
cor.vars

fig.test <- ggplot(db.ytor, aes(x=log10(Total.outside.2023+1), y=log10(Seedlings.2023+1)))+geom_point()
fig.test

fig.test2 <- ggplot(db.ytor, aes(x=log10(Total.outside.2023+1), y=log10(Seedlings.2023+1)))+geom_point()
fig.test2

# model
mod.groddplantor <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022,
                            family = nbinom2, data = db.ytor)


summary(mod.groddplantor)
AIC(mod.groddplantor)
# nbinom2 fits the data better, and DHARMa doesnt complain about the residual vs. predicted either.
mod_dharma1 <- mod.groddplantor %>% simulateResiduals(n=1000)
testZeroInflation(mod_dharma1)
plot(mod_dharma1)
plot(allEffects(mod.groddplantor))
vif(mod.groddplantorLME)

mod.groddplantor2 <- glm.nb(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022,
                            link=log, data = db.ytor)
summary(mod.groddplantor2)
AIC(mod.groddplantor2)
vif(mod.groddplantor2)
visreg(mod.groddplantor2, scale="response")
plot(allEffects(mod.groddplantor2))


# Zero-inflated model
zimod.groddplantor <- glmmTMB(Seedlings.2023 ~  Digging.depth + Total.outside.2023 + Big.small.2022,
                            family = nbinom2, ziformula=~1, data = db.ytor)
summary(zimod.groddplantor)
AIC(zimod.groddplantor)
mod_dharma1 <- zimod.groddplantor %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(zimod.groddplantor))

