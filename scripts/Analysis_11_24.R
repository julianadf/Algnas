rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(vegan)
library(MuMIn)
library(visreg)
library(effects)
library(ggpubr)
library(car)
library(interactions)
library(emmeans)
library()

# Create database without trees and with the new sites
raw <- read.csv2(here("data", "raw data clean_long.csv" ))

# Select relevant columns and prepare dataset
db <- raw %>% 
  dplyr::select(ID_plt, Area, Digging.depth, Year, 
                Seedlings.inside, Big.inside, Small.inside, 
                Big.outside.l, Small.outside.l, 
                Big.outside.b, Small.outside.b,
                Big.outside.u, Small.outside.u,
                Total.big.outside,
                Surroundings, Orientation) %>% 
  mutate(ID_plt = as.factor(ID_plt)) %>%
  mutate(Year = as.factor(Year)) %>% 
  mutate(Surroundings = as.factor(Surroundings)) %>% 
  mutate(Orientation = as.factor(Orientation)) %>% 
  mutate(Plants.outside = ifelse(Total.big.outside == 0, "No", "Yes")) %>%
  mutate(Plants.outside = as.factor(Plants.outside))
str(db)

# Data exploration
# 2022
db.2022 <- db %>% 
  filter(Year == "2022")
db.2022
 
plot1.2022 <- ggplot(db.2022, aes(x=Big.inside, y=Small.inside, color=Area)) + geom_point() +
  stat_smooth(method="glm", color="gold", se=TRUE,alpha = 0.1,method.args = list(family=poisson)) +
  theme_bw() +
  ggtitle("2022")
plot1.2022

# 2023
db.2023 <- db %>% 
  filter(Year == "2023")
db.2023

plot1.2023 <- ggplot(db.2023, aes(x=Big.inside, y=Small.inside)) + geom_point() +
  stat_smooth(method="glm", color="gold", se=TRUE,alpha = 0.1,method.args = list(family=poisson)) +
  theme_bw() +
  ggtitle("2023")
plot1.2023





# Make plots for 2023




# Select those without plants outside and plot
db1 <- db %>% 
  filter(Year != "2022") %>% 
  filter(plantorUtanför == "Nej")
db1

plot1 <- ggplot(db1, aes(x=Djup.cm , y=Big.Small, color= plantorUtanför)) + 
  geom_point(position=position_jitter(h=0.09,w=0.09)) +
  theme_bw() +
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size=16), 
        legend.title = element_blank())
plot1

# Select those with plants outside and plot
db.dirtyOutside <- db %>% 
  filter(!Total.outside.2023 == 0)
db.dirtyOutside

plot2 <- ggplot(db.dirtyOutside, aes(x=Digging.depth, y= Seedlings.2023)) + 
  geom_point() +
  theme_bw() +
  ylab("Antal groddplantor inom bekämpningsytan") +
  xlab("Grävdjup (cm)") +
  ggtitle("Ytor med plantor utanför grävytan (n=97)") +
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size=16), 
        legend.title = element_blank())
plot2

# filtrera bort punktgrävningarna
db.ytor <- db %>% 
  filter(Point.digging == "No")
db.ytor

plot3 <- ggplot(db.ytor, aes(x=Digging.depth, y= Seedlings.2023, color=plantorUtanför)) + 
  geom_point(size=2) +
  theme_bw() +
  ylab("Antal groddplantor inom bekämpningsytan") +
  xlab("Grävdjup (cm)") +
  #ggtitle("Bara stora bekämpningsytor (dvs. inga punktgrävningar, n=127)") +
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size=16)) 

plot3


plot4 <-  ggplot(db.ytor, aes(x=log10(Big.small.2022+1), y= log10(Seedlings.2023+1), color=Digging.depth)) + 
  geom_point(position=position_jitter(h=0.11,w=0.11), size=2 ) +    scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_bw() +
  ylab("log(Antal groddplantor inom bekämpningsytan + 1)") +
  xlab("log(Antal plantor inom ytan 2022 + 1)") +
  #ggtitle("Bara stora bekämpningsytor (dvs. inga punktgrävningar, n=127)") +
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size=16)) 
plot4


plot5 <-  ggplot(db.ytor, aes(x=log10(Total.outside.2023+1), y= log10(Seedlings.2023+1), color=Digging.depth)) + 
  geom_point(size=2) +    scale_colour_gradientn(colours = terrain.colors(10)) +
  theme_bw() +
  ylab("log(Antal groddplantor inom bekämpningsytan + 1)") +
  xlab("log(Antal plantor utanför ytan 2023 + 1)") +
  #ggtitle("Bara stora bekämpningsytor (dvs. inga punktgrävningar, n=127)") +
  theme(axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14), 
        plot.title = element_text(size=16)) 
plot5
