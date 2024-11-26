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


# Select those without plants outside and plot
db.cleanOutside <- db %>% 
  filter(Total.outside.2023 == 0)
db.cleanOutside

plot1 <- ggplot(db.cleanOutside, aes(x=Digging.depth, y= Seedlings.2023)) + 
  geom_point(position=position_jitter(h=0.09,w=0.09)) +
  theme_bw() +
  ylab("Antal groddplantor inom bekämpningsytan") +
  xlab("Grävdjup (cm)") +
  ggtitle("Ytor utan plantor utanför grävytan (n=50)") +
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
