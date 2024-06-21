library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(EnvStats)
library(gridExtra)
library(car)
library(ggpubr)
library(vegan)
library(dartR)

data(dune)
data(varespec)
data(dune.env)
glimpse(dune.env)
adonis(dune ~ Management*A1, data=dune.env, permutations=99)

species_pres_abs <- dat_joe[,41:65]
m_species_pres_abs <- as.matrix(species_pres_abs)
nmds = metaMDS(m_species_pres_abs, distance = "euclidian")
nmds

plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)
data.scores$decade = dat_joe$decade
data.scores$Zone = dat_joe$Zone
head(data.scores)

ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 2, aes(colour = Zone))+ 
  theme(panel.background = element_blank())+
  geom_jitter()



ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 2, aes( shape = decade, colour = Zone))+ 
  theme(panel.background = element_blank()) 

head(data.scores)
