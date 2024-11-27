install.packages("vegan")
install.packages("dartR")
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


glimpse(dhu_records)

dhu_records_cleaned <- dhu_records[complete.cases(dhu_records[, 49:73]), ]

species_pres_abs <- dhu_records_cleaned[,49:73]
m_species_pres_abs <- as.matrix(species_pres_abs) 

nmds = metaMDS(m_species_pres_abs, distance = "euclidian")
nmds

plot(nmds)

data.scores = as.data.frame(scores(nmds)$sites)
data.scores$decade = dhu_records_cleaned$decade
data.scores$Zone = dhu_records_cleaned$Zone.y

species_pres_abs$decade= as.factor(dhu_records_cleaned$decade) 


species_pres_abs %>%
  group_by(decade) %>%
  summarise(across(everything(), sum))

species_pres_abs %>%
  group_by(decade) %>%
  summarise(across(everything(), ~ mean(.))) 

glimpse(species_pres_abs)

head(data.scores)

ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 2, aes(colour = Zone))+ 
  theme(panel.background = element_blank())+
  geom_jitter()

species.scores <- as.data.frame(scores(nmds, display = "species"))
species.scores$Species <- colnames(m_species_pres_abs)


ggplot() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = decade), size = 1, position = position_jitter(width = 0.1, height = 0.1)) + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = Species), 
            color = "red", vjust = 1.5, hjust = 0.5, size = 3) +  # Label species
  theme(panel.background = element_blank()) + 
  geom_jitter()



head(data.scores)
