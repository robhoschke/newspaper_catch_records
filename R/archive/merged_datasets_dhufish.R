library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(EnvStats)
library(gridExtra)
library(car)
library(ggpubr)

####import and clean datasets####
###alan

dat_alan <- as_tibble(read_csv('data/alan_database_expanded.csv'))%>% 
  mutate(Zone=as.factor(Zone)) %>%
  mutate(yyyy=as.numeric(yyyy)) %>%
  mutate_at(c('largest.dhufish.kg'), ~na_if(., 0)) %>% 
  drop_na(Zone) %>% 
  drop_na(largest.dhufish.kg) %>% 
  mutate(decade = as.factor(case_when(
    yyyy >= 1900  & yyyy <= 1909 ~ '1900-1909',
    yyyy >= 1910  & yyyy <= 1919 ~ '1910-1919',
    yyyy >= 1920  & yyyy <= 1929 ~ '1920-1929',
    yyyy >= 1930  & yyyy <= 1939 ~ '1930-1939',
    yyyy >= 1940  & yyyy <= 1949 ~ '1940-1949',
    yyyy >= 1950  & yyyy <= 1959 ~ '1950-1959'))) %>% 
  
  mutate(Zone = dplyr::recode(Zone, '1' = "offshore_north",
                              '2' = "nearshore_north",
                              '3' = "nearshore_perth",
                              '4' = "rottnest_is",
                              '6' = "carnac_is",
                              '7' = "nearshore_freo",
                              '8' = "behind_garden_is",
                              '9' = "cockburn_sound",
                              '10' = "safety_bay",))

glimpse(dat_alan)

###Joe

dat_joe <- as_tibble(read_csv('data/historic_catch_data.csv')) %>% 
  mutate(Zone=as.factor(Zone)) %>% 
  mutate(mm=as.factor(mm)) %>% 
  mutate(yyyy=as.numeric(yyyy)) %>%
  mutate_at(c('largest.dhufish.kg'), ~na_if(., 0)) %>% 
  drop_na(Zone) %>% 
  drop_na(largest.dhufish.kg) %>% 
  mutate(decade = as.factor(case_when(
    yyyy >= 1950  & yyyy <= 1959 ~ '1950-1959',
    yyyy >= 1960  & yyyy <= 1969 ~ '1960-1969',
    yyyy >= 1970  & yyyy <= 1979 ~ '1970-1979',
    yyyy >= 1980  & yyyy <= 1989 ~ '1980-1989',
    yyyy >= 1990  & yyyy <= 2000 ~ '1990-2000'))) %>% 
  
  mutate(Zone = dplyr::recode(Zone, '1' = "offshore_north",
                              '2' = "nearshore_north",
                              '3' = "nearshore_perth",
                              '4' = "rottnest_is",
                              '5' = "offshore_south",
                              '6' = "carnac_is",
                              '7' = "nearshore_freo",
                              '8' = "behind_garden_is",
                              '9' = "cockburn_sound",
                              '10' = "safety_bay",))

###Rob
dat_rob <- as_tibble(read_csv('data/rob_database.csv'))%>% 
  mutate(Zone=as.factor(Zone)) %>%
  mutate(yyyy=as.numeric(yyyy)) %>%
  mutate_at(c('largest.dhufish.kg'), ~na_if(., 0)) %>% 
  drop_na(largest.dhufish.kg) %>% 
  mutate(decade = as.factor(case_when(
    yyyy >= 2001  & yyyy <= 2009 ~ '2001-2009',
    yyyy >= 2010  & yyyy <= 2011 ~ '2010-2011')))



####merge early and late dfs####

merged_df <- merge(dat_alan, dat_joe, by = c("Reference", "yyyy", "decade", "Zone", "largest.dhufish.kg"), all = TRUE)
glimpse(merged_df)
unique(merged_df$Zone)

####merged with robs dataset####

merged_df_2 <- merge(merged_df, dat_rob, by = c("yyyy", "decade", "largest.dhufish.kg"), all = TRUE)
glimpse(merged_df_2)

ggplot(data = dat_rob, aes(x = yyyy, y = largest.dhufish.kg)) + 
  geom_point() +
  geom_smooth(method = lm, se = T, col= 'orange') +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  theme_classic2()+
  ylab("dhufish weight (kg)") +
  xlab("year") +
  scale_x_continuous(breaks = round(seq(min("1900"), max("2012"), by = 10),1))



####basic plots####

ggplot(data = merged_df, aes(x = decade, y = largest.dhufish.kg)) + 
  geom_boxplot() +
  stat_n_text(size=3)+
  ggtitle(paste("Largest dhufish by decade (whole metro area)"))+
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8))

pdf("figures/largest_dhufish_zone.pdf")

ggplot(data = merged_df, aes(x = Zone, y = largest.dhufish.kg)) + 
  geom_boxplot(color="black", fill="orange", alpha=0.5) +
  stat_n_text(size=3) +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  ylab("dhufish weight (kg)") +
  xlab("zone") 

dev.off()

pdf("figures/largest_dhufish_year.pdf")

ggplot(data = merged_df, aes(x = yyyy, y = largest.dhufish.kg)) + 
  geom_point() +
  geom_smooth(method = lm, se = T, col= 'orange') +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  theme_classic2()+
  ylab("dhufish weight (kg)") +
  xlab("year") +
  scale_x_continuous(breaks = round(seq(min("1900"), max("2000"), by = 10),1))

dev.off()


#####plots by zone#####

pdf("figures/plots_by_zone.pdf")

unique(c(merged_df$Zone))

zones_to_include <- unique(c(merged_df$Zone))

plots_list <- list()

for (zone in zones_to_include) {
  
  subset_data <- subset(merged_df, Zone %in% zone)
  
  
  plot <- ggplot(subset_data, aes(yyyy, largest.dhufish.kg)) +
    geom_point(size=1) +
    geom_smooth(method = lm, se = T, col= 'orange') +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 1900, label.y = 25, size=2) +
    ggtitle(paste(zone))+
    ylab("dhufish weight (kg)") +
    scale_x_continuous(breaks = round(seq(min("1900"), max("2000"), by = 10),1))+
    theme(
      plot.title = element_text(size = 6),  
      axis.title.x = element_blank(),  
      axis.title.y = element_text(size = 6),
      panel.border = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6),
      axis.text.y = element_text( vjust = 0.5, hjust=1, size=6)) +
    coord_cartesian(ylim = c(0, 25), xlim=c(1900,2000)) 
   
  
  plots_list[[zone]] <- plot
}


grid.arrange(
  grobs = plots_list,
  ncol = 4,  
  nrow = 3 
)

dev.off()

#####plots by decade#####

pdf("figures/plots_by_decade.pdf")

glimpse(merged_df)

decades_to_include <- unique(c(merged_df$decade))

plots_list2 <- list()

for (d in decades_to_include) {
  
  subset_data <- subset(merged_df, decade %in% d)
  
  plot1 <- ggplot(subset_data, aes(Zone, largest.dhufish.kg)) +
    geom_boxplot(color="black", fill="orange", alpha=0.5, lwd=0.2) +
    ggtitle(paste(d))+
    stat_n_text(size = 1.5, y.pos = 0.5) +
    ylab("largest dhufish (kg)") +
    theme(
      panel.background = element_blank(), 
      panel.grid.major = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7),
      axis.text.y = element_text(size = 5),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 7),  
      axis.title.y = element_text(size = 6),
      axis.title.x = element_blank()) +
    coord_cartesian(ylim = c(0, 25)) 
   
  
  plots_list2[[d]] <- plot1
}

grid.arrange(
  grobs = plots_list2,
  ncol = 4,  
  nrow = 3 
)

dev.off()

####test####

model <- aov(largest.dhufish.kg ~ Zone, data = merged_df)
model1 <- lm(largest.dhufish.kg ~ yyyy, data = dat_rob)
summary(model1)
tukeys <- TukeyHSD(model)
summary(tukeys)



####proportion of counts for spatial plots in QGIS####

glimpse(merged_df)

result_table1 <- merged_df %>%
  group_by(Zone, decade) %>%
  summarise(RecordCount = n()) %>%
  pivot_wider(names_from = decade, values_from = RecordCount, values_fill = 0)

colSums(result_table1[,2:11])

####export merged datasets with location description####

