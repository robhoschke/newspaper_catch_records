library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(EnvStats)
library(gridExtra)
library(car)
library(ggpubr)


dt <- as_tibble(read_csv("historic_catch_data.csv")) %>% 
      mutate(Zone=as.factor(Zone)) %>% 
      mutate(mm=as.factor(mm)) %>% 
      mutate_at(c('largest.dhufish.kg'), ~na_if(., 0)) %>% 
      drop_na(Zone) %>% 
      drop_na(largest.dhufish.kg) %>% 
      mutate(decade = as.factor(case_when(
                                 yyyy >= 1957  & yyyy <= 1969 ~ '1957-1969',
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

unique(dt$Zone)
glimpse(dt)
head(dt)


#####broad plots#####

lm1 <- lm(largest.dhufish.kg~yyyy, data = dt)
summary(lm1)

ggplot(data = dt, aes(x = yyyy, y = largest.dhufish.kg)) + 
  geom_point() +
  geom_smooth(method=lm)+
  theme_classic()+
  stat_cor()

ggplot(data = dt, aes(x = yyyy, y = largest.dhufish.kg, colour=Zone)) + 
  geom_point() +
  geom_smooth(method = lm, se = F) +
  stat_cor()+
  scale_color_brewer(palette="Spectral")+
  scale_fill_brewer(palette="Spectral")+
  theme_classic2()

ggplot(data = dt, aes(x = decade, y = largest.dhufish.kg)) + 
  geom_boxplot() +
  stat_n_text(size=3)+
  ggtitle(paste("Largest dhufish by decade (whole metro area)"))+
  scale_fill_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

ggplot(data = dt, aes(x = mm, y = largest.dhufish.kg)) + 
  geom_boxplot() +
  stat_n_text(size=3)+
  scale_color_viridis_b()
  ggtitle(paste("Largest dhufish by month"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

month_count <- count(dt, mm)

ggplot(data = month_count, aes(x = mm, y = n)) + 
  geom_point() +
  ggtitle(paste("records per month"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()


ggplot(data = dt, aes(x = yyyy, y = largest.dhufish.kg)) + 
  geom_line() +
  ggtitle(paste("Largest dhufish by decade (whole metro area)"))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = dt, aes(x = Zone, y = largest.dhufish.kg)) + 
  geom_boxplot() +
  stat_n_text(size=3) +
  ggtitle(paste("Largest dhufish by zone (full period)"))+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8))


ggplot(subset(dt, decade %in% "2"), aes(Zone, largest.dhufish.kg)) + 
  geom_boxplot()

ggplot(subset(dt, Zone %in% "1"), aes(decade, largest.dhufish.kg)) + 
  geom_boxplot() + 
  stat_n_text(size=3)


#####plots by zone#####

unique(c(dt$Zone))


zones_to_include <- unique(c(dt$Zone))


plots_list <- list()


for (zone in zones_to_include) {
 
  subset_data <- subset(dt, Zone %in% zone)
  
  
  plot <- ggplot(subset_data, aes(decade, largest.dhufish.kg)) +
    geom_boxplot() +
    ggtitle(paste(zone))+
    stat_n_text(size = 2.5, y.pos = 0.5) +
    theme(
      plot.title = element_text(size = 8),  
      axis.title.x = element_blank(),  
      axis.title.y = element_text(size = 5)) +
      coord_cartesian(ylim = c(0, 25)) +
    
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=7))
  
 
  plots_list[[zone]] <- plot
}


grid.arrange(
  grobs = plots_list,
  ncol = 4,  
  nrow = 3 
)


#####plots by decade#####

glimpse(dt)


decades_to_include <- unique(c(dt$decade))


plots_list2 <- list()


for (d in decades_to_include) {
  
  subset_data <- subset(dt, decade %in% d)
  
  plot1 <- ggplot(subset_data, aes(Zone, largest.dhufish.kg)) +
    geom_boxplot() +
    ggtitle(paste("decade", d))+
    stat_n_text(size = 2.5, y.pos = 0.5) +
    theme(
      plot.title = element_text(size = 7),  
      axis.title.x = element_text(size = 7),  
      axis.title.y = element_text(size = 7)) +
      coord_cartesian(ylim = c(0, 25)) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8))
    
  
  
  plots_list2[[d]] <- plot1
}


grid.arrange(
  grobs = plots_list2,
  layout_matrix = rbind(c(1,4),
                        c(2,3)))


#####catches from dinghy#####

glimpse(dt)
unique(dt$Technology.mentioned)
unique(dt$Technology)
dinghy_records <- subset(dt, dinghy_presence %in% "1")
dinghy_records$Location



dt$dinghy_presence <- as.numeric(grepl("dinghy", dt$Technology.mentioned) | grepl("dinghy", dt$Technology))


ggplot(subset(dt, dinghy_presence %in% "1"), aes(yyyy, largest.dhufish.kg)) + 
  geom_point() +
  ggtitle(paste("Largest Dhufish from Dinghy")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(subset(dt, dinghy_presence %in% "1"), aes(decade, largest.dhufish.kg)) + 
  geom_boxplot() +
  ggtitle(paste("Largest Dhufish from Dinghy"))+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_n_text()


####two-way ANOVA####


model <- lm(largest.dhufish.kg ~ Zone * decade, data = dt)
model1 <- car::Anova(model)
summary(model)
car::
TukeyHSD(model1)


####proportion of counts for spatial plots in QGIS####

glimpse(dt)

result_table <- dt %>%
  group_by(Zone, decade) %>%
  summarise(RecordCount = n()) %>%
  pivot_wider(names_from = decade, values_from = RecordCount, values_fill = 0)

for (col in colnames(result_table[,-1])) {
  result_table[[col]] <- result_table[[col]] / sum(result_table[[col]])
}

print(result_table)
result_percent <- result_table[,-1]*100

####spatial plots in R####








