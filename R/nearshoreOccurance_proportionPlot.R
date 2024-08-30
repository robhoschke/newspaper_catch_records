###
# Project: Historical recreational fishing
# Data:    fishing trip distance data
# Task:    plotting proportion of fishing trips in nearshore zone overtime, and overlaying GAM predicts
# Author:  Rob
# Date:    July 2024

##
############### retrieve dataframe from fishing trip heatmaps 
############### using GAM predicts from bootstrap_GAM_nearshore_proportion;
#source("R/bootstrap_GAM_nearshore_proportion.R") ## run once, save outputs

library(ggplot2)
library(dplyr)
library(cowplot)

# Create 5-year bins
# df <- df %>%
#   mutate(yyyy_bin = cut(yyyy, breaks = c(1900, 1957, 1965, 1989, 2006, 2011))) %>%
#   mutate(depth_category = ifelse(bathy > -20, "Nearshore", "Inshore Demersal")) 

df <- df %>%
  mutate(yyyy_bin = cut(yyyy, breaks = c(1900, 1956, 1965, 1989, 2006, 2011), right = TRUE)) %>%
  mutate(depth_category = ifelse(bathy > -20, "Nearshore", "Inshore Demersal"))

# Summarize the data within each bin and divide counts by 1000
summary_data <- df %>%
  group_by(yyyy_bin, depth_category) %>%
  summarise(trip_count = n() / 1000) %>%
  ungroup()

# Calculate proportions
summary_data <- summary_data %>%
  group_by(yyyy_bin) %>%
  mutate(proportion = trip_count / sum(trip_count)) %>%
  ungroup()

summary_data <- summary_data %>% 
  mutate (yyyy_bin=str_remove_all(yyyy_bin, "\\(|\\]")) %>% 
  mutate(yyyy_bin=str_replace_all(yyyy_bin, ",", "-")) 

# Calculate sample sizes for each bin
sample_sizes <- summary_data %>%
  group_by(yyyy_bin) %>%
  summarise(total_trips = sum(trip_count))

####plot with year as factors on x axis####
#############
#####################
##########################

ggplot() +
  geom_col(data=summary_data, aes(x = yyyy_bin, y = proportion, fill = depth_category), position = "stack") +
  scale_fill_manual(values = c("Inshore Demersal" = "lightsalmon", "Nearshore" = "lightskyblue")) +
#  geom_text(data = sample_sizes, aes(x = yyyy_bin, y = 1.05, label = total_trips), size = 3, vjust = 0.5) +
  labs(
    x = "Time period",
    y = "Proportion of dhufish catches",
    fill = "Depth category")+
  scale_x_discrete (labels= c("A", "B", "C", "D", "E")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x = element_text(vjust = 5),
        axis.ticks.x = element_blank(),
        text = element_text(size = 20))


## plot each time bin individually

plots_list <- list()

for( i in unique(summary_data$yyyy_bin)){
  subset_period <- subset(summary_data, yyyy_bin %in% i)
  p <- ggplot() +
    geom_col(data=subset_period, aes(x = yyyy_bin, y = proportion, fill = depth_category),width=1) +
    scale_fill_manual(values = c("Inshore Demersal" = "lightsalmon", "Nearshore" = "lightskyblue"))+
    labs(y = "Proportion of catches")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_blank())+
    guides(fill = "none")
  plots_list[[i]] <- p 
}

leg <- cowplot::get_legend(plot[[1]])
plot(leg)
plot <- lapply(plots_list, ggplotGrob)
grid.arrange(grobs = plot, ncol = 3, nrow = 2) 

#### plot to overlay GAM - with continuous x-axis ####
###########
############## 

calculate_midpoint <- function(period) {
  years <- as.numeric(strsplit(period, "-")[[1]])
  return(mean(years))
}

# Apply the function to create a new column for midpoints
summary_data$year_midpoint <- sapply(summary_data$yyyy_bin, calculate_midpoint)
sample_sizes$year_midpoint <- sapply(sample_sizes$yyyy_bin, calculate_midpoint)

# Plot with continuous x-axis
ggplot() +
  geom_col(data = summary_data, aes(x = year_midpoint, y = proportion, fill = depth_category), position = "stack", width=4.6) +
  scale_fill_manual(values = c("Inshore Demersal" = "salmon", "Nearshore" = "lightskyblue")) +
  geom_text(data = sample_sizes, aes(x = year_midpoint, y = 1.05, label = total_trips), size = 3, vjust = 1) +
  #geom_line(data = mean_values, aes(x = yyyy, y = fit_mean), linetype=2, linewidth=0.8) +      ##############need to run gam models and save predictions
  #geom_ribbon(data = mean_values, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean), alpha = 0.1)+
  scale_x_continuous(breaks=c(1902.5, 1907.5, 1912.5, 1917.5, 1922.5, 1927.5, 1932.5, 1937.5, 1942.5, 1947.5, 1952.5, 1957.5, 1962.5, 1967.5, 1972.5, 1977.5, 1982.5, 1987.5, 1992.5, 1997.5, 2002.5, 2007.5, 2012.5),
                   labels=c("1900-1905", "1905-1910","1910-1915", "1915-1920","1920-1925", "1925-1930", "1930-1935", "1935-1940", "1940-1945", "1945-1950", "1950-1955", "1955-1960", "1960-1965", "1965-1970", "1970-1975", "1975-1980", "1980-1985", "1985-1990", "1990-1995", "1995-2000", "2000-2005", "2005-2010", "2010-2015"),
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = "Year",
    y = "Proportion of trips",
    fill = "Depth category",
    title = "Proportion of fishing trips in depth zones"
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank()) +
  theme(
    axis.text.x = element_text(angle=90,vjust=0.5, hjust=0.5),
    axis.ticks.x = element_blank()
  )




