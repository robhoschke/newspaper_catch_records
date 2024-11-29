###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    visualising month effect 
# Author:  Rob
# Date:    August 2024
##

source("R/data_filtering.R")
library("plyr")


##overall monthly occurance
unique(trip_points$mm)

head(trip_points)
dat

ggplot(dat, aes(x = month, fill = depth_zone)) +
  geom_bar(position = "dodge") + # Bar plot with bars grouped by depth_zone
  facet_wrap(~depth_zone) + # Separate plots for each depth zone
  labs(
    title = "Counts by Month and Depth Zone",
    x = "Month",
    y = "Count",
    fill = "Depth Zone"
  ) +
  scale_fill_manual(values = c("skyblue", "salmon")) + # Adjust colors if needed
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top"
  )





dat <- dat %>%
  mutate(
    decade = cut(yyyy, 
                 breaks = seq(1900, 2020, by = 10), 
                 labels = paste0(seq(1900, 2010, by = 10), "s"), 
                 include.lowest = TRUE)
  )

dat_counts <- dat %>%
  dplyr::group_by(decade, month, depth_zone) %>%
  dplyr::summarise(count = n(), .groups = "drop")  



dat_counts <- dat %>%
  mutate(
    decade = cut(
      yyyy,
      breaks = seq(1900, 2020, by = 10),
      labels = paste0(seq(1900, 2010, by = 10), "s"),
      include.lowest = TRUE
    )
  ) %>%
  count(decade, month, depth_zone)
 












ggplot(dat, aes(x = yyyy, fill = month)) +
  geom_bar(position = "fill", stat = "count") +
  facet_wrap(~depth_zone) +
  labs(
    title = "Proportion of Monthly Counts Over Time",
    x = "Year",
    y = "Proportion",
    fill = "Month"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )


ggplot(dat, aes(x = yyyy, fill = month)) +
  geom_bar(position = "fill", stat = "count") +
  facet_wrap(~depth_zone) +
  labs(
    title = "Proportion of Monthly Counts Over Time",
    x = "Year",
    y = "Proportion",
    fill = "Month"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )



monthly_occ <- trip_points %>% 
  group_by(mm) %>% 
  tally() %>%
  mutate(n=n/1000) %>% 
  mutate(mm=as.factor(mm)) 

##monthly occurance by depth category

nearshore_mm <- df %>% 
  dplyr::filter(bathy > -20)%>% 
  group_by(mm) %>% 
  tally() %>%
  mutate(n=n/1000) %>% 
  mutate(mm=as.factor(mm)) %>% 
  as.data.frame(nearshore_mm) %>% 
  dplyr::select(c(mm, n))

midshore_mm <- df %>% 
  dplyr::filter(bathy < -20)%>% 
  group_by(mm) %>% 
  tally() %>%
  mutate(n=n/1000) %>% 
  mutate(mm=as.factor(mm)) %>% 
  as.data.frame(nearshore_mm) %>% 
  dplyr::select(c(mm, n))

offshore_mm <- df %>% 
  dplyr::filter(bathy < -50)%>% 
  group_by(mm) %>% 
  tally() %>%
  mutate(n=n/1000) %>% 
  mutate(mm=as.factor(mm)) %>% 
  as.data.frame(nearshore_mm) %>% 
  dplyr::select(c(mm, n))

##combine depth zones into signle df for plot

ggplot() +
  geom_point(data=nearshore_mm, aes(x=mm, y=n), col='red')+
  geom_point(data=midshore_mm, aes(x=mm, y=n), col='blue')+
  geom_point(data=offshore_mm, aes(x=mm, y=n), col='green')

month_summary <- join_all(list(nearshore_mm,midshore_mm,offshore_mm), by='mm', type='left')

month_summary <- left_join(nearshore_mm,midshore_mm,offshore_mm, by = "mm") 
  colnames(month_summary) <- c("month", "nearshore", "midshore", "offshore")
  
  month_summary <- month_summary %>% 
  mutate(month = as.numeric(month)) %>% 
    pivot_longer(nearshore:offshore, names_to = "depth_zone", values_to = "n") %>% 
    glimpse()

ggplot()+
  geom_line(data = month_summary, aes(x=month, y=n, colour = depth_zone))+
  scale_x_continuous(breaks = c(1:12))+
  theme_minimal()

