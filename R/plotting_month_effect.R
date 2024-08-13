###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    visualising month effect 
# Author:  Rob
# Date:    August 2024
##

source("R/data_filtering.R")


##overall monthly occurance
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

