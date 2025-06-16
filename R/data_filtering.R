###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    Loading and filtering data
# Author:  Rob
# Date:    April 2024
##


 # install.packages(c(
 #   "paletteer", "spatstat", "sf", "tibble", "dplyr", "tidyr", "readr",
 #   "ggplot2", "tidyverse", "MASS", "ggpubr","ggspatial", "car", "maps", "ggtext",
 #   "metR", "terra", "EnvStats", "gridExtra", "geosphere", "osmdata",
 #   "gratia", "mgcv", "corrr", "broom", "ggsn","googleway", "ggrepel",
 # "libwgeom", "rnaturalearth", "rnaturalearthdata"
 # 
 # ))

library(paletteer)
library(spatstat)
library(sf)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(EnvStats)
library(gridExtra)
library(car)
library(ggpubr)
library(MASS)
library(tidyverse)
library(ggtext)
library(metR)
library(terra)
library(geosphere)
library(osmdata)
library(gratia)
library(mgcv)
library(corrr)
library(broom)
library(ggspatial)


##### import data#####

##fishing records
dhu_records <- read.csv('data/dhufish_records_updated01042025.csv')

##checks
summary(dhu_records$ID)
duplicated(dhu_records$ID)
unique(dhu_records$yyyy)

###fishing location estimates
##
trip_location <- st_read('Qgis/shp/trip_location_estimate_complete.shp')

# check no missing ID's 
id_list <- trip_location$ID
id_list <- sort(id_list)
full_sequence <- seq(min(id_list), max(id_list))
missing_ids <- setdiff(full_sequence, id_list)

##checks
summary(trip_location$ID)
duplicated(trip_location$ID)



##remaining data
metro_area <- st_read('Qgis/shp/perth_metro.shp')
perth_coastline <- st_read(('Qgis/shp/Perth_coastline.shp'))
Freo_harbour <- st_read(('Qgis/shp/Freo_harbour.shp'))
WA_base <- st_read('Qgis/shp/basemap.shp')
bathy <- rast("Qgis/raster/bathy_cropped1.tif")
WApop <- read.csv('data/WApop.csv') %>% 
          rename(yyyy = Year)

##### crop trip locations to remove land #####
#still going to be some overlap, where the bathy does not align with the basemap
glimpse(dhu_records)
cropped_trip_location <- st_intersection(trip_location, metro_area) 
glimpse(cropped_trip_location)

##### join spatial data #####

fishing_trips <- left_join(cropped_trip_location, dhu_records, by = c("ID")) %>% 
arrange(yyyy) 
unique(fishing_trips$yyyy)  
is.na(fishing_trips$yyyy)

#left_join(WApop, by = "yyyy")

head(fishing_trips)

glimpse(fishing_trips)


##### centroid from each polygon for reference model #####
glimpse(fishing_trips)
plot(fishing_trips$geometry)

centroids <- st_centroid(fishing_trips)
centroid_df <- data.frame(
  ID=centroids$ID,
  yyyy=centroids$yyyy,
  month=centroids$mm,
  largest.dhufish.kg=centroids$largest.dhufish.kg,
  geometry=centroids$geometry
)
glimpse(centroid_df)

dat <- centroid_df %>%
  st_as_sf() %>%
  mutate(
    month = as.factor(month),
    latitude = st_coordinates(geometry)[, "Y"],
    scientific = "fish.size",
    echo_sounders = factor(ifelse(yyyy < 1970, "pre", "post"), levels = c("pre", "post")),
    gps = factor(ifelse(yyyy < 1990, "pre", "post"), levels = c("pre", "post"))
  ) %>%
  mutate(
    bathy = extract(bathy, .)$bathy_cropped1,
    bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy), ##resample positive bathy values
    depth = bathy * -1,
    depth_zone = as.factor(ifelse(depth <= 20, "nearshore", "inshore_demersal"))
  ) %>% 
  arrange(ID) %>% 
  mutate(depth=bathy*-1)    

glimpse(dat)
plot(dat)
#write.csv(dat, "data/population_data_centroids.csv")


###add distance to shoreline variable

# dist <- geosphere::dist2Line(p = st_coordinates(dat), 
#                              line = st_coordinates(perth_coastline)[,1:2])

dist1 <- geosphere::dist2Line(p = st_coordinates(dat), 
                              line = st_coordinates(Freo_harbour)[,1:2])

#combine initial data with distance to coastline
#dat <- cbind(dat,dist) 
dat <- cbind(dat,dist1)
glimpse(dat)

# Perform the join
dat <- dat %>%
  left_join(WApop, by = "yyyy")

# Check the result
glimpse(dat)



