###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    Loading and filtering data
# Author:  Rob
# Date:    April 2024
##

# install.packages("sf")
# install.packages("spatstat")
# install.packages("paletteer")
# install.packages("maps")
# install.packages("ggtext")
# install.packages("metR")
# install.packages("terra")
# install.packages("EnvStats")
# install.packages("gridExtra")
# install.packages("ggpubr")


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

##### import data#####
dhu_records <- read.csv('data/all_dhufish_records_edited.csv')
metro_area <- st_read('Qgis/shp/perth_metro.shp')
###perth_zones <- st_read('Qgis/shp/perth_rec_fsihing_zones.shp') ## old zones
perth_zones <- st_read('Qgis/shp/rec_zones_fewer.shp') 
trip_location <- st_read('Qgis/shp/trip_location_estimate_complete.shp')
WA_base <- st_read('Qgis/shp/basemap.shp')
bathy <- rast("Qgis/raster/bathy_cropped1.tif")

##### crop trip locations to remove land #####
#still going to be some overlap, where the bathy does not align with the basemap
glimpse(dhu_records)
cropped_trip_location <- st_intersection(trip_location, metro_area) 
glimpse(cropped_trip_location)

##### join spatial data #####

fishing_trips <- left_join(cropped_trip_location, dhu_records, by = c("ID") )
glimpse(fishing_trips)

##### centroid from each polygon for reference model #####
glimpse(fishing_trips)
plot(fishing_trips$geometry)

centroids <- st_centroid(fishing_trips)
centroid_df <- data.frame(
  ID=centroids$ID,
  yyyy=centroids$yyyy,
  largest.dhufish.kg=centroids$largest.dhufish.kg,
  geometry=centroids$geometry
)
glimpse(centroid_df)

dat <- centroid_df %>%
  st_as_sf() %>%
  st_join(st_as_sf(perth_zones)) %>%
  mutate(
    Zone = as.factor(Zone),
    latitude = st_coordinates(geometry)[, "Y"],
    scientific = "fish.size",
    echo_sounders = factor(ifelse(yyyy < 1970, "pre", "post"), levels = c("pre", "post")),
    gps = factor(ifelse(yyyy < 1990, "pre", "post"), levels = c("pre", "post"))
  ) %>%
  mutate(
    bathy = extract(bathy, .)$bathy_cropped1,
    bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy) ##resample positive bathy values
  ) %>% 
  arrange(ID) %>% 
  mutate(depth=bathy*-1,
         yyyy=as.numeric(yyyy)-1904)

glimpse(dat)
write.csv(dat, "data/population_data_centroids.csv")
