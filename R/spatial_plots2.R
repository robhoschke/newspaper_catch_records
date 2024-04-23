install.packages("sf")
install.packages("spatstat")
install.packages("paletteer")
install.packages("maps")
install.packages("ozmaps")

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
library(ozmaps)
library(gridExtra)

getwd()
dhu_records <- read.csv('data/all_dhufish_records_edited.csv')
trip_location <- st_read('Qgis/shp/trip_location_estimate_complete.shp')
glimpse(trip_location)
st_crs(trip_location)
head(trip_location)
plot(trip_location)
WA_base <- st_read('Qgis/shp/basemap.shp')
st_crs(WA_base)


##### join spatial data #####

fishing_trips <- left_join(trip_location, dhu_records, by = c("ID") )
glimpse(fishing_trips)

dhu_with_location <- merge(dhu_records, trip_location, by = c("ID"))
glimpse(dhu_with_location)
plot(dhu_with_location$geometry)
ozmap(add=TRUE)
st_crs(dhu_with_location$geometry)


#####sampling random points within single polygon#####

polygon <- fishing_trips[1, ]
random_points <- st_sample(polygon$geometry, 1000)
plot(random_points)
metadata_df <- data.frame(yyyy = rep(polygon$yyyy, 1000),
                          largest.dhufish.kg = rep(polygon$largest.dhufish.kg, 1000))
random_points_with_metadata <- cbind(metadata_df, geometry = random_points)
print(random_points_with_metadata)

#####random points for each polygon#####

all_random_points_with_metadata <- list()


for (i in 1:nrow(fishing_trips)) {
  
  polygon <- fishing_trips[i, ]
  random_points <- st_sample(polygon$geometry, 10)
  metadata_df <- data.frame(yyyy = rep(polygon$yyyy, 10),
                            largest.dhufish.kg = rep(polygon$largest.dhufish.kg, 10),
                            decade = as.factor(rep(polygon$decade, 10)))
  random_points_with_metadata <- cbind(metadata_df, geometry = random_points)
  all_random_points_with_metadata[[i]] <- random_points_with_metadata
}

trip_points <- do.call(rbind, all_random_points_with_metadata)
glimpse(trip_points)
print(trip_points)
st_crs(trip_points$geometry)
warnings()
##### spatial plots all data #####


plot(trip_points$geometry, 
     col=trip_points$decade, pch=20, cex = 0.2)
ozmap(add= TRUE, col='gray')

# Plot the points
plot(trip_points$geometry, 
     col = trip_points$decade, 
     pch = 20, cex = 0.2)

# Create a legend
legend("topright", 
       legend = levels(trip_points$decade), 
       col = palette()[1:length(levels(final_random_points_with_metadata$decade))], 
       pch = 20, 
       title = "Decade")

##### spatial plots by decade #####


decades_to_include <- unique(trip_points$decade)

overall_bbox <- st_bbox(trip_points$geometry)

for (i in 1:length(decades_to_include)) {
  # Subset data for the current decade
  subset_data <- subset(trip_points, decade == decades_to_include[i])
  plot(subset_data$geometry, pch = 20, cex = 0.2, col = subset_data$decade,
       main = paste(decades_to_include[i]),
       xlim = c(overall_bbox["xmin"], overall_bbox["xmax"]),
       ylim = c(overall_bbox["ymin"], overall_bbox["ymax"]))
}


##### sizing points based on dhufish size  #####

min_fish_size <- min(trip_points$largest.dhufish.kg) 
max_fish_size <- max(trip_points$largest.dhufish.kg)

decades_to_include <- unique(trip_points$decade)

overall_bbox <- st_bbox(trip_points$geometry)

for (i in 1:length(decades_to_include)) {
  
  subset_data <- subset(trip_points, decade == decades_to_include[i])
  scaled_sizes <- 1 + 4 * ((subset_data$largest.dhufish.kg - 15) / (max_fish_size - 15))^2
  plot(subset_data$geometry, pch = 20, cex = scaled_sizes, 
       col = subset_data$decade,
       main = paste(decades_to_include[i]),
       xlim = c(overall_bbox["xmin"], overall_bbox["xmax"]),
       ylim = c(overall_bbox["ymin"], overall_bbox["ymax"]))
}


##### density plot all decades #####
##different plotting options

ggplot(trip_points, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  stat_density_2d(aes(fill = ..level..), bins=200, geom = "polygon") +
  geom_sf(data = WA_base, inherit.aes = FALSE) +
  xlim(114.9851, 115.8) +
  ylim(-32.7966, -31.30936) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() + 
  geom_point(size=0.5) 

ggplot(trip_points, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  stat_density_2d(aes(fill = ..density..), bins=20, geom = "raster", contour=FALSE) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() + 
  geom_point(size=0.5) 

ggplot(trip_points, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  geom_bin2d(bins=40) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


##### heat map by decade #####

plot(WA_base)
glimpse(WA_base)
st_crs(WA_base)
st_crs(trip_points)
str(trip_points)
str(trip_location)
str(WA_base)

p2 <- 
  ggplot() +
  geom_sf(data = WA_base) +
  labs(title = "Polygon from QGIS") +
  xlim(114.9851, 115.7638) +
  ylim(-32.7966, -31.30936)+
  theme_minimal()

str(trip_points)
ordered_trip_points <- trip_points[order(trip_points[,1]),]

decades_to_include <- unique(c(ordered_trip_points$decade))

plots_list4 <- list()

for (d in decades_to_include) {
  subset_data <- subset(ordered_trip_points, decade %in% d)
  
  p <- ggplot(subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
    stat_density_2d(aes(fill = ..density..), bins = 45, geom = "raster", contour = FALSE, show.legend = FALSE)+
    geom_sf(data = WA_base, inherit.aes = FALSE) +
    labs(title = paste(d), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    xlim(114.9851, 115.8) +
    ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=5) 
  
  plots_list4[[d]] <- p 
  
}

g <- lapply(plots_list4, ggplotGrob)
grid.arrange(grobs = g, ncol = 6, nrow = 2)


##### broad decade groups #####
















