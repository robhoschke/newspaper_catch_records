###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    Plotting fishing trip density 
# Author:  Rob
# Date:    April 2024
##

source("R/data_filtering.R")

##### random points for each polygon#####
all_random_points_with_metadata <- list()

for (i in 1:nrow(fishing_trips)) {
  
  polygon <- fishing_trips[i, ]
  random_points <- st_sample(polygon$geometry, 1)
  metadata_df <- data.frame(yyyy = rep(polygon$yyyy, 1),
                            ID = rep(polygon$ID, 1),
                            largest.dhufish.kg = rep(polygon$largest.dhufish.kg, 1),
                            decade = as.factor(rep(polygon$decade, 1)))
  random_points_with_metadata <- cbind(metadata_df, geometry = random_points)
  all_random_points_with_metadata[[i]] <- random_points_with_metadata
}

trip_points <- do.call(rbind, all_random_points_with_metadata)
glimpse(trip_points)

#####add zones and bathy#####

data <- trip_points %>%
  st_as_sf() %>%
  st_join(st_as_sf(perth_zones)) %>%
  mutate(Zone = as.factor(Zone)) %>%
  st_crop(bathy) %>%
  mutate(bathy = extract(bathy, .)$bathy_cropped1) %>% 
  mutate(
    bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy))   ##resample positive bathy values

###########can't arrange data
summary(data$bathy)
plot(largest.dhufish.kg~yyyy, data=data)

##### sample of points for heatmaps#####

##### spatial plots all data #####

# Plot the points
plot(trip_points$geometry, 
     pch = 20, cex = 0.2)

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

ordered_trip_points <- trip_points[order(trip_points[,1]),]

decades_to_include <- unique(c(data$decade))

plots_list3 <- list()

for (d in decades_to_include) {
  subset_data <- subset(data, decade %in% d)
  
  p <- ggplot(subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
    stat_density_2d(aes(fill = ..density..), bins = 45, geom = "raster", contour = FALSE, show.legend = FALSE)+
    geom_sf(data = WA_base, inherit.aes = FALSE) +
    labs(title = paste(d), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    xlim(114.9851, 115.8) +
    ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=5) 
  
  plots_list3[[d]] <- p 
  
}

plot1 <- lapply(plots_list3, ggplotGrob)
grid.arrange(grobs = plot1, ncol = 6, nrow = 2)


##### broader periods #####
###add broader periods

ordered_dt_with_zones <- dt_with_zones[order(dt_with_zones$yyyy), ]

breaks <- c(1900, 1930, 1950, 1970, 1990, 2008, 2011)
ordered_dt_with_zones$period <- cut(ordered_dt_with_zones$yyyy, breaks = breaks, labels = c("1900-1929", "1930-1949", "1950-1969", "1970-1989", "1990-2008", "2009-2011"))

periods_to_include <- unique(c(ordered_dt_with_zones$period))
plots_list4 <- list()

for (d in periods_to_include) {
  subset_data <- subset(ordered_dt_with_zones, period %in% d)
  
  p <- ggplot(subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
    stat_density_2d(aes(fill = ..density..), bins = 45, geom = "raster", contour = FALSE, show.legend = FALSE) +
    geom_sf(data = WA_base, inherit.aes = FALSE) +
    labs(title = paste(d), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    xlim(114.9851, 115.8) +
    ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=5) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  plots_list4[[d]] <- p 
  
}

plot2 <- lapply(plots_list4, ggplotGrob)
grid.arrange(grobs = plot2, ncol = 6, nrow = 1)


##### plots with contours ######

###coarsen bathy and make df

bathy_coarse <- aggregate(bathy, fact = 5)
bathy_df_coarse <- as.data.frame(bathy_coarse, xy = TRUE)

names(bathy_df_coarse) <- c("x", "y", "z")

###order data by year 

ordered_data <- data[order(data$yyyy), ]

###for contours

contour_levels <- c(-200, -100, -50, -30, -20, -10)

###add periods

breaks <- c(1900, 1930, 1950, 1970, 1990, 2008, 2011)
ordered_data$period <- cut(ordered_data$yyyy, breaks = breaks, labels = c("1900-1929", "1930-1949", "1950-1969", "1970-1989", "1990-2008", "2009-2011"))

periods_to_include <- unique(c(ordered_data$period))

###generate plot

plots_list5 <- list()

for (d in periods_to_include) {
  subset_data <- subset(ordered_data, period %in% d)
  
  p <-  ggplot() +
    stat_density_2d(data = subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],fill = ..density..), bins = 45, geom = "raster", contour = FALSE, show.legend = FALSE) +
    geom_contour(data = bathy_df_coarse, aes(x=x, y=y, z = z), breaks = contour_levels, colour='white', linewidth = 0.1) +
    geom_text_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z), 
                      breaks = contour_levels, size = 0.8,
                      colour = 'white',
                      label.placer = label_placer_n(1)) +
    geom_sf(data = WA_base, inherit.aes = FALSE) +
    labs(title = paste(d), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    annotate(geom = "text", x = c(115.78,115.85,115.85, 115.85, 115.83, 115.78 ), y = c(-31.5,-31.8, -31.9, -32.06, -32.29, -32.6 ), label = c("Two Rocks","Hillarys","Perth", "Fremantle", "Rockingham", "Mandurah" ), size = 0.8) +
    coord_sf(xlim = c(114.9851, 115.9),
             ylim = c(-32.7966, -31.30936))+
    # xlim(114.9851, 115.8) +
    # ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  plots_list5[[d]] <- p 
  
}

plot3 <- lapply(plots_list5, ggplotGrob)
grid.arrange(grobs = plot3, ncol = 6, nrow = 1)







breaks <- c(1900, 1950, 1965, 1970, 1990, 2005, 2011)
ordered_data$period <- cut(ordered_data$yyyy, breaks = breaks, labels = c("1900-1949 (n=67)", "1950-1964 (n=228)", "1965-1969 (n=136)", "1970-1989 (n=140)", "1990-2004 (n=166)", "2005-2011 (n=83)"))
sum(ordered_data$period=="1900-1949")
sum(ordered_data$period=="1950-1959")
sum(ordered_data$period=="1960-1964")
sum(ordered_data$period=="1965-1969")
sum(ordered_data$period=="1970-1989")
sum(ordered_data$period=="1990-2004")
sum(ordered_data$period=="2005-2011")


periods_to_include <- unique(c(ordered_data$period))

###generate plot

plots_list5 <- list()

for (d in periods_to_include) {
  subset_data <- subset(ordered_data, period %in% d)
  
  p <-  ggplot() +
    stat_density_2d(data = subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],fill = ..density..), bins = 45, geom = "raster", contour = FALSE, show.legend = FALSE) +
    geom_contour(data = bathy_df_coarse, aes(x=x, y=y, z = z), breaks = contour_levels, colour='white', linewidth = 0.1) +
    geom_text_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z), 
                      breaks = contour_levels, size = 0.8,
                      colour = 'white',
                      label.placer = label_placer_n(1)) +
    geom_sf(data = WA_base, inherit.aes = FALSE) +
    labs(title = paste(d), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    annotate(geom = "text", x = c(115.78,115.85,115.85, 115.85, 115.83, 115.78 ), y = c(-31.5,-31.8, -31.9, -32.06, -32.29, -32.6 ), label = c("Two Rocks","Hillarys","Perth", "Fremantle", "Rockingham", "Mandurah" ), size = 0.8) +
    coord_sf(xlim = c(114.9851, 115.9),
             ylim = c(-32.7966, -31.30936))+
    # xlim(114.9851, 115.8) +
    # ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=3) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  plots_list5[[d]] <- p 
  
}

plot3 <- lapply(plots_list5, ggplotGrob)
grid.arrange(grobs = plot3, ncol = 6, nrow = 1)


