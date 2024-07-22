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
  random_points <- st_sample(polygon$geometry, 1000)
  metadata_df <- data.frame(yyyy = rep(polygon$yyyy, 1000),
                            ID = rep(polygon$ID, 1000),
                            largest.dhufish.kg = rep(polygon$largest.dhufish.kg, 1000),
                            decade = as.factor(rep(polygon$decade, 1000)))
  random_points_with_metadata <- cbind(metadata_df, geometry = random_points)
  all_random_points_with_metadata[[i]] <- random_points_with_metadata
  print(i)
}

trip_points <- do.call(rbind, all_random_points_with_metadata)
glimpse(trip_points)
plot(st_as_sf(trip_points))
#####add zones and bathy#####

df <- trip_points %>%
  st_as_sf() %>%
  st_join(st_as_sf(perth_zones)) %>%
  mutate(Zone = as.factor(Zone)) %>%
  st_crop(bathy) %>%
  mutate(bathy = extract(bathy, .)$bathy_cropped1) %>% 
mutate(
  bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy))   ##resample positive bathy values

glimpse(df)
tail(df)
summary(df$bathy)
plot(largest.dhufish.kg~yyyy, data=df)


##### spatial plots all data #####

# Plot the points
plot(df$geometry, 
     pch = 20, cex = 0.2)

##### spatial plots by decade #####

decades_to_include <- unique(df$decade)

overall_bbox <- st_bbox(df$geometry)

for (i in 1:length(decades_to_include)) {
  # Subset data for the current decade
  subset_data <- subset(df, decade == decades_to_include[i])
  plot(subset_data$geometry, pch = 20, cex = 0.2, col = subset_data$decade,
       main = paste(decades_to_include[i]),
       xlim = c(overall_bbox["xmin"], overall_bbox["xmax"]),
       ylim = c(overall_bbox["ymin"], overall_bbox["ymax"]))
}

##### density plot all decades #####
##different plotting options

ggplot(df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  stat_density_2d(aes(fill = ..level..), bins=200, geom = "polygon") +
  geom_sf(data = WA_base, inherit.aes = FALSE) +
  xlim(114.9851, 115.8) +
  ylim(-32.7966, -31.30936) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() + 
  geom_point(size=0.5) 

ggplot(df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  stat_density_2d(aes(fill = ..density..), bins=20, geom = "raster", contour=FALSE) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() + 
  geom_point(size=0.5) 

ggplot(df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
  geom_bin2d(bins=40) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


##### heat map by decade #####
##sample size##
a <- list()

for (d in decades_to_include) {
  subset_data <- subset(fishing_trips, decade %in% d)
  b <- nrow(subset_data)
  a[[d]] <- b

}
##plot heatmap by decade

##Manually add decades, because previous decades were incorrect

breaks <- c(1900, 1909, 1919, 1929, 1939, 1949, 1959, 1969, 1979, 1989,1999,2009,2011) 
df$Decade <- cut(df$yyyy, breaks = breaks,labels = c("1900-1909","1910-1919","1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-2069","1970-2079","1980-2089","1990-1999", "2000-2009", "2010-2011"))
glimpse(df)

decades_to_include <- unique(c(df$Decade))

plots_list3 <- list()

for (d in decades_to_include) {
  subset_data <- subset(df, Decade %in% d)
  row_count <- nrow(subset_data)
  
  p <- ggplot()+
    stat_density_2d(data=subset_data, 
                    aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2],
                        fill = ..density..), bins = 5, geom = "raster", contour = FALSE, show.legend = FALSE)+
    geom_sf(data = WA_base, inherit.aes = FALSE) +
   # labs(title = paste(d), x = "Longitude", y = "Latitude") +
    labs(title = paste(d, "(n=", row_count / 1000,")"), x = "Longitude", y = "Latitude") +
    paletteer::scale_fill_paletteer_c("viridis::plasma") +
    xlim(114.9851, 115.8) +
    ylim(-32.7966, -31.30936) +
    theme_minimal(base_size=5)
  
  plots_list3[[d]] <- p 
  
}

plot1 <- lapply(plots_list3, ggplotGrob)
grid.arrange(grobs = plot1, ncol = 6, nrow = 2)


##### heatmap by broader milestone years #####


###coarsen bathy for contours and make df

bathy_coarse <- aggregate(bathy, fact = 5)
bathy_df_coarse <- as.data.frame(bathy_coarse, xy = TRUE)

names(bathy_df_coarse) <- c("x", "y", "z")


    contour_levels <- c(-200, -100, -50, -30, -20, -10)
    
    breaks <- c(1900, 1929, 1949, 1969, 1989, 2008, 2011)
    df$period <- cut(df$yyyy, breaks = breaks,labels = c("1900-1929", "1930-1949", "1950-1969", "1970-1989", "1990-2008", "2009-2011"))
    
    
    periods_to_include <- unique(c(df$period))
  
    
##open pdf to write to
    pdf("my_plots4.pdf", width = 10, height = 10) 
    
##generate plot
    plots_list6 <- list()
    
    for (d in periods_to_include) {
      subset_data <- subset(df, period %in% d)
      row_count <- nrow(subset_data)
      
      p <- ggplot() +
        stat_density_2d(data = subset_data, 
                        aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], 
                            fill = after_stat(density)), 
                        bins = 5,                                                     ### not sure if changing anything
                        geom = "raster", contour = FALSE, show.legend = FALSE) +
        geom_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z), 
                     breaks = contour_levels, colour = 'white', linewidth = 0.1) +
        geom_sf(data = WA_base, inherit.aes = FALSE) +
        labs(title = paste0(d," (n=", row_count / 1000,")"), x = "Longitude", y = "Latitude") +
        paletteer::scale_fill_paletteer_c("viridis::plasma") +
        annotate(geom = "text", x = c(115.77, 115.85, 115.85, 115.88, 115.87, 115.78),           ###place names fewer      
                 y = c(-31.5, -31.8, -31.9, -32.06, -32.29, -32.6), 
                 label = c("Two Rocks", "Hillarys", "Perth", "Fremantle", "Rockingham", "Mandurah"), size = 2.7) +
        annotate(geom = "text", x = c(114.99, 115.11, 115.2, 115.45, 115.55, 115.6),           ###contour labels     
                 y = c(-31.55, -31.55, -31.55, -31.55,-31.55,-31.55), 
                 label = c("-200", "-100", "-50", "-30", "-20", "-10"), size = 2, colour= 'white') +
        coord_sf(xlim = c(114.9851, 116.0),
                 ylim = c(-32.7966, -31.30936)) +
        theme_minimal(base_size = 9) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      plots_list6[[d]] <- p 
    }
    
    plot <- lapply(plots_list6, ggplotGrob)
    grid.arrange(grobs = plot, ncol = 3, nrow = 2)
  
    
dev.off()

####another method for density plots####
glimpse(df)
  d <- ggplot(df, (aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])))  
  
 d + stat_density_2d(
   geom = "raster",
   aes(fill = after_stat(density)),
   contour = FALSE) + 
   scale_fill_viridis_c() +
   facet_wrap(vars(decade))
   geom_sf(data = WA_base, inherit.aes = FALSE)+
   coord_sf(xlim = c(114.9851, 115.9),
            ylim = c(-32.7966, -31.30936))
  
  

    