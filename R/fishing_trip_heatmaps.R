###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    Plotting fishing trip density 
# Author:  Rob
# Date:    April 2024
##

source("R/data_filtering.R")

##### random points for each polygon#####             ####should I generate 10 random points or a single point for the heatmap??
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
}

trip_points <- do.call(rbind, all_random_points_with_metadata)
glimpse(trip_points)

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
summary(df$bathy)
plot(largest.dhufish.kg~yyyy, data=df)

##### sample of points for heatmaps#####

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


decades_to_include <- unique(c(df$decade))

plots_list3 <- list()

for (d in decades_to_include) {
  subset_data <- subset(df, decade %in% d)
  
  p <- ggplot(subset_data, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2])) +
    stat_density_2d(aes(fill = ..density..), bins = 5, geom = "raster", contour = FALSE, show.legend = FALSE)+
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


breaks <- c(1900, 1930, 1950, 1970, 1990, 2008, 2011)
df$period <- cut(df$yyyy, breaks = breaks, labels = c("1900-1929", "1930-1949", "1950-1969", "1970-1989", "1990-2008", "2009-2011"))

periods_to_include <- unique(c(df$period))
plots_list4 <- list()

for (d in periods_to_include) {
  subset_data <- subset(df, period %in% d)
  
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


    contour_levels <- c(-200, -100, -50, -30, -20, -10)
    
    breaks <- c(1900, 1931, 1950, 1970, 1990, 2008, 2011)
    df$period <- cut(df$yyyy, breaks = breaks, labels = c("1900-1929 (n=65)", "1930-1949 (n=2)", "1950-1969 (n=364)", "1970-1989 (n=140)", "1990-2008 (n=203)", "2009-2011 (n=46)"))

    periods_to_include <- unique(c(df$period))
    
##calculate n for each period##
    row_counts <- df %>%
      group_by(period) %>%
      summarize(count = n())
    
##open pdf to write to
    pdf("my_plots3.pdf", width = 10, height = 10) 
    
##generate plot
    plots_list6 <- list()
    
    for (d in periods_to_include) {
      subset_data <- subset(df, period %in% d)
      
      p <- ggplot() +
        stat_density_2d(data = subset_data, 
                        aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], 
                            fill = after_stat(density)), 
                        bins = 5,                                                     ### not sure if changing anything
                        geom = "raster", contour = FALSE, show.legend = FALSE) +
        geom_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z), 
                     breaks = contour_levels, colour = 'white', linewidth = 0.1) +
        # geom_text_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z),        ##automatic contour labels
        #                   breaks = contour_levels, size = 0.8,
        #                   colour = 'white',
        #                   label.placer = label_placer_n(1)) +
        geom_sf(data = WA_base, inherit.aes = FALSE) +
        labs(title = paste(d), x = "Longitude", y = "Latitude") +
        paletteer::scale_fill_paletteer_c("viridis::plasma") +
        annotate(geom = "text", x = c(115.77, 115.85, 115.85, 115.88, 115.87, 115.78),           ###place names fewer      
                 y = c(-31.5, -31.8, -31.9, -32.06, -32.29, -32.6), 
                 label = c("Two Rocks", "Hillarys", "Perth", "Fremantle", "Rockingham", "Mandurah"), size = 2.5) +
        annotate(geom = "text", x = c(114.99, 115.11, 115.2, 115.45, 115.55, 115.6),           ###contour labels     
                 y = c(-31.55, -31.55, -31.55, -31.55,-31.55,-31.55), 
                 label = c("-200", "-100", "-50", "-30", "-20", "-10"), size = 1.8, colour= 'white') +
        coord_sf(xlim = c(114.9851, 116.0),
                 ylim = c(-32.7966, -31.30936)) +
        theme_minimal(base_size = 7) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      plots_list6[[d]] <- p 
    }
    
    plot4 <- lapply(plots_list6, ggplotGrob)
    grid.arrange(grobs = plot4, ncol = 3, nrow = 2)
  
    
dev.off()

####another method####
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
  
  

    