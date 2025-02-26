###
# Project: Historical recreational fishing
# Data:    Fishing trip shapefiles
# Task:    Plotting fishing trip density 
# Author:  Rob
# Date:    April 2024
##

source("R/data_filtering.R")
install.packages("ggsn")
library(prettymapr)

##### random points for each polygon#####             
all_random_points_with_metadata <- list()

for (i in 1:nrow(fishing_trips)) {
  
  polygon <- fishing_trips[i, ]
  random_points <- st_sample(polygon$geometry, 1000)
  metadata_df <- data.frame(yyyy = rep(polygon$yyyy, 1000),
                            mm = rep(polygon$mm, 1000),
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
glimpse(df$geometry)
plot(df$geometry, 
     pch = 20, cex = 0.02)


#######
######
#####
####
###


##### heatmap by milestone years #####

## bathy for contours

bathy_coarse <- aggregate(bathy, fact = 5)
bathy_df_coarse <- as.data.frame(bathy_coarse, xy = TRUE)

names(bathy_df_coarse) <- c("x", "y", "z")


    contour_levels <- c(-200, -100, -50, -30, -20, -10)
    
    
##time period breaks in spatial plots
    
    breaks <- c(1900, 1958, 1965, 1989, 2006, 2011)
    df$period <- cut(df$yyyy, breaks = breaks,labels = c("A: 1900-1958", "B: 1959-1965", "C: 1966-1989", "D: 1990-2006", "E: 2007-2011"))
    periods_to_include <- unique(c(df$period))
    

    plots_list <- list()
    
    for (i in seq_along(periods_to_include)) {
      d <- periods_to_include[i]
      subset_data <- subset(df, period %in% d)
      row_count <- nrow(subset_data)
      
      p <- ggplot() +
        stat_density_2d(data = subset_data, 
                        aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], 
                            fill = after_stat(density)), 
                        bins = 5,
                        geom = "raster", contour = FALSE, show.legend = ifelse(i == 1, TRUE, FALSE)) + 
        geom_contour(data = bathy_df_coarse, aes(x = x, y = y, z = z), 
                     breaks = contour_levels, colour = 'white', size = 0.05) +
        geom_sf(data = WA_base, inherit.aes = FALSE) +
        labs(title = paste0(d, " (n=", row_count / 1000, ")"), x = "Longitude", y = "Latitude") +
        paletteer::scale_fill_paletteer_c("viridis::plasma") +
        annotate(geom = "text", x = c(115.77, 115.85, 115.88, 115.78),             
                 y = c(-31.5, -31.8, -32.06, -32.6), 
                 label = c("Two Rocks", "Hillarys", "Fremantle", "Mandurah"), size = 3.5) +
        annotate(geom = "text", x = c(115.5),             
                 y = c(-31.92), 
                 label = c("Rottnest Island"), size = 3, colour = 'white') +
        annotate(geom = "text", x = c(115.51),             
                 y = c(-32.2), 
                 label = c("Garden Island"), size = 3, colour = 'white') +
        annotate(geom = "text", x = c(115.95),             
                 y = c(-31.9), 
                 label = c("Perth"), size = 4) +
        annotate(geom = "text", x = c(114.99, 115.11, 115.2, 115.45, 115.55, 115.6),              
                 y = c(-31.55, -31.55, -31.55, -31.55, -31.55, -31.55), 
                 label = c("-200", "-100", "-50", "-30", "-20", "-10"), size = 2.5, colour = 'white') +
        coord_sf(xlim = c(114.9851, 116.0),
                 ylim = c(-32.7966, -31.30936)) +
        theme_minimal(base_size = 10) 
      
      
      # Add the scalebar to plot1 only
      if (i == 1) {
        p <- p + 
         
          annotation_scale(location = "bl", text_cex= 0) +
  
          annotate(
            geom = "text", 
            x = c(114.99, 115.08, 115.21, 115.11, 115.11),             
            y = c(-32.78, -32.78, -32.78, -32.485, -32.695), 
            label = c("0", "10", "20 km", "High", "low"), 
            size = 2.7, 
            colour = 'white'
          ) +
          
          annotation_north_arrow(location = "br", which_north = "true", 
                                 pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                                 height = unit(0.8, "cm"), width = unit(0.8, "cm"),
                                 style = north_arrow_orienteering()) +
          theme(
            legend.position = c(0.15, 0.2),         
            legend.key.size = unit(0.3, "cm"),      
            legend.text = element_text(size = 0, colour = "white"),
            legend.title = element_text(size = 10, colour = "white")) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank()) 
      }
      
      plots_list[[d]] <- p
    }
    
    
  plot <- lapply(plots_list, ggplotGrob)
  # grid.arrange(grobs = plot, ncol = 3, nrow = 2)
    
##
####
#####
######
####### plotting australia basemap #######
    
##make basemap

library(cowplot)                 
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)                 
    

xmin <- 114.9
ymin <- -32.9
xmax <- 115.95
ymax <- -31.2

rectangle <- st_polygon(list(rbind(
  c(xmin, ymin), 
  c(xmin, ymax), 
  c(xmax, ymax), 
  c(xmax, ymin), 
  c(xmin, ymin)
)))

rectangle_sf <- st_sfc(rectangle, crs = 4326)  # WGS84 CRS
rectangle_sf <- st_sf(geometry = rectangle_sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

reference_map <- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = rectangle_sf, fill = NA, color = "blue", linewidth = 0.8) +
  coord_sf(xlim = c(110, 129), ylim = c(-12, -38), expand = FALSE) +
  annotate(geom = "text", x = c(122),             
           y = c(-25), 
           label = c("Western Australia"), size = 7) +
  annotate(geom = "text", x = c(117.3),             
           y = c(-31.9), 
           label = c("Perth"), size = 5) +
  
  annotation_scale(location = "bl", style = "bar", text_cex = 0) +
  
  annotate(
    geom = "text", 
    x = c(110.6, 112.6, 114.9),             
    y = c(-36.5, -36.5, -36.5), 
    label = c("0", "200", "400 km"), 
    size = 3) +
   
  
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())

##
###
####
#####
###### Final plot ######

##add basemap to spatial plots

plots_list[["reference_map"]] <- reference_map

plot <- lapply(plots_list, ggplotGrob)
grid_layout <- grid.arrange(grobs = plot, ncol = 3, nrow = 2)

ggsave(
  filename = "grid_layout4.pdf",
  plot = grid_layout,   
  width = 12,           
  height = 12,          
  dpi = 600             
)
    