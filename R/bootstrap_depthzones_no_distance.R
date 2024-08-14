###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    modelling fish size by zone
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")


####gam by zone ####
##############
#########
#######
#####

##to do:
#correct years for plots
#run over 1000 iterations


n_repeats <- 1

# Initialize a list to store the predictions from each iteration
all_preds <- list()
gam_summaries <- list()

# Loop over repetitions
for (i in 1:n_repeats) {
  print(i)
  all_r_points_with_metadata <- list()
  
  for (j in 1:nrow(fishing_trips)) {
    polygon <- fishing_trips[j, ]
    r_points <- st_sample(polygon$geometry, 1)  # Sample 1 random point within each polygon
    metadata_df <- data.frame(ID = polygon$ID,
                              yyyy = polygon$yyyy,
                              largest.dhufish.kg = polygon$largest.dhufish.kg)
    r_points_with_metadata <- st_as_sf(cbind(metadata_df, geometry = st_geometry(r_points)))
    all_r_points_with_metadata[[j]] <- r_points_with_metadata
  }
  
  single_trip_points <- do.call(rbind, all_r_points_with_metadata)
  
  dist <- geosphere::dist2Line(p = st_coordinates(single_trip_points$geometry), 
                               line = st_coordinates(perth_coastline)[,1:2])
  
  # Combine initial data with distance to coastline
  dt <- cbind(single_trip_points, dist) %>%
    st_as_sf() %>%
    st_join(st_as_sf(perth_zones)) %>%
    mutate(
      Zone = as.factor(Zone),
      bathy = extract(bathy, .)$bathy_cropped1,
      bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy), # Resample positive bathy values
      depth = bathy * -1,
      depth_zone = as.factor(ifelse(depth <= 20, "nearshore", "inshore_demersal")), # Add depth_zone column
      yyyy = as.numeric(yyyy) - 1904,
      iteration = i  # Add iteration number
    ) %>%
    arrange(ID)
  
  # Fit the GAM model
  gam_model <- gam(largest.dhufish.kg ~ s(yyyy, k=4, bs="cr"),
                   family = gaussian(link = "identity"), data = dt %>% filter(depth_zone=="inshore_demersal"))
  
  # Generate predictions for the current data
  pred_df <- dt %>% 
    group_by(depth_zone) %>% 
    do({
      newdata <- data.frame(yyyy = seq(min(1), max(110), length.out = 109),
                            depth_zone = unique(.$depth_zone))
      pred <- predict(gam_model, newdata = newdata, se.fit = TRUE)
      newdata$fit <- pred$fit
      newdata$se <- pred$se.fit
      newdata$lwr <- newdata$fit - 1.96 * newdata$se
      newdata$upr <- newdata$fit + 1.96 * newdata$se
      newdata$iteration <- i  # Add iteration identifier
      newdata
    })
  
  # Store the predictions in the list
  all_preds[[i]] <- pred_df
}



####bootstrap_GAM_fishsize_Zone
# Combine all predictions into one data frame
combined_preds <- bind_rows(all_preds)
#write.csv(combined_preds, "data/bootstrap_gam_preds_byZone.csv")
#combined_preds <- read.csv("data/bootstrap_gam_preds_byZone.csv")
glimpse(combined_preds)


mean_values_by_zone <- combined_preds %>%
  group_by(yyyy, depth_zone) %>%
  summarise(
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop') %>% 
  glimpse()


#mean_values_by_zone <- mean_values_by_zone %>%
#  filter(!(Zone %in% c('nearshore_north', 'offshore_north', 'offshore_south') & yyyy < 50))  ###only model from 1950 onwars in selected zones

subset_nearshore <- subset(dt, depth_zone=="nearshore") 



ggplot() +
  geom_ribbon(data = mean_values_by_zone, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean), fill = "coral", alpha = 0.4) +
  geom_line(data = mean_values_by_zone, aes(x = yyyy, y = fit_mean), col= "coral") +
  scale_x_continuous(breaks = c(-4, 46, 96),
                     labels = c("1900", "1950", "2000")) +
  scale_y_continuous(limits = c(5, 20)) +
  labs(title = "Effect of year by zone with multiple iterations", 
       x = "Year", 
       y = "Dhufish size (kg)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") 
#facet_grid(~depth_zone, scales = "fixed")

