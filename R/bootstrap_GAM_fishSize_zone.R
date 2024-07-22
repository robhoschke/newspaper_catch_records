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


n_repeats <- 2

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
      yyyy = as.numeric(yyyy) - 1904,
      iteration = i  # Add iteration number
    ) %>%
    arrange(ID)
  
  # Fit the GAM model
  gam_model <- gam(largest.dhufish.kg ~ s(yyyy, by = Zone, k=4, bs="cr") + s(distance, k=4, bs="cr"),
                   family = gaussian(link = "identity"), data = dt)
  
  gam_summary <- summary(gam_model)
  deviance_explained <- gam_summary$dev.expl
  aic <- AIC(gam_model)
  
  # Extract p-values (Pr(>|t|)) for the smooth terms
  p_values <- gam_summary$s.table[, "p-value"]
  
  # Store the summary data
  gam_summaries[[i]] <- data.frame(
    iteration = i,
    deviance_explained = deviance_explained,
    aic = aic,
    s_yyyy_Rottnest = p_values["s(yyyy):Zonenear_rottnest"],
    s_yyyy_NN = p_values["s(yyyy):Zonenearshore_north"],
    s_yyyy_NS = p_values["s(yyyy):Zonenearshore_south"],
    s_yyyy_ON = p_values["s(yyyy):Zoneoffshore_north"],
    s_yyyy_OS = p_values["s(yyyy):Zoneoffshore_south"],
    s_distance_p_value = p_values["s(distance)"]
  )
  
  # Generate predictions for the current data
  pred_df <- dt %>% 
    group_by(Zone) %>% 
    do({
      newdata <- data.frame(yyyy = seq(min(1), max(110), length.out = 109),
                            distance = mean(.$distance),
                            Zone = unique(.$Zone))
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

##gam summarys
gam_summary_df <- do.call(rbind, gam_summaries)

####bootstrap_GAM_fishsize_Zone
# Combine all predictions into one data frame
combined_preds <- bind_rows(all_preds)
write.csv(combined_preds1, "data/bootstrap_gam_preds_byZone.csv")
combined_preds <- read.csv("data/bootstrap_gam_preds_byZone.csv")
glimpse(combined_preds1)


mean_values_by_zone <- combined_preds %>%
  group_by(yyyy, Zone) %>%
  summarise(
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop') %>% 
  glimpse()


mean_values_by_zone <- mean_values_by_zone %>%
  filter(!(Zone %in% c('nearshore_north', 'offshore_north', 'offshore_south') & yyyy < 50))  ###only model from 1950 onwars in selected zones


p <- ggplot() +
  geom_ribbon(data = mean_values_by_zone, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean,fill="salmon"), alpha = 1) +
  geom_line(data = mean_values_by_zone, aes(x = yyyy, y = fit_mean)) +
  geom_rug(data = dat, aes(x = yyyy - 1904, y = largest.dhufish.kg), position="jitter" , alpha = 0.4, sides="b")+
  scale_x_continuous(breaks = c(-4, 46, 96),
                     labels = c("1900", "1950", "2000")) +
  scale_y_continuous(limits = c(0, 25)) +
  #geom_point(data = dat, aes(x = yyyy - 1904, y = largest.dhufish.kg), size = 0.2) +
  labs(title = "Effect of year by zone with multiple iterations", 
       x = "Year", 
       y = "Dhufish size (kg)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.position = "none") +
  facet_wrap(~Zone, scales = "fixed")

print(p)

