###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    modelling fish size controlling for distance from shore, and plotting responses with distance held at different constants
# Author:  Rob
# Date:    June 2024

##to do:

#re-run 1000

source("R/data_filtering.R")  


####gam with distance####
#######
############ distance held at mean for plots

n_repeats <- 2

# Empty lists to store predictions
gam_summaries <- list()
all_yyyy_predictions <- vector("list", length = n_repeats)

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
  
  # Fit a GAM model to the data
  
  gam_model <- gam(largest.dhufish.kg ~ ti(yyyy, distance, k=4, bs="cr") + s(yyyy, k=4, bs="cr") + s(distance, k=4, bs="cr"),
                   family = gaussian(link = "identity"), data = dt)
  
  # Extract model summary statistics
  
  gam_summary <- summary(gam_model)
  deviance_explained <- gam_summary$dev.expl
  aic <- AIC(gam_model)

  # Extract p-values (Pr(>|t|)) for the smooth terms
  p_values <- gam_summary$s.table[, "p-value"]

  # Store the summary data
  gam_summaries[[i]] <- data.frame(
    iteration = i,
    deviance_explained = deviance_explained,
    aic = aic
    , s_yyyy_p_value = p_values["s(yyyy)"],
    s_distance_p_value = p_values["s(distance)"],
    s_distancebyyyyy_p_value = p_values["s(yyyy):distance"]
  )

  #Generate predictions and standard errors for yyyy effect

  
    yyyy_seq <- seq(0, 107, length.out = 108)
    
    # Calculate Q1, mean, and Q3 for the distance
    lower_distance <- quantile(dt$distance, 0.05)
    mean_distance <- mean(dt$distance)
    upper_distance <- quantile(dt$distance, 0.95)
    
    # List of distances to loop through
    distances <- list(lower = lower_distance, Mean = mean_distance, upper = upper_distance)
    
    # Loop over the different distances (Q1, mean, Q3)
    for (dist_name in names(distances)) {
      distance_value <- distances[[dist_name]]
      
      # Create the prediction dataframe
      yyyy_pred <- data.frame(yyyy = yyyy_seq, distance = distance_value)
      predictions <- predict(gam_model, newdata = yyyy_pred, se.fit = TRUE)
      yyyy_pred$fit <- predictions$fit
      yyyy_pred$se <- predictions$se.fit
      yyyy_pred$lwr <- yyyy_pred$fit - 1.96 * yyyy_pred$se
      yyyy_pred$upr <- yyyy_pred$fit + 1.96 * yyyy_pred$se
      yyyy_pred$iteration <- i
      yyyy_pred$distance_type <- dist_name  # Add a column to indicate which distance type this is
      
      # Store the predictions in the list
      all_yyyy_predictions[[paste(i, dist_name, sep = "_")]] <- yyyy_pred
    }
  
}

# Combine the predictions into a single data frame
gam_summary_df <- do.call(rbind, gam_summaries)
yyyy_predictions_df <- bind_rows(all_yyyy_predictions)
distance_predictions_df <- bind_rows(all_distance_predictions)

tail(yyyy_predictions_df)
# Save the results to CSV files if needed
write.csv(yyyy_predictions_df, "outputs/yyyy_predictions.csv", row.names = FALSE)
write.csv(distance_predictions_df, "outputs/distance_predictions.csv", row.names = FALSE)

yyyy_predictions_df <- read.csv("outputs/yyyy_predictions.csv")
distance_predictions_df <- read.csv("outputs/distance_predictions.csv")
glimpse(yyyy_predictions_df)


mean_values <- yyyy_predictions_df %>%
  group_by(yyyy, distance_type) %>%                      
  summarise(
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop') %>% 
  glimpse()

mean_values_distance <- distance_predictions_df %>%
  group_by(row_number) %>%
  summarise(
    distance = first(distance),  # Retain the distance column
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop'
  ) %>%
  glimpse()


ggplot() +
  geom_line(data= mean_values, aes(x = yyyy, y = fit_mean, color = distance_type)) +
  geom_ribbon(data=mean_values, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean, fill = distance_type), alpha = 0.2) +
  geom_rug(data = dat, aes(x = yyyy - 1904, y = largest.dhufish.kg), position="jitter" , alpha = 0.4, sides="b") +
  labs(title = "Predicted Dhufish Size Over Time",
       x = "Time (yyyy)",
       y = "Predicted Size",
       color = "Distance constant") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(0, 26, 56, 86),
                     labels = c("1904", "1930", "1960", "1990")) +
  scale_y_continuous(limits = c(0, 24.5)) +
  labs(title = "Predictions for year effect across iterations",
       x = "Year",
       y = "Predicted Largest Dhufish (kg)")+
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2")


mean_only <- subset(mean_values, distance_type == "Mean" )

ggplot() +
  geom_ribbon(data=mean_only, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean), fill = 'salmon') +
  geom_line(data= mean_only, aes(x = yyyy, y = fit_mean)) +
  geom_rug(data = dat, aes(x = yyyy - 1904, y = largest.dhufish.kg), position="jitter" , alpha = 0.4, sides="b") +
  labs(title = "Predicted Dhufish Size Over Time",
       x = "Time (yyyy)",
       y = "Predicted Size",
       color = "Distance constant") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = c(0, 26, 56, 86),
                     labels = c("1904", "1930", "1960", "1990")) +
  scale_y_continuous(limits = c(0, 24)) +
  labs(title = "Predictions for year effect across iterations",
       x = "Year",
       y = "Predicted Largest Dhufish (kg)")+
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2")




