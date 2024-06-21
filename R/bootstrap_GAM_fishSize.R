###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    resampling spatial data for bootstrap
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")


####gam with distance####

n_repeats <- 1000

# Empty lists to store predictions
all_yyyy_predictions <- vector("list", length = n_repeats)
all_distance_predictions <- vector("list", length = n_repeats)
all_zone_predictions <- vector("list", length = n_repeats)

# Function to fit the GAM model
fit_gam <- function(data) {
  gam(largest.dhufish.kg ~ s(yyyy, k=4, bs="cr") + s(distance, k=4, bs="cr"),
      family = gaussian(link = "identity"), data = data)
}

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
  gam_model <- fit_gam(dt)
  
  # Generate predictions and standard errors for yyyy effect
  yyyy_seq <- seq(0, 107, length.out = 108)
  distance_mean <- mean(dt$distance)
  yyyy_pred <- data.frame(yyyy = yyyy_seq, distance = distance_mean)
  yyyy_pred$fit <- predict(gam_model, newdata = yyyy_pred, se.fit = TRUE)$fit
  yyyy_pred$se <- predict(gam_model, newdata = yyyy_pred, se.fit = TRUE)$se.fit
  yyyy_pred$lwr <- yyyy_pred$fit - 1.96 * yyyy_pred$se
  yyyy_pred$upr <- yyyy_pred$fit + 1.96 * yyyy_pred$se
  yyyy_pred$iteration <- i
  all_yyyy_predictions[[i]] <- yyyy_pred
  
  # Generate predictions and standard errors for distance effect
  distance_seq <- seq(min(dt$distance), max(dt$distance), length.out = 100)
  yyyy_mean <- mean(dt$yyyy)
  distance_pred <- data.frame(yyyy = yyyy_mean, distance = distance_seq)
  distance_pred$fit <- predict(gam_model, newdata = distance_pred, se.fit = TRUE)$fit
  distance_pred$se <- predict(gam_model, newdata = distance_pred, se.fit = TRUE)$se.fit
  distance_pred$lwr <- distance_pred$fit - 1.96 * distance_pred$se
  distance_pred$upr <- distance_pred$fit + 1.96 * distance_pred$se
  distance_pred$iteration <- i
  all_distance_predictions[[i]] <- distance_pred
  
}

# Combine the predictions into a single data frame
yyyy_predictions_df <- bind_rows(all_yyyy_predictions)
distance_predictions_df <- bind_rows(all_distance_predictions)

# Save the results to CSV files if needed
write.csv(yyyy_predictions_df, "outputs/yyyy_predictions.csv", row.names = FALSE)
write.csv(distance_predictions_df, "outputs/distance_predictions.csv", row.names = FALSE)

yyyy_predictions_df <- read.csv("outputs/yyyy_predictions.csv")
distance_predictions_df <- read.csv("outputs/distance_predictions.csv")

# Plotting the results for yyyy effect
ggplot()+
  geom_line(data = yyyy_predictions_df, aes(x = yyyy, y = fit, group = iteration, color = as.factor(iteration), alpha = 0.01)) +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg), inherit.aes = FALSE, alpha = 0.5)+
  annotate("rect", xmin = 26, xmax = 41, ymin = 0, ymax = 35, alpha = .5) +
  scale_x_continuous(breaks = c(0, 26, 56, 86),
                     labels = c("1904", "1930", "1960", "1990")) +
  labs(title = "Predictions for year effect across iterations",
       x = "Year",
       y = "Predicted Largest Dhufish (kg)") +
  theme(legend.position = "none")

# Plotting the results for distance effect
ggplot()+
  geom_line(data=distance_predictions_df, aes(x = distance, y = fit, group = iteration, color = as.factor(iteration), alpha = 0.05)) +
  geom_point(data = dat, aes(x = distance, y = largest.dhufish.kg), inherit.aes = FALSE, alpha = 0.5) +
  labs(title = "Predictions for Distance Effect Across Iterations",
       x = "Distance",
       y = "Predicted Largest Dhufish (kg)") +
  theme(legend.position = "none")

#####
#######
#########
##############
####gam by zone ####
##############
#########
#######
#####


n_repeats <- 1000

# Initialize a list to store the predictions from each iteration
all_preds <- list()

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
  tst <- gam(largest.dhufish.kg ~ s(yyyy, by = Zone, k=5, bs="cr") + s(distance, k=5, bs="cr"),
             family = gaussian(link = "identity"), data = dt)
  
  # Generate predictions for the current data
  pred_df <- dt %>% 
    group_by(Zone) %>% 
    do({
      newdata <- data.frame(yyyy = seq(min(.$yyyy), max(.$yyyy), length.out = 100),
                            distance = mean(.$distance),
                            Zone = unique(.$Zone))
      pred <- predict(tst, newdata = newdata, se.fit = TRUE)
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

# Combine all predictions into one data frame
combined_preds <- bind_rows(all_preds)
write.csv(combined_preds, "data/bootstrap_gam_preds_byZone.csv")
combined_preds <- read.csv("data/bootstrap_gam_preds_byZone.csv")


# data for rectangles
rect_data <- data.frame(
  xmin = c(20, 0, 20, 0,0), 
  xmax = c(45, 45, 45,50,50),
  ymin = c(0, 0, 0,0,0),
  ymax = c(35, 35, 35,35,35),
  Zone = c("near_rottnest", "nearshore_north", "nearshore_south", "offshore_north", "offshore_south")
)

p <- ggplot() +
  geom_line(data = combined_preds, aes(x = yyyy, y = fit, color = factor(iteration)), alpha=0.02) +
  #geom_ribbon(data = combined_preds, aes(x = yyyy, y = fit, ymin = lwr, ymax = upr, fill = factor(iteration)), alpha = 0.2) +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg), size = 0.2) +
  labs(title = "Effect of year by zone with multiple iterations", x = "yyyy", y = "largest.dhufish.kg") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  facet_wrap(~Zone, scales = "fixed")+
  geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.8, inherit.aes = FALSE)

print(p)
