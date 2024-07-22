#####gam bootstrap fishisize with GAM summaries##### (incorporate into GAM fishsize)

# Load necessary libraries
library(mgcv)
library(dplyr)
library(sf)
library(geosphere)


# Number of repeats
n_repeats <- 10  # Set this to your desired number of repeats

# List to store summary data for each iteration
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
  
  dist <- geosphere::dist2Line(p = st_coordinates(single_trip_points$geometry),  ##distance from shore
                               line = st_coordinates(perth_coastline)[, 1:2])
  
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
  gam_model <- gam(largest.dhufish.kg ~ s(yyyy, k = 4, bs = "cr") + s(distance, k = 4, bs = "cr"),
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
    aic = aic,
    s_yyyy_p_value = p_values["s(yyyy)"],
    s_distance_p_value = p_values["s(distance)"]
  )
  
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

# Combine all summary data into a single dataframe
gam_summary_df <- do.call(rbind, gam_summaries)

# View the resulting dataframe
print(gam_summary_df)


yyyy_predictions_df <- bind_rows(all_yyyy_predictions)
distance_predictions_df <- bind_rows(all_distance_predictions)


