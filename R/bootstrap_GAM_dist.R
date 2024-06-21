###
# Project: Historical recreational fishing
# Data:    fishing trip distance data
# Task:    resampling spatial data for bootstrap against year
# Author:  Rob
# Date:    June 2024

n_repeats <- 1000

# Empty lists to store predictions
all_yyyy_predictions <- vector("list", length = n_repeats)

# Function to fit the GAM model
fit_gam <- function(data) {
  gam(distance ~ s(yyyy, k=5, bs="cr"),
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
                               line = st_coordinates(Freo_harbour)[,1:2])
  
  # Combine initial data with distance to coastline
  dt <- cbind(single_trip_points, dist) %>%
    st_as_sf() %>%
    st_join(st_as_sf(perth_zones)) %>%
    mutate(
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
  
  
}

# Combine the predictions into a single data frame
yyyy_predictions_df <- bind_rows(all_yyyy_predictions)

# Save the results to CSV files if needed
write.csv(yyyy_predictions_df, "outputs/yyyy_predictions_dist.csv", row.names = FALSE)



ggplot()+
  geom_line(data = yyyy_predictions_df, aes(x = yyyy, y = fit, group = iteration, color = as.factor(iteration), alpha = 0.01)) +
  #geom_line(data = yyyy_predictions_df, aes(x = yyyy, y = fit, group = iteration, color = as.factor(iteration), alpha = 0.01)) + ##add line for base model
  geom_point(data = dt, aes(x = yyyy, y = distance), inherit.aes = FALSE, alpha = 0.5)+
  annotate("rect", xmin = 20, xmax= 45, ymin=0, ymax =100000, alpha = 0.5)+
  scale_x_continuous(breaks = c(0, 26, 56, 86),
                     labels = c("1904", "1930", "1960", "1990")) +
  labs( x = "Year",
       y = "Distance from Fremantle") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") 

