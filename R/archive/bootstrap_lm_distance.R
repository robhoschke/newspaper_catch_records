###
# Project: Historical recreational fishing
# Data:    fishing trip points
# Task:    model distance of catches from central point of population over time
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")

# Define the number of repetitions
n_repeats <- 10

# empty list to store model coefficients
all_lm_coefs <- vector("list", length = n_repeats)

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
  
  # Fit a linear model to the data
  lm_model <- lm(formula = distance ~  yyyy, data = dt)     ######original model Zone*yyyy
  
  # Store the coefficients of the model
  all_lm_coefs[[i]] <- coef(lm_model)
}


# Convert coefficients to a data frame
coefficients_df_distance <- do.call(rbind, all_lm_coefs)
write.csv (coefficients_df_distance, "outputs/bootstap_coeffs.csv")

intercepts <- (coefficients_df_distance[, "(Intercept)"])
slopes <- (coefficients_df_distance[, "yyyy"]) 



# Create a dataframe with intercepts and slopes
coefficients_df <- data.frame(intercept = intercepts, slope = slopes)

# Plot using ggplot2
ggplot(dataframe, aes(x = yyyy, y = distance.1)) +
  geom_point() +  # Plot the actual data points
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(title = "Linear Regression Model with Bootstrapped Coefficients", x = "Year", y = "Distance") +
  # Add the lines based on bootstrapped coefficients
  geom_abline(data = coefficients_df, aes(intercept = intercept, slope = slope), color = rgb(1, 0, 0, 1))






