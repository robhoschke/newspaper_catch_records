###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    generate bootsrap liear models and store coefficients
# Author:  Rob
# Date:    April 2024


source("R/data_filtering.R")


#####model bootsrap#####

library(sf)
library(dplyr)
library(devtools)

fishing_trips
glimpse(fishing_trips)
str(fishing_trips)


# Define the number of repetitions
n_repeats <- 1000

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
  
  # Fit a linear model to the data
  lm_model <- lm(formula = largest.dhufish.kg ~  yyyy*Zone, data = dt)     ######original model Zone*yyyy
  
  # Store the coefficients of the model
  all_lm_coefs[[i]] <- coef(lm_model)
}


# Convert coefficients to a data frame
coefficients_df <- do.call(rbind, all_lm_coefs)
write.csv (coefficients_df, "outputs/bootstap_coeffs.csv")


coefficients_df <- read.csv("outputs/bootstap_coeffs.csv") #### this seems to generate a different 'intercept' column heading from the initial output 
head(coefficients_df)
summary(lm_model)

coef_means <- colMeans(do.call(rbind, all_lm_coefs))
coef_sds <- apply(do.call(rbind, all_lm_coefs), 2, sd)
coef_lowers <- apply(do.call(rbind, all_lm_coefs), 2, quantile, probs = 0.025)
coef_uppers <- apply(do.call(rbind, all_lm_coefs), 2, quantile, probs = 0.975)

# Create a table of coefficients
coef_table <- data.frame(
  Mean = coef_means,
  SD = coef_sds,
  Lower = coef_lowers,
  Upper = coef_uppers
)

# Print the table
print(coef_table)

