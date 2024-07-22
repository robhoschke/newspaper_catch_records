###
# Project: Historical recreational fishing
# Data:    fishing trip distance data
# Task:    modelling distance of catches from fremantle over time
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")

n_repeats <- 1000

# Empty lists to store predictions
all_yyyy_predictions <- vector("list", length = n_repeats)
gam_summaries <- list()

# Function to fit the GAM model

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
  gam_model <- gam(distance ~ s(yyyy, k=5, bs="cr"),
        family = gaussian(link = "identity"), data = dt)
  
  summary(gam_model)
  # Extract model summary statistics
  gam_summary <- summary(gam_model)
  deviance_explained <- gam_summary$dev.expl
  aic <- AIC(gam_model)
  
  
  # Store the summary data
  gam_summaries[[i]] <- data.frame(
    iteration = i,
    deviance_explained = deviance_explained,
    aic = aic,
    s_yyyy_p_value = gam_summary$s.table[, "p-value"]
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
  
}

gam_summary_df <- do.call(rbind, gam_summaries)
write.csv(gam_summary_df, "data/dist_gam_summary.csv", row.names = FALSE)

yyyy_predictions_df <- bind_rows(all_yyyy_predictions)
write.csv(yyyy_predictions_df, "data/dist_preds.csv", row.names = FALSE)

dist_preds <- read.csv("data/dist_preds.csv")
glimpse(dist_preds)

mean_values <- dist_preds %>%
  group_by(yyyy) %>%
  summarise(
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop') %>% 
  glimpse()


ggplot() +
  geom_ribbon(data = mean_values, aes(x = yyyy+1904, ymin = lwr_mean, ymax = upr_mean,fill="salmon"), alpha = 1)+
  geom_line(data = mean_values, aes(x = yyyy+1904, y = fit_mean)) +
  geom_rug(data = dat, aes(x = yyyy, y = distance.1), position="jitter" , alpha = 0.4, sides="b")+
  # geom_point(data = dat, aes(x = yyyy, y = distance.1), inherit.aes = FALSE, alpha = 0.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  scale_y_continuous(labels = function(y) y / 1000, limits=c(0,65000)) +
  labs(title = "Predictions for distance effect across years",
       x = "Year",
       y = "Distance from Fremantle (km)") 


dist_gam_summary <- read.csv("data/dist_gam_summary.csv")
mean(dist_gam_summary$deviance_explained)



