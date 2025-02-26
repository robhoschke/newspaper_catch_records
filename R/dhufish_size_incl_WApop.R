###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    modelling fish size, using depth zone as a covariate
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")


##to do:
#save GAM summaries


n_repeats <- 1

# Initialize a list to store the predictions from each iteration
all_preds <- list()
gam_summaries <- list()
overall_mean_pop <- mean(fishing_trips$Pop, na.rm = TRUE)

# Loop over repetitions
for (i in 1:n_repeats) {
  print(i)
  all_r_points_with_metadata <- list()
  
  for (j in 1:nrow(fishing_trips)) {
    polygon <- fishing_trips[j, ]
    r_points <- st_sample(polygon$geometry, 1)  # Sample 1 random point within each polygon
    metadata_df <- data.frame(ID = polygon$ID,
                              yyyy = polygon$yyyy,
                              largest.dhufish.kg = polygon$largest.dhufish.kg,
                              Pop = polygon$Pop)
    r_points_with_metadata <- st_as_sf(cbind(metadata_df, geometry = st_geometry(r_points)))
    all_r_points_with_metadata[[j]] <- r_points_with_metadata
  }
  
  single_trip_points <- do.call(rbind, all_r_points_with_metadata)
  
  dist <- geosphere::dist2Line(p = st_coordinates(single_trip_points$geometry), ##distance from shore
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
  gam_model <- gam(largest.dhufish.kg ~ s(yyyy, by = depth_zone, k=4, bs="cr") + s(distance, k=4, bs="cr") +s(Pop, k=4, bs="cr"),
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
    s_distance_p_value = p_values["s(distance)"],
    s_Pop_p_value = p_values["s(Pop)"],  # Extract p-value for population smooth
    s_yyyyinshore_p_value = p_values["s(yyyy):depth_zoneinshore_demersal"],
    s_yyyynearshore_p_value = p_values["s(yyyy):depth_zonenearshore"]
  )
  
  # Generate predictions for the current data
  pred_df <- dt %>% 
    group_by(depth_zone) %>% 
    do({
      newdata <- data.frame(
        yyyy = seq(min(1), max(110), length.out = 109),
        distance = mean(.$distance),
        depth_zone = unique(.$depth_zone),
        Pop = overall_mean_pop  # Include population in the prediction data
      )
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


gam_summary_df <- do.call(rbind, gam_summaries)
#write.csv(gam_summary_df, "outputs/gam_summary_sizebydepthzone_1000reps.csv") ## run once, saved on 10/10
combined_preds <- bind_rows(all_preds) ## run once
#write.csv(combined_preds, "outputs/preds_sizebydepthzone_1000reps.csv")    ##run once, saved on 30/8
combined_preds <- read.csv("outputs/preds_sizebydepthzone_1000reps.csv")
glimpse(combined_preds)



mean_values_by_zone <- combined_preds %>%
  group_by(yyyy, depth_zone) %>%  # Group by Pop as well
  summarise(
    fit_mean = mean(fit),
    lwr_mean = mean(lwr),
    upr_mean = mean(upr),
    .groups = 'drop') %>% 
  glimpse()


##### plot
####
##


##rename inshore demersal in mean_values_by_zone and dat
cor.test(dt$Pop, dt$yyyy)




ggplot() +
  geom_line(
    data = combined_preds,
    aes(x = yyyy + 1904, y = fit, color = depth_zone),
    size = 1
  ) +
  geom_ribbon(
    data = combined_preds,
    aes(x = yyyy + 1904, ymin = lwr, ymax = upr, fill = depth_zone),
    alpha = 0.2,
    color = NA
  ) +
  labs(
    title = "Predicted Largest Dhufish Weight Over Time by Depth Zone",
    x = "Year",
    y = "Predicted Largest Dhufish Weight (kg)",
    color = "Depth Zone",
    fill = "Depth Zone"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )












