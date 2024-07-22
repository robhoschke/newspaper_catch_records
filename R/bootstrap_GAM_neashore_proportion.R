###
# Project: Historical recreational fishing
# Data:    fishing trip distance data
# Task:    modelling the proportion of trips taking place in nearshore zone over time
# Author:  Rob
# Date:    June 2024

####bootstrap####
# Define the number of repetitions
#revert to k=4

source("R/data_filtering.R")

n_repeats <- 1000

# Empty lists to store predictions
all_yyyy_predictions <- vector("list", length = n_repeats)
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
      yyyy = as.numeric(yyyy),
      iteration = i  # Add iteration number
    ) %>%
    arrange(ID)
  
  summary_data <- dt %>%
    mutate(depth_category = ifelse(bathy > -20, "Nearshore", "Inshore Demersal")) %>% 
    group_by(yyyy,depth_category) %>%
    summarise(trip_count = n()) %>%
    mutate(proportion = trip_count / sum(trip_count)) %>% 
    ungroup()%>% 
    filter(depth_category=="Nearshore")
  
  gam_model <- gam(proportion ~ s(yyyy, k = 5, bs = "cr"),
                   family = quasibinomial(link = "logit"), 
                   data = summary_data)
  
  gam_summary <- summary(gam_model)
  deviance_explained <- gam_summary$dev.expl
  #aic <- AIC(gam_model)                            ####can't calculate AIC for quasibinomial
  
  
  # Store the summary data
  gam_summaries[[i]] <- data.frame(
    iteration = i,
    deviance_explained = deviance_explained,
    #aic = aic,
    s_yyyy_p_value = gam_summary$s.table[, "p-value"]
  )
  
  # Generate predictions and standard errors for yyyy effect
  yyyy_seq <- seq(1904, 2011, length.out = 108)
  proportion_mean <- mean(summary_data$proportion)
  yyyy_pred <- data.frame(yyyy = yyyy_seq, proportion = proportion_mean)
  yyyy_pred$fit <- predict(gam_model, newdata = yyyy_pred, se.fit = TRUE, type="response")$fit
  yyyy_pred$se <- predict(gam_model, newdata = yyyy_pred, se.fit = TRUE, type = "response")$se.fit
  yyyy_pred$lwr <- yyyy_pred$fit - 1.96 * yyyy_pred$se
  yyyy_pred$upr <- yyyy_pred$fit + 1.96 * yyyy_pred$se
  yyyy_pred$iteration <- i
  all_yyyy_predictions[[i]] <- yyyy_pred
 
}


  yyyy_predictions_df <- bind_rows(all_yyyy_predictions)
  write.csv(yyyy_predictions_df, "outputs/proportion_nearshore_preds.csv", row.names = FALSE)
  
  gam_summary_df <- do.call(rbind, gam_summaries)
  write.csv(gam_summary_df, "data/proportion_gam_summary.csv", row.names = FALSE)
  
  gam_sums <- read.csv("data/proportion_gam_summary.csv")
  
  preds <- read.csv("outputs/proportion_nearshore_preds.csv")
  
  mean_values <- preds %>%
    group_by(yyyy) %>%
    summarise(
      fit_mean = mean(fit),
      lwr_mean = mean(lwr),
      upr_mean = mean(upr),
      .groups = 'drop') %>% 
    glimpse()
  
  ggplot() +
    geom_line(data=mean_values, aes(x=yyyy, y=fit_mean))
  
  