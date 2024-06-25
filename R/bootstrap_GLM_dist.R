
n_repeats <- 10



fit_glm <- function(data){
  glm(formula = distance ~ yyyy, family = Gamma, data = data)
}


all_predictions <- vector("list", length = n_repeats)



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
  glm_model <- fit_glm(dt)
  
  # Generate predictions and standard errors for yyyy effect
  
  xseq <- seq(0,110,1)
  ypred <- predict(glm_model, list(yyyy=xseq), type="response")
  pred <- data.frame(xseq,ypred)
  pred$iteration <- i
  all_predictions[[i]] <- pred
  
  
}

# Combine the predictions into a single data frame
predictions_df <- bind_rows(all_predictions)

summary(glm_model)
AIC(glm_model)
AIC(gam_model)
gam.check(gam_model)
AIC(mylm)
gam_model

ggplot()+
  geom_line(data = predictions_df, aes(x = xseq, y = ypred, group = iteration, color = as.factor(iteration), alpha = 0.1)) +
  #geom_line(data = yyyy_predictions_df, aes(x = yyyy, y = fit, group = iteration, color = as.factor(iteration), alpha = 0.01)) + ##add line for base model
  #geom_jitter(data = dt, aes(x = yyyy, y = distance), inherit.aes = FALSE, alpha = 0.5)+
  geom_rug(data=dt, aes(x=yyyy, y =distance), alpha =0.5,position="jitter", sides="b")+
  geom_line(data=pred, aes(xseq, ypred))+
  annotate("rect", xmin = 24, xmax= 45, ymin=0, ymax =100000, alpha = 0.5)+
  scale_x_continuous(breaks = c(0, 26, 56, 86),
                     labels = c("1904", "1930", "1960", "1990")) +
  labs( x = "Year",
        y = "Distance from Fremantle") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") 
