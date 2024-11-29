###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    modelling fish size, using depth zone as a covariate
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")


##to do:
#save GAM summaries


n_repeats <- 1000

# Initialize a list to store the predictions from each iteration
all_preds <- list()
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
  gam_model <- gam(largest.dhufish.kg ~ s(yyyy, by = depth_zone, k=4, bs="cr") + s(distance, k=4, bs="cr"),
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
    s_yyyyinshore_p_value = p_values["s(yyyy):depth_zoneinshore_demersal"],
    s_yyyynearshore_p_value = p_values["s(yyyy):depth_zonenearshore"]
  )

  # Generate predictions for the current data
  pred_df <- dt %>% 
    group_by(depth_zone) %>% 
    do({
      newdata <- data.frame(yyyy = seq(min(1), max(110), length.out = 109),
                            distance = mean(.$distance),
                            depth_zone = unique(.$depth_zone))
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

mean(gam_summary_df$deviance_explained)

mean_values_by_zone <- combined_preds %>%
  group_by(yyyy, depth_zone) %>%
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


mean_values_by_zone <- mean_values_by_zone %>%
  mutate(depth_zone = fct_recode(depth_zone, offshore = "inshore_demersal"))

dat1 <- dat %>%
  mutate(depth_zone = fct_recode(depth_zone, offshore = "inshore_demersal"))

## subset for depth zones for rug plot

sub1 <- subset(dat1, depth_zone == "nearshore")
sub2 <- subset(dat1, depth_zone == "offshore")


ggplot() +
  geom_line(data = mean_values_by_zone, aes(x = yyyy, y = fit_mean, colour = depth_zone)) +
  geom_ribbon(data = mean_values_by_zone, aes(x = yyyy, ymin = lwr_mean, ymax = upr_mean, fill = depth_zone), alpha = 0.4) +
  scale_fill_manual(values = c("nearshore" = "skyblue", "offshore" = "salmon"),
                    labels = c("offshore (20-250m)","nearshore (0-20m)")) +
  scale_colour_manual(values = c("nearshore" = "skyblue","offshore" = "salmon"), 
                      labels = c( "offshore (20-250m)","nearshore (0-20m)")) +
  # geom_rug(data = dat, aes(x = yyyy-1904, y = largest.dhufish.kg, colour = depth_zone),
  #          position = "jitter", alpha = 0.4, sides = "b") +
  geom_segment(data = sub2,
    aes(
      x = yyyy-1904,
      y    = -1,
      yend = -0.5
    ), colour = "salmon",
    position = position_jitter(width = 0.5, height = 0), alpha = 0.4) +
  geom_segment(data = sub1,
               aes(
                 x = yyyy-1904,
                 y    = -0.5,
                 yend = 0
               ), colour = "skyblue",
               position = position_jitter(width = 0.5, height = 0), alpha = 0.4) +
  scale_x_continuous(breaks = c(-4, 46, 96),
                     labels = c("1900", "1950", "2000")) +
  scale_y_continuous(limits = c(-1, 20), expand = c(0,0)) +
  labs(
    x = "Year", 
    y = "Dhufish size (kg)",
    colour = "Depth zone",
    fill = "Depth zone") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(0.8,0.2),
  
text = element_text(size = 14),  # Default text size for all elements
axis.title = element_text(size = 16),  # Axis titles
axis.text = element_text(size = 12),  # Axis labels
legend.title = element_text(size = 14),  # Legend title
legend.text = element_text(size = 12)  # Legend labels
) + 
  
  guides(
    fill = guide_legend(reverse = TRUE),
    colour = guide_legend(reverse = TRUE)
  )




