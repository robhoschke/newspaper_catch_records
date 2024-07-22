####nearshore trend
nearshore_points <- filter(df, bathy>-20)
very_nearshore_points <- filter(df, bathy>-10)


filter(df, bathy>-10)
filter(dat, bathy>-20)
102291/820000
257277/820000
182/820
361/820

nearshore_points <- filter(df, bathy>-20)
yearly_counts <- nearshore_points %>%
  group_by(yyyy) %>%
  summarise(count = n())

# Create the plot
ggplot(yearly_counts, aes(x = yyyy, y = count)) +
  geom_col() +
  geom_smooth(method = "gam")+
  labs(title = "Count of Points per Year",
       x = "Year",
       y = "Count of Points") +
  theme_minimal()

###Random sample of points, filter for nearshore depths, plot number of trips over time, repeat yearly_counts <- nearshore_points %>%

####centroid data####
# Prepare the data
dat <- dat %>%
  mutate(depth_category = ifelse(bathy > -20, "Shallow", "Deep"))

# Summarize the data by year and depth category
summary_data <- dat %>%
  group_by(yyyy, depth_category) %>%
  summarise(trip_count = n()) %>%
  ungroup()


ggplot(summary_data, aes(x = yyyy, y = trip_count, fill = depth_category)) +
  geom_col() +
  scale_x_continuous(breaks = seq(min(summary_data$yyyy), max(summary_data$yyyy), by = 10)) +
  labs(
    x = "Year",
    y = "Number of Trips",
    fill = "Depth Category",
    title = "Number of Fishing Trips by Year and Depth Category"
  ) +
  theme_minimal()

####bootstrap####
# Define the number of repetitions

n_repeats <- 2 # Adjust the number of repeats as needed

# List to store the results of each iteration
all_iterations <- list()

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
  
  # Store the results of this iteration
  all_iterations[[i]] <- dt
}

# Combine all iterations into a single data frame
combined_data <- do.call(rbind, all_iterations)
write.csv(combined_data, "data/nearshore_vs_inshore")
glimpse(combined_data)
####plot average number of trips over iterations####

combined_data <- combined_data %>%
  mutate(depth_category = ifelse(bathy > -20, "Nearshore", "Inshore Demersal")) 

summary_data <- combined_data %>%
  group_by(yyyy, depth_category, iteration) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%
  group_by(yyyy, depth_category) %>%
  summarise(avg_trip_count = mean(trip_count), .groups = 'drop')

glimpse(summary_data)

# Plotting
plot1 <- ggplot(summary_data, aes(x = yyyy, y = avg_trip_count, fill = depth_category)) +
  geom_col() +
  geom_smooth(data=summary_data1, aes(x=yyyy, y=proportion_shallow*40), method="gam", linewidth=1, col='darkgrey', fill='black', alpha = 0.1)+
  scale_x_continuous(breaks = seq(min("1900"), max(summary_data$yyyy), by = 10)) +
  labs(
    x = "Year",
    y = "Number of trips",
    fill = "Depth category",
    title = "Number of fishing trips by year and depth category"
  ) +
theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

plot1+ scale_y_continuous(
  
  # Add a second axis and specify its features
  sec.axis = sec_axis(~.*(2.5/100), name ="proportion nearshore"))
  

####add error bars####


glimpse(summary_data)

# Plotting
ggplot(summary_data, aes(x = yyyy, y = avg_trip_count, fill = depth_category)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = avg_trip_count - sd_trip_count, ymax = avg_trip_count + sd_trip_count),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  scale_x_continuous(breaks = seq(min(summary_data$yyyy), max(summary_data$yyyy), by = 10)) +
  labs(
    x = "Year",
    y = "Number of trips",
    fill = "Depth category",
    title = "Number of fishing trips by year and depth category"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )



####add proportion model to column plot####
##########
##############
####################

#single iteration:

# Create depth category
dat <- dat %>%
  mutate(depth_category = ifelse(bathy > -20, "Shallow", "Deep"))

# Summarize the data by year and depth category
summary_data1 <- dat %>%
  group_by(yyyy, depth_category) %>%
  summarise(trip_count = n()) %>%
  ungroup()

glimpse(summary_data1)

# Calculate the total trips per year
total_trips_per_year <- summary_data1 %>%
  group_by(yyyy) %>%
  summarise(total_trips = sum(trip_count)) %>%
  ungroup()

total_trips_per_year <- as.data.frame(total_trips_per_year)
summary_data1 <- as.data.frame(summary_data1) %>%
  left_join(total_trips_per_year, by = "yyyy")

# Calculate proportion of shallow trips
summary_data1 <- summary_data1 %>%
  mutate(proportion_shallow = ifelse(depth_category == "Shallow", trip_count / total_trips, NA)) %>%
  filter(!is.na(proportion_shallow))


# Plotting
plot1 <- ggplot()+
  geom_col(data=summary_data, aes(x = yyyy, y = avg_trip_count, fill = depth_category)) +
  geom_line(data = plot_data, aes(x = yyyy, y = fit*40), color = 'darkgray', size = 1) +
  geom_ribbon(data = plot_data, aes(x = yyyy, ymin = lower*40, ymax = upper*40), alpha = 0.2, fill = 'darkgray') +
   scale_x_continuous(breaks = seq(min("1900"), max(summary_data$yyyy), by = 10)) +
  labs(
    x = "Year",
    y = "Number of trips",
    fill = "Depth category",
    title = "Number of fishing trips by year and depth category"
  ) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

plot1 + scale_y_continuous(
  sec.axis = sec_axis(~.*(2.5/100), name ="proportion nearshore"))
  
####proportion gam####

tst<-  gam(proportion_shallow ~ s(yyyy, k=4, bs="cr"),
           family = gaussian(link = "identity"),  data = summary_data1)

new_data <- data.frame(yyyy = seq(min(summary_data1$yyyy), max(summary_data1$yyyy), length.out = 100))

# Generate predictions with standard errors
predictions <- predict(tst, new_data, se.fit = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(
  yyyy = new_data$yyyy,
  fit = predictions$fit,
  upper = predictions$fit + 1.96 * predictions$se.fit,
  lower = predictions$fit - 1.96 * predictions$se.fit
)

# Create the ggplot
ggplot()+
  geom_point(data=summary_data1, aes(x = yyyy, y = proportion_shallow)) +
  geom_point(color = 'blue') +
  geom_line(data = plot_data, aes(x = yyyy, y = fit), color = 'orange', size = 1) +
  geom_ribbon(data = plot_data, aes(x = yyyy, ymin = lower, ymax = upper), alpha = 0.2, fill = 'orange') +
  labs(x = "Year", y = "Proportion Shallow", title = "GAM Predictions with Confidence Intervals") +
  theme_minimal()

####proportion gam 1000 interations####
#add to col plot ################################### 


summary_data <- combined_data %>%
  group_by(yyyy, depth_category, iteration) %>%
  summarise(trip_count = n(), .groups = 'drop') %>%    ##### this didn't create trip_count column? 
  group_by(yyyy, depth_category) %>%
  summarise(avg_trip_count = mean(trip_count), .groups = 'drop')

# Calculate the total trips per year
total_trips_per_year <- summary_data %>%
  group_by(yyyy) %>%
  summarise(total_trips = sum(trip_count)) %>%
  ungroup()

total_trips_per_year <- as.data.frame(total_trips_per_year)
summary_data1 <- as.data.frame(summary_data1) %>%
  left_join(total_trips_per_year, by = "yyyy")

# Calculate proportion of shallow trips
summary_data <- summary_data %>%
  mutate(proportion_shallow = ifelse(depth_category == "Shallow", trip_count / total_trips, NA)) %>%
  filter(!is.na(proportion_shallow))




