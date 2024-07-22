library(dplyr)
library(ggplot2)

# Create 5-year bins
df <- df %>%
  mutate(year_bin = cut(yyyy, breaks = seq(floor(min(yyyy)/5)*5, ceiling(max(yyyy)/5)*5, by = 5), right = FALSE)) %>%
  mutate(depth_category = ifelse(bathy > -20, "Nearshore", "Inshore Demersal")) 


summary_data <- summary_data %>%
  mutate(year_bin = floor(yyyy / 5) * 5)
tail(summary_data)

# Aggregate data by 5-year bins and depth category
summary_data_binned <- summary_data %>%
  group_by(year_bin, depth_category) %>%
  summarize(avg_trip_count = mean(avg_trip_count, na.rm = TRUE))

# Fit the model (if not already done)
tst <- gam(proportion_shallow ~ s(yyyy, k=4, bs="cr"),
           family = gaussian(link = "identity"), data = summary_data)

# Generate a sequence of 'yyyy' values over which to predict
new_data <- data.frame(yyyy = seq(min(summary_data$yyyy), max(summary_data$yyyy), length.out = 100))

# Generate predictions with standard errors
predictions <- predict(tst, new_data, se.fit = TRUE)

# Create a data frame for plotting
plot_data <- data.frame(
  yyyy = new_data$yyyy,
  fit = predictions$fit,
  upper = predictions$fit + 1.96 * predictions$se.fit,
  lower = predictions$fit - 1.96 * predictions$se.fit
)

# Create the ggplot with 5-year bins
plot1 <- ggplot() +
  geom_col(data = summary_data_binned, aes(x = year_bin, y = avg_trip_count, fill = depth_category)) +
  geom_line(data = plot_data, aes(x = yyyy, y = fit * 40), color = 'black', size = 0.8, alpha = 0.7) +
  geom_ribbon(data = plot_data, aes(x = yyyy, ymin = lower * 40, ymax = upper * 40), alpha = 0.2, fill = 'darkgray') +
  scale_x_continuous(breaks = seq(min(summary_data$yyyy), max(summary_data$yyyy), by = 10)) +
  labs(
    x = "Year",
    y = "Number of trips",
    fill = "Depth category",
    title = "Number of fishing trips by year and depth category"
  ) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

plot1 + scale_y_continuous(
  sec.axis = sec_axis(~.*(2.5/100), name = "Proportion Nearshore")
)
