
### distance from fremantle less than or greater than 20km 
##distance.1 = distance from fremantly
##distance = distance from shore
glimpse(dat)



##distance from fremantle##

tim_test <- dat %>%
  mutate(
    distance_zone = factor(ifelse(distance.1 < 20000, "<20km Fremantle", ">20km Fremantle"), levels = c("<20km Fremantle", ">20km Fremantle"))
  ) 

glimpse(tim_test)

ggplot() +
  geom_point(data=tim_test, aes(x=yyyy, y=largest.dhufish.kg, colour = distance_zone))

gam_model <- gam(largest.dhufish.kg ~ s(yyyy, by = distance_zone, k=4, bs="cr"),
                 family = gaussian(link = "identity"), data = tim_test)
summary(gam_model)
plot(gam_model)

library(ggplot2)
library(mgcv)



# Define the new data frame for predictions
new_data <- expand.grid(
  yyyy = seq(min(tim_test$yyyy), max(tim_test$yyyy), length.out = 100),
  distance_zone = unique(tim_test$distance_zone)
)

# Add predictions and standard errors to the new data frame
predictions <- predict(gam_model, newdata = new_data, type = "response", se.fit = TRUE)
new_data$predicted <- predictions$fit
new_data$se <- predictions$se.fit

# Calculate confidence intervals
new_data$lower <- new_data$predicted - 1.96 * new_data$se
new_data$upper <- new_data$predicted + 1.96 * new_data$se

# Plot the results with SE bands
ggplot(new_data, aes(x = yyyy, y = predicted, color = distance_zone, fill = distance_zone)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "Predicted Largest Dhufish (kg)",
    color = "Distance Zone",
    fill = "Distance Zone"
  ) +
  theme_minimal()




##distance from shore##

tim_test1 <- dat %>%
  mutate(
    distance_zone = factor(ifelse(distance < 20000, "<20km shore", ">20km shore"), levels = c("<20km shore", ">20km shore"))
  ) 

glimpse(tim_test1)

ggplot() +
  geom_point(data=tim_test1, aes(x=yyyy, y=largest.dhufish.kg, colour = distance_zone))


gam_model <- gam(largest.dhufish.kg ~ s(yyyy, by = distance_zone, k=4, bs="cr"),
                 family = gaussian(link = "identity"), data = tim_test1)
summary(gam_model)
plot(gam_model)

library(ggplot2)
library(mgcv)

new_data <- expand.grid(
  yyyy = seq(min(tim_test1$yyyy), max(tim_test1$yyyy), length.out = 100),
  distance_zone = unique(tim_test1$distance_zone)
)

# Add predictions and standard errors to the new data frame
predictions <- predict(gam_model, newdata = new_data, type = "response", se.fit = TRUE)
new_data$predicted <- predictions$fit
new_data$se <- predictions$se.fit

# Calculate confidence intervals
new_data$lower <- new_data$predicted - 1.96 * new_data$se
new_data$upper <- new_data$predicted + 1.96 * new_data$se

# Plot the results with SE bands
ggplot(new_data, aes(x = yyyy, y = predicted, color = distance_zone, ymin = 0, fill = distance_zone)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(
    x = "Year",
    y = "Predicted Largest Dhufish (kg)",
    color = "Distance Zone",
    fill = "Distance Zone"
  ) +
  theme_minimal()


