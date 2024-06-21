###
# Project: Historical recreational fishing
# Data:    fishing trip points
# Task:    model distance of catches from central point of population over time
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")

####using centroids####

glimpse(dat)
glimpse(Freo_harbour)

dist <- geosphere::dist2Line(p = st_coordinates(dat$geometry), 
                             line = st_coordinates(Freo_harbour)[,1:2])


# Combine initial data with distance to coastline
dataframe <- cbind(dat, dist) 
plot(distance.1~yyyy, data = dataframe)
mylm <- lm(distance.1~yyyy, data = dataframe)
summary(mylm)

plot(mylm)
plot(gam_model)
plot(exp_model)

dataframe$log_distance <- log(dataframe$distance.1)

# Fit a linear model to the transformed data
exp_model <- lm(log_distance ~ yyyy, data = dataframe)
gam_model <- gam(distance.1 ~ s(yyyy, k=5, bs="cr"),
                 family = gaussian(link = "identity"), data = dataframe)
glm_model <- glm(formula = distance.1 ~ yyyy, family = Gamma, data = dataframe)
plot(glm_model)

residualPlots(glm_model)
gam.check(gam_model)

summary(gam_model)


summary(exp_model)
summary(mylm)
summary(glm_model, dispersion=1)

plot(mylm)
plot(exp_model)

# Add prexp_model# Add predictions to the dataframe (on the log scale)
dataframe$log_predicted_distance <- predict(exp_model, newdata = dataframe)
dataframe$lm_predicted_distance <- predict(mylm, newdata = dataframe)
# Back-transform the predictions to the original scale
dataframe$predicted_distance <- exp(dataframe$log_predicted_distance)

# Create the plot
ggplot(dataframe, aes(x = yyyy, y = distance.1)) +
  geom_point() +  # Plot the actual data points
  geom_line(aes(y = predicted_distance), color = "blue") + 
  geom_line(aes(y = lm_predicted_distance), color = "blue") + 
  # Add the exponential growth curve
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(title = "Exponential Growth Model", x = "Year", y = "Distance")

ggplot(dataframe, aes(x = yyyy, y = distance.1)) +
  geom_point() +  # Plot the actual data points
  geom_smooth(method="gam", col="black")+
  geom_smooth(method="lm", col="black")+
  geom_smooth(method="glm", col="black")+
  # Add the exponential growth curve
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(title = "Exponential Growth Model", x = "Year", y = "Distance")
plot.gam(gam_model)

xseq <- seq(0,110,1)
ypred <- predict(glm_model, list(yyyy=xseq), type="response")
pred <- data.frame(xseq,ypred)

ggplot()+
  geom_point(data=dataframe, aes(x = yyyy, y = distance.1)) +
  geom_line(data=pred, aes(xseq, ypred))

