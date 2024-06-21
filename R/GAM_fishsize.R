###
# Project: Historical recreational fishing
# Data:    dhufish size data with centroids as point data
# Task:    GAM fishsize
# Author:  Rob
# Date:    June 2024

source("R/data_filtering.R")
library(mgcv)

####linear model plot####

# Plot the original data
plot(largest.dhufish.kg ~ yyyy, data = dat, xlim = c(0, 111), ylim = c(0, 30), pch = 20, 
     xlab = "Year", ylab = "Dhufish Size (kg)", xaxt = "n", yaxt = "n", bty = "n", xaxs = "i", yaxs = "i")
axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8, lwd = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 1)

# yyyy <- seq(1900:2010)
x_pred <- seq(0, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred,
                                               yyyy_sq = x_pred^2, 
                                               mm = as.factor(12)))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)


####gam####

# subset_dat <- subset(dat, yyyy>46)    ###test below with large period of missing data removed


# Fit the linear model for comparison
lm1 <- lm(largest.dhufish.kg ~ yyyy + distance*yyyy, data = dat)

tst<-  gam(largest.dhufish.kg ~ s(yyyy, k=5, bs="cr"),
           family = gaussian(link = "identity"),  data = dat)

tst<-  gam(largest.dhufish.kg ~ s(yyyy, k=4, bs="cr") + s(distance, k=4, bs="cr"),
           family = gaussian(link = "identity"),  data = dat)


anova(lm1,tst, test ="F")

summary(tst)
gam.check(tst)
plot(tst, residuals=TRUE)


# Generate predictions and standard errors for yyyy effect
# yyyy_seq <- seq(min(subset_dat$yyyy), max(subset_dat$yyyy), length.out = 60)   ##starting from 1950
yyyy_seq <- seq(min(dat$yyyy), max(dat$yyyy), length.out = 100)
distance_mean <- mean(dat$distance)
yyyy_pred <- data.frame(yyyy = yyyy_seq, distance = distance_mean)
yyyy_pred$fit <- predict(tst, newdata = yyyy_pred, se.fit = TRUE)$fit
yyyy_pred$se <- predict(tst, newdata = yyyy_pred, se.fit = TRUE)$se.fit
yyyy_pred$lwr <- yyyy_pred$fit - 1.96 * yyyy_pred$se
yyyy_pred$upr <- yyyy_pred$fit + 1.96 * yyyy_pred$se

# Generate predictions and standard errors for distance effect
distance_seq <- seq(min(dat$distance), max(dat$distance), length.out = 100)
yyyy_mean <- mean(dat$yyyy)
distance_pred <- data.frame(yyyy = yyyy_mean, distance = distance_seq)
distance_pred$fit <- predict(tst, newdata = distance_pred, se.fit = TRUE)$fit
distance_pred$se <- predict(tst, newdata = distance_pred, se.fit = TRUE)$se.fit
distance_pred$lwr <- distance_pred$fit - 1.96 * distance_pred$se
distance_pred$upr <- distance_pred$fit + 1.96 * distance_pred$se

# Plot the yyyy effect
p1 <- ggplot() +
  geom_line(data = yyyy_pred, aes(x = yyyy, y = fit), color = "blue") +
  geom_ribbon(data = yyyy_pred, aes(x = yyyy, ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg)) +
  labs(title = "Effect of year", x = "year", y = "largest.dhufish.kg") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Plot the distance effect
p2 <- ggplot() +
  geom_line(data = distance_pred, aes(x = distance, y = fit), color = "blue") +
  geom_ribbon(data = distance_pred, aes(x = distance, ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_point(data = dat, aes(x = distance, y = largest.dhufish.kg)) +
  labs(title = "Effect of distance", x = "distance from shore", y = "largest.dhufish.kg") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#########
###########
####gam by zone####
tst<-  gam(largest.dhufish.kg ~ s(yyyy, by = Zone, k=4, bs="cr") + s(distance, k=4, bs="cr"),
           family = gaussian(link = "identity"),  data = dat)



# Generate predictions and standard errors for each zone
pred_df <- dat %>% 
  group_by(Zone) %>% 
  do({
    newdata <- data.frame(yyyy = seq(min(.$yyyy), max(.$yyyy), length.out = 100),
                          distance = mean(.$distance),
                          Zone = unique(.$Zone))
    pred <- predict(tst, newdata = newdata, se.fit = TRUE)
    newdata$fit <- pred$fit
    newdata$se <- pred$se.fit
    newdata$lwr <- newdata$fit - 1.96 * newdata$se
    newdata$upr <- newdata$fit + 1.96 * newdata$se
    newdata
  })


#plot the yyyy effect by zone
p <- ggplot() +
  geom_line(data=pred_df, aes(x = yyyy, y = fit)) +
  geom_ribbon(data = pred_df, aes(x = yyyy, y = fit, ymin = lwr, ymax = upr), alpha = 0.2, fill="blue") +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg), size=0.8) +
  labs(title = "Effect of year by zone", x = "yyyy", y = "largest.dhufish.kg") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~Zone, scales = "fixed")




