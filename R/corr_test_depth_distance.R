##corr test##

source("R/data_filtering.R")
glimpse(dat)

# Perform ANOVA to compare distance across depth zones
result <- aov(distance ~ depth_zone, data = dat)
summary(result)

# Perform Kruskal-Wallis test to compare distance across depth zones
kruskal.test(distance ~ depth_zone, data = dat)

dat$depth_zone_numeric <- ifelse(dat$depth_zone == "inshore_demersal", 1, 0)

cor(dat$distance, dat$depth_zone_numeric)


dat$depth_zone_numeric <- ifelse(dat$depth_zone == "inshore_demersal", 1, 0)
model <- lm(largest.dhufish.kg ~ distance + depth_zone_numeric, data = dat)

library(car)
vif(model)
