###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    experimental modelling fish size
# Author:  Rob
# Date:    April 2024
##
# devtools::install_github("beckyfisher/FSSgam_package") # Run once
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("corrr")
# install.packages("mgcv")
# install.packages("car")
# install.packages("FSSgam")
# install.packages("devtools")
# install.packages("MuMIn")
# install.packages("doBy")
# install.packages("doSNOW")

# devtools::install_github("UWAMEGFisheries/GlobalArchive") # Run once
#install.packages(c("vcdExtra", "bbmle", "DescTools", "gridExtra","corrplot"))

library(FSSgam)
library(tidyverse)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(doSNOW)
library(devtools)
library(GlobalArchive)
library(ggplot2)
library(corrr)
library(lme4)
library(MASS)
library(vcdExtra)
library(bbmle)
library(DescTools)
library(remotes)
library(gridExtra)
library(lattice)
library(corrplot)

#devtools::install_github("GlobalArchiveManual/CheckEM") ###failed

source("R/data_filtering.R")


###plot centroids by zone
ggplot() +
  geom_point(data = dat, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], color = Zone), size = 0.5) +
  geom_sf(data = WA_base, inherit.aes = FALSE) +
  xlim(114.9851, 115.8) +
  ylim(-32.7966, -31.30936) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() 


##### check bathy above zero #####

bathy_above_zero <- dat[dt$bathy >= 0, ] ### Plot points with bathy values above zero on basemap

ggplot() +
  geom_sf(data = WA_base) +  # Basemap
  geom_sf(data = bathy_above_zero, color = "blue") +  
  labs(title = "Points with Bathy Values above Zero on WA Base Map") +
  xlim(114.9851, 115.8) +
  ylim(-32.7966, -31.30936) +
  theme_minimal()


##### plots by depth classes#####

depth_range_subset <- dat %>%
  filter(bathy <= 0 & bathy >= -10)

plot(largest.dhufish.kg ~ yyyy, data=depth_range_subset)

depth_range_subset <- dt %>%
  filter(bathy <= -20 & bathy >= -30)


depth_intervals <- seq(0, -200, by = -10)  # Generate depth intervals from 0 to -200 in steps of -10

for (depth_interval in depth_intervals) {
  lower_bound <- depth_interval
  upper_bound <- depth_interval - 10
  
  depth_subset <- dt %>%
    filter(bathy >= upper_bound & bathy <= lower_bound)
  
  if (nrow(depth_subset) > 0) {  # Check if subset is not empty
    plot <- ggplot(depth_subset, aes(x = yyyy, y = largest.dhufish.kg)) +
      geom_point() +
      labs(title = paste("Depth Range:", lower_bound, "to", upper_bound),
           x = "Year", y = "Largest Dhufish (kg)")
    
    print(plot)
  }
}


##### plots by zone#####
ggplot() +
  geom_point(data = dt, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], color = Zone), size = 0.5) +
  geom_sf(data = WA_base, inherit.aes = FALSE) +
  xlim(114.9851, 115.8) +
  ylim(-32.7966, -31.30936) +
  scale_fill_gradient(trans = "log") +
  labs( x = "Longitude", y = "Latitude") +
  theme_minimal() 


sum(dt$Zone=="offshore_south")

unique_zones <- unique(dt$Zone)
Zone_sums <- list()

for(i in unique_zones) {
  fact_sums <- sum(dt$Zone==i)
  Zone_sums[[i]] <- fact_sums
}

sum(dt$Zone=="nearshore_north")


#####plotting size against year for each zone #####


ggplot() +
  geom_point(data = dt, aes( x = yyyy, y = latitude)) 

ggplot() +
  geom_point(data = dt, aes( x = latitude, y = largest.dhufish.kg)) 

ggplot() +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg)) +
  facet_wrap(~Zone, scales = "free_y", nrow = 4) +  # Separate plots for each zone
  labs(x = "Year", y = "Largest Dhufish (kg)") +
  theme_minimal()

ggplot() +
  geom_point(data = dt, aes(x = yyyy, y = largest.dhufish.kg)) +
  geom_smooth(data = dt, aes(x = yyyy, y = largest.dhufish.kg, group = Zone), method = "lm", se = FALSE) +
  facet_wrap(~Zone, scales = "free_y", nrow = 4) +  # Separate plots for each zone
  labs(x = "Year", y = "Largest Dhufish (kg)") +
  theme_minimal()


slope_intercept <- dt %>%
  group_by(Zone) %>%
  do({
    lm_model <- lm(largest.dhufish.kg ~ yyyy, data = .)
    tibble(
      Zone = unique(.$Zone),
      slope = coef(lm_model)[["yyyy"]],
      intercept = coef(lm_model)[["(Intercept)"]]
    )
  })


##### plotting depth against dhu size for each zone #####


plot_list6 <- list()

zones_to_include <- unique(dt$Zone)

# Loop through each zone
for (i in 1:length(zones_to_include)) {
  subset_data <- subset(dt, Zone == zones_to_include[i])
  
  lm_model <- lm(largest.dhufish.kg ~ bathy, data = subset_data)
  
  r_squared <- summary(lm_model)$r.squared
  
  intercept <- coef(lm_model)[1]
  
  plot <- ggplot(subset_data, aes(x = bathy, y = largest.dhufish.kg)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add lm line
    labs(title = zones_to_include[i]) +
    annotate("text", x = min(subset_data$bathy), y = max(subset_data$largest.dhufish.kg), 
             label = paste("R2 =", round(r_squared, 3), "\nIntercept =", round(intercept, 3)), 
             hjust = 0, vjust = 1)  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  # Add the plot to the list
  plot_list6[[i]] <- plot
}

# Print the list of plots
plot_list6



##### sizing points based on dhufish size  #####

min_fish_size <- min(dt$largest.dhufish.kg) 
max_fish_size <- max(dt$largest.dhufish.kg)

zones_to_include <- unique(dt$Zone)

overall_bbox <- st_bbox(dt$geometry)

for (i in 1:length(zones_to_include)) {
  
  subset_data <- subset(dt, Zone == zones_to_include[i])
  scaled_sizes <- 1 + 4 * ((subset_data$largest.dhufish.kg - 15) / (max_fish_size - 15))^2
  plot(subset_data$geometry, pch = 20, cex = scaled_sizes, 
       col = subset_data$decade,
       main = paste(zones_to_include[i]),
       xlim = c(overall_bbox["xmin"], overall_bbox["xmax"]),
       ylim = c(overall_bbox["ymin"], overall_bbox["ymax"]))
}



#####compare model from random points and centroids#####
plot(largest.dhufish.kg~Zone*yyyy, data=dat)
plot(largest.dhufish.kg~Zone*yyyy, data=dt)
lm1 <- lm(largest.dhufish.kg~Zone*yyyy, data=dat) ### centroids
summary(lm1)

lm2 <- lm(largest.dhufish.kg~Zone*yyyy, data=dt) ### random
summary(lm2)

lm.test <- lm(largest.dhufish.kg ~ yyyy*bathy, data = dt)
summary(lm.test)

lm.test <- lm(largest.dhufish.kg ~ latitude, data = dt)
summary(lm.test)

plot(largest.dhufish.kg ~ latitude, data = dt)

# Convert selected columns to numeric if they are not already numeric
numeric_cols <- c("largest.dhufish.kg", "yyyy", "bathy")
dt_numeric <- dt[, numeric_cols]

# Convert to numeric
dt_numeric[] <- lapply(dt_numeric, as.numeric)

# Remove rows with missing values
dt_numeric <- na.omit(dt_numeric)

# Compute the correlation matrix
cor_matrix <- cor(dt_numeric)

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color")



cor_matrix <- cor(dt[c("largest.dhufish.kg", "yyyy", "bathy")])

# Visualize the correlation matrix as a heatmap
corrplot(cor_matrix, method = "color")

par(mfrow = c(1, 1))

plot(lm.test.resid ~ as.factor(dt$Zone), xlab = "Zone",
     ylab = "Standardized residuals")

abline(0, 0, lty = 2)


lm1 <- lm(largest.dhufish.kg ~ bathy, data=subset(dt, Zone=='nearshore_south'))
summary(lm1)


lm1 <- lm(largest.dhufish.kg ~ bathy, data=dt)
summary(lm1)

plot(largest.dhufish.kg ~ bathy, data=dt)+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, color = "blue") 
  

ggplot(dt, aes(x = bathy, y = largest.dhufish.kg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")

ggplot(dt, aes(x = yyyy, y = largest.dhufish.kg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue")+
  geom_smooth(method = "rlm", se = TRUE, color = "blue")+
  geom_smooth(method = "glm", se = TRUE, color = "blue")+
  geom_smooth(method = "gam", se = TRUE, color = "blue")

ggplot(dt, aes(x = yyyy, y = bathy)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")+
  geom_smooth(method = "rlm", se = TRUE, color = "blue")+
  geom_smooth(method = "glm", se = TRUE, color = "blue")+
  geom_smooth(method = "gam", se = TRUE, color = "blue")



lm_model <- lm(largest.dhufish.kg ~ bathy, data = dt)
summary(lm_model)

lm_model <- lm(bathy ~ yyyy, data = dt)
summary(lm_model)


plot_list6 <- list()

zones_to_include <- unique(dt$Zone)

for (i in 1:length(zones_to_include)) {
  subset_data <- subset(dt, Zone == zones_to_include[i])
  
  lm_model <- lm(largest.dhufish.kg ~ bathy, data = subset_data)
  
  r_squared <- summary(lm_model)$r.squared
  
  intercept <- coef(lm_model)[1]
  
  p_value_slope <- summary(lm_model)$coefficients[2, 4]
  
  lm_summary <- list(R2 = r_squared, Intercept = intercept, P_Value_Slope = p_value_slope)
  
  plot <- ggplot(subset_data, aes(x = bathy, y = largest.dhufish.kg)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add lm line
    labs(title = zones_to_include[i]) +
    annotate("text", x = min(subset_data$bathy), y = max(subset_data$largest.dhufish.kg), 
             label = paste("R2 =", round(r_squared, 3), "\nIntercept =", round(intercept, 3), "\nP-Value =", round(p_value_slope, 3)), 
             hjust = 0, vjust = 1)  +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  plot_list6[[i]] <- list(Plot = plot, LM_Summary = lm_summary)
}

plot_list6

lm_model <- lm(largest.dhufish.kg ~  bathy*yyyy, data = dt)
summary(lm_model)

residuals <- residuals(lm_model)

# Create a histogram of residuals
hist(residuals, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")


mixed_model <- lmer(largest.dhufish.kg ~ yyyy * bathy + (1 | Zone), data = dt)
summary(mixed_model)




# mixed-effects model
mixed_model1 <- lmer(largest.dhufish.kg ~ yyyy * depth + (1 | Zone), data = dt)
mixed_model2 <- lmer(largest.dhufish.kg ~ yyyy  + (1 | Zone), data = dt)
mixed_model3 <- lmer(largest.dhufish.kg ~ yyyy * bathy + (1 | echo_sounders), data = dt)
mixed_model3 <- lmer(largest.dhufish.kg ~ yyyy * bathy + (1 | echo_sounders), data = dt)


summary(mixed_model1)



#####GAM#####
tidy.count <- dat 
tidy.count$response <- "fish_size"
glimpse(tidy.count)
sum(is.na(tidy.count$bathy))
sum(is.na(tidy.count$largest.dhufish.kg))
sum(is.na(tidy.count$yyyy))
sum((is.na(tidy.count$latitude)))


pred.vars <- c("bathy", "yyyy", "latitude") 

summary(dat[,pred.vars])

round(cor(tidy.count[ , pred.vars]), 2)


unique.vars <- unique(as.character(tidy.count$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy.count[which(tidy.count$response == unique.vars[i]), ]
  if(length(which(temp.dat$largest.dhufish.kg == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

outdir <- ("outputs") 
out.all <- list()
var.imp <- list()


####run full subset model selection process####

for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(tidy.count[which(tidy.count$response == resp.vars[i]),])
  print(resp.vars[i])
  
  Model1  <- gam(largest.dhufish.kg ~ s(bathy, k = 3, bs = 'cr'),
                 family = gaussian(link = "identity"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  factor.smooth.interactions = NA,
                                  cyclic.vars = "aspect",
                                  k = 3)
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models 
  mod.table = out.list$mod.data.out 
  mod.table = mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi = cumsum(mod.table$wi.AICc)
  out.i = mod.table[which(mod.table$delta.AICc <= 2),]
  out.all = c(out.all,list(out.i))
  var.imp = c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))
  
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = here::here(paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/")))
    if(best.model.name != "null"){
      par(mfrow = c(3,1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T,pages = 1,residuals = T,pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}















#####without loop#####

glimpse(use.dat)
factor.vars <- c("Zone", "gps", "echo_sounders") 

use.dat$response <- "fish_size"
Model1 <- gam(largest.dhufish.kg ~ s(yyyy, bs = "cr", k=3),
              family = gaussian(), data = use.dat)

summary(Model1)

plot(Model1, se = TRUE, col = "blue")


model.set <- generate.model.set(use.dat = use.dat,
                                max.predictors = 2,
                                test.fit = Model1,
                                pred.vars.cont = pred.vars,  
                                pred.vars.fact = factor.vars,    
                                linear.vars = "yyyy",          
                                k = 3,
                                cov.cutoff = 0.3,
                                factor.smooth.interactions = FALSE) 


out.list <- fit.model.set(model.set,parallel = TRUE)
