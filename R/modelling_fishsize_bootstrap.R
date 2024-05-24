###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    bootstrap modelling fish size and zone
# Author:  Rob
# Date:    April 2024


source("R/data_filtering.R")

#####model selection#####

predictors <- c("yyyy", "Zone")  # List of predictor variables
interaction_terms <- combn(predictors, 2, paste, collapse = ":")  # Generate all pairwise interactions

all_terms <- c(predictors, interaction_terms)

predictor_combinations <- lapply(1:length(all_terms), function(x) combn(all_terms, x, simplify = FALSE))

fit_lm_and_calc_aic <- function(predictors, data) {
  formula <- as.formula(paste("largest.dhufish.kg ~", paste(predictors, collapse = " + ")))
  
  lm_model <- lm(formula, data = dat)
  
  # Calculate AIC
  return(AIC(lm_model))
}

# Generate combinations including interactions
all_predictor_combinations <- unlist(predictor_combinations, recursive = FALSE)

# Calculate AIC for all combinations of predictors
aic_values <- sapply(all_predictor_combinations, function(predictors) {
  fit_lm_and_calc_aic(predictors, data = dt)
})

aic_df <- data.frame(
  Predictor_Combination = sapply(all_predictor_combinations, paste, collapse = ", "),
  AIC_Value = aic_values
)

print(aic_df)
arrange(aic_df, AIC_Value)

# Find the index of the minimum AIC value
best_model_index <- which.min(aic_values)

best_predictor_combo <- all_predictor_combinations[[best_model_index]]



#####model bootsrap#####

library(sf)
library(dplyr)
library(devtools)

fishing_trips
glimpse(fishing_trips)
str(fishing_trips)





# Define the number of repetitions
n_repeats <- 1000

# Initialize an empty list to store model coefficients
all_lm_coefs <- vector("list", length = n_repeats)

# Loop over repetitions
for (i in 1:n_repeats) {
  print(i)
  all_r_points_with_metadata <- list()
  for (j in 1:nrow(fishing_trips)) {
    polygon <- fishing_trips[j, ]
    r_points <- st_sample(polygon$geometry, 1)
    metadata_df <- data.frame(ID = rep(polygon$ID, 1),
                              yyyy = rep(polygon$yyyy, 1),
                              largest.dhufish.kg = rep(polygon$largest.dhufish.kg, 1))
    r_points_with_metadata <- cbind(metadata_df, geometry = r_points)
    all_r_points_with_metadata[[j]] <- r_points_with_metadata
  }
  single_trip_points <- do.call(rbind, all_r_points_with_metadata)
  
  # Add zones and bathy
  dt <- single_trip_points %>%
    st_as_sf() %>%
    st_join(st_as_sf(perth_zones)) %>%
    mutate(
      Zone = as.factor(Zone)
    ) %>%
    mutate(
      bathy = extract(bathy, .)$bathy_cropped1,
      bathy = ifelse(bathy >= 0, runif(sum(bathy >= 0), min = -10, max = -2), bathy) ##resample positive bathy values
    ) %>% 
    arrange(ID) %>% 
    mutate(depth=bathy*-1,
           yyyy=as.numeric(yyyy)-1904)
  
  # Fit a linear model to the data
  lm_model <- lm(formula = largest.dhufish.kg ~  yyyy*Zone, data = dt)     ######original model Zone*yyyy
  
  # Store the coefficients of the model
  all_lm_coefs[[i]] <- coef(lm_model)
}


# Convert coefficients to a data frame
coefficients_df <- do.call(rbind, all_lm_coefs)
write.csv (coefficients_df, "outputs/bootstap_coeffs.csv")


coefficients_df <- read.csv("outputs/bootstap_coeffs.csv") #### this seems to generate a different 'intercept' column heading from the initial output 
head(coefficients_df)
summary(lm_model)

coef_means <- colMeans(do.call(rbind, all_lm_coefs))
coef_sds <- apply(do.call(rbind, all_lm_coefs), 2, sd)
coef_lowers <- apply(do.call(rbind, all_lm_coefs), 2, quantile, probs = 0.025)
coef_uppers <- apply(do.call(rbind, all_lm_coefs), 2, quantile, probs = 0.975)


summary(lm_model)
# Create a table of coefficients
coef_table <- data.frame(
  Mean = coef_means,
  SD = coef_sds,
  Lower = coef_lowers,
  Upper = coef_uppers
)

# Print the table
print(coef_table)


##### plot lines and confidence intervals for each zone #####

plot(largest.dhufish.kg~yyyy, data = dat, xlim = c(0, 111), ylim = c(0, 20), pch = 20, 
    xlab = "Year", ylab = "Dhufish Size (kg)", main = "compare regressions", xaxt = "n")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

# Set up plot with custom axis limits
plot(largest.dhufish.kg ~ yyyy, data = dat, xlim = c(0, 111), ylim = c(0, 20), pch = 20, 
    xlab = "Year", ylab = "Dhufish Size (kg)", 
    main = "compare regressions", xaxt = "n", yaxt = "n", bty = "n", xaxs = "i", yaxs = "i")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8, lwd = 1)
    axis(2, las = 1, cex.axis = 0.8, lwd = 1)


###rename intercept column to match code below - saving and re-reading seemed to change the column name 
coefficients_df$"(Intercept)" <- coefficients_df$X.Intercept.

###openPDF



pdf(file="outputs/bootsrap_plots_cols_2.pdf", width= 11.5, height = 11.5 )
par(mfrow = c(3, 2))



###near rottnest

##base plot
subset_data <- subset(dat, Zone == "near_rottnest")  
lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = subset_data)


plot(largest.dhufish.kg~yyyy, data = subset_data, xlim = c(0, 111), ylim = c(0, 25), pch = 20, cex=0.5, 
    xlab = "Year", ylab = "Dhufish Size (kg)", main = "Near Rottnest", xaxt = "n")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

intercepts <- coefficients_df[, "(Intercept)"]
slopes <- coefficients_df[, "yyyy"]


for (i in 1:length(intercepts)) {
  x <- c(0, 107)
  y <- intercepts[i] + slopes[i] * x
  lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02))
}

abline(lm_pop,lty=2,col = rgb(0.2, 0, 0.8, 1) )  ##pop lm
abline(mean(intercepts),mean(slopes), lwd=1, lty=2, col = rgb(0, 0.8, 0.2, 1) ) ##mean bootsrap 

##confidence intervals
predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))


for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- coefficients_df[i, "(Intercept)"] + coefficients_df[i, "yyyy"] * year
    predictions_df[i, year] <- prediction
  }
}

lower <- function(column) {
  quantile(column, probs = 0.025)
}

upper <- function(column) {
  quantile(column, probs = 0.975)
}

# Apply the function to each column of the data frame
lowerresult <- as.vector(sapply(predictions_df, lower))
upperresult <- as.vector(sapply(predictions_df, upper))

lines(upperresult, lty=2)
lines(lowerresult, lty=2)


#### nearshore north

##base plot
subset_data <- subset(dat, Zone == "nearshore_north")
lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = subset_data)


plot(largest.dhufish.kg~yyyy, data = subset_data, xlim = c(0, 111), ylim = c(0, 25), pch = 20, cex=0.5, 
    xlab = "Year", ylab = "Dhufish Size (kg)", main = "Nearshore north", xaxt = "n")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

intercepts <- (coefficients_df[, "(Intercept)"])+(coefficients_df[, "Zonenearshore_north"])
slopes <- (coefficients_df[, "yyyy"]) + (coefficients_df[, "yyyy.Zonenearshore_north"])


for (i in 1:length(intercepts)) {
  x <- c(50, 107)
  y <- intercepts[i] + slopes[i] * x
  lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
}

abline(lm_pop,lty=2,col = rgb(0.2, 0, 0.8, 1) )  ##pop lm
abline(mean(intercepts),mean(slopes), lwd=1, lty=2, col = rgb(0, 0.8, 0.2, 1) ) ##mean bootsrap 

##confidence intervals

predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))

for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- (coefficients_df[i, "(Intercept)"])+(coefficients_df[i, "Zonenearshore_north"]) + 
    ((coefficients_df[i, "yyyy"]+coefficients_df[i, "yyyy.Zonenearshore_north"]) * year)
    predictions_df[i, year] <- prediction
  }
}

head(predictions_df)

lower <- function(column) {
  quantile(column, probs = 0.025)
}

upper <- function(column) {
  quantile(column, probs = 0.975)
}

# Apply the function to each column of the data frame
lowerresult <- as.vector(sapply(predictions_df, lower))
upperresult <- as.vector(sapply(predictions_df, upper))

x_values <- 50:110
y_lower <- lowerresult[50:110]
y_upper <- upperresult[50:110]

lines(x_values, y_lower, lty = 2)
lines(x_values, y_upper, lty = 2)


####nearshore south

subset_data <- subset(dat, Zone == "nearshore_south")
lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = subset_data)


##base plot
plot(largest.dhufish.kg~yyyy, data = subset_data, xlim = c(0, 111), ylim = c(0, 25), pch = 20, cex=0.5, 
  xlab = "Year", ylab = "Dhufish Size (kg)", main = "Nearshore south", xaxt = "n")
  axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

##lm lines
intercepts <- (coefficients_df[, "(Intercept)"])+(coefficients_df[, "Zonenearshore_south"])
slopes <- (coefficients_df[, "yyyy"]) + (coefficients_df[, "yyyy.Zonenearshore_south"])


for (i in 1:length(intercepts)) {
  x <- c(0, 107)
  y <- intercepts[i] + slopes[i] * x
  lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
}

abline(lm_pop,lty=2,col = rgb(0.2, 0, 0.8, 1) )  ##pop lm
abline(mean(intercepts),mean(slopes), lwd=1, lty=2, col = rgb(0, 0.8, 0.2, 1) ) ##mean bootsrap 

##confidence intervals

predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))

for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- (coefficients_df[i, "(Intercept)"])+(coefficients_df[i, "Zonenearshore_south"]) + 
    ((coefficients_df[i, "yyyy"]+coefficients_df[i, "yyyy.Zonenearshore_south"]) * year)
    predictions_df[i, year] <- prediction
  }
}

head(predictions_df)

lower <- function(column) {
  quantile(column, probs = 0.025)
}

upper <- function(column) {
  quantile(column, probs = 0.975)
}

# Apply the function to each column of the data frame
lowerresult <- as.vector(sapply(predictions_df, lower))
upperresult <- as.vector(sapply(predictions_df, upper))


lines(upperresult, lty=2)
lines(lowerresult, lty=2)


####offshore north

##base plot
subset_data <- subset(dat, Zone == "offshore_north")
lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = subset_data)


plot(largest.dhufish.kg~yyyy, data = subset_data, xlim = c(0, 111), ylim = c(0, 25), pch = 20, cex=0.5,
    xlab = "Year", ylab = "Dhufish Size (kg)", main = "Offshore north", xaxt = "n")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

##lm lines

intercepts <- (coefficients_df[, "(Intercept)"])+(coefficients_df[, "Zoneoffshore_north"])
slopes <- (coefficients_df[, "yyyy"]) + (coefficients_df[, "yyyy.Zoneoffshore_north"])


for (i in 1:length(intercepts)) {
  x <- c(50, 107)
  y <- intercepts[i] + slopes[i] * x
  lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
}

abline(lm_pop,lty=2,col = rgb(0.2, 0, 0.8, 1) )  ##pop lm
abline(mean(intercepts),mean(slopes), lwd=1, lty=2, col = rgb(0, 0.8, 0.2, 1) ) ##mean bootsrap 

##confidence intervals

predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))

for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- (coefficients_df[i, "(Intercept)"])+(coefficients_df[i, "Zoneoffshore_north"]) + 
    ((coefficients_df[i, "yyyy"]+coefficients_df[i, "yyyy.Zoneoffshore_north"]) * year)
    predictions_df[i, year] <- prediction
  }
}


lower <- function(column) {
  quantile(column, probs = 0.025)
}

upper <- function(column) {
  quantile(column, probs = 0.975)
}

# Apply the function to each column of the data frame
lowerresult <- as.vector(sapply(predictions_df, lower))
upperresult <- as.vector(sapply(predictions_df, upper))

x_values <- 50:110
y_lower <- lowerresult[50:110]
y_upper <- upperresult[50:110]

lines(x_values, y_lower, lty = 2)
lines(x_values, y_upper, lty = 2)


####offshore south  

##base plot
subset_data <- subset(dat, Zone == "offshore_south")
lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = subset_data)


plot(largest.dhufish.kg~yyyy, data = subset_data, xlim = c(0, 111), ylim = c(0, 25), pch = 20, cex=0.5,
    xlab = "Year", ylab = "Dhufish Size (kg)", main = "Offshore south", xaxt = "n")
    axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8)

##lines for each model
intercepts <- (coefficients_df[, "(Intercept)"])+(coefficients_df[, "Zoneoffshore_south"])
slopes <- (coefficients_df[, "yyyy"]) + (coefficients_df[, "yyyy.Zoneoffshore_south"])


for (i in 1:length(intercepts)) {
  x <- c(50, 107)
  y <- intercepts[i] + slopes[i] * x
  lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
}

abline(lm_pop,lty=2,col = rgb(0.2, 0, 0.8, 1) )  ##pop lm
abline(mean(intercepts),mean(slopes), lwd=1, lty=2, col = rgb(0, 0.8, 0.2, 1) ) ##mean bootsrap 

##confidence intervals

predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))

for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- (coefficients_df[i, "(Intercept)"])+(coefficients_df[i, "Zoneoffshore_south"]) + 
    ((coefficients_df[i, "yyyy"]+coefficients_df[i, "yyyy.Zoneoffshore_south"]) * year)
    predictions_df[i, year] <- prediction
  }
}


lower <- function(column) {
  quantile(column, probs = 0.025)
}

upper <- function(column) {
  quantile(column, probs = 0.975)
}

# Apply the function to each column of the data frame
lowerresult <- as.vector(sapply(predictions_df, lower))
upperresult <- as.vector(sapply(predictions_df, upper))

x_values <- 50:110
y_lower <- lowerresult[50:110]
y_upper <- upperresult[50:110]

lines(x_values, y_lower, lty = 2)
lines(x_values, y_upper, lty = 2)


### savePDF
dev.off()
