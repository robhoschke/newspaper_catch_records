###
# Project: Historical recreational fishing
# Data:    historical fish size data
# Task:    Bootstrap linear model plots
# Author:  Rob
# Date:    April 2024

source("R/data_filtering.R")


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


##### all zones combined #####

lm_pop <- lm(largest.dhufish.kg ~ yyyy, data = dat)

# Plot the original data with appropriate labels and axes
plot(largest.dhufish.kg ~ yyyy, data = dat, xlim = c(0, 111), ylim = c(0, 30), pch = 20, 
     xlab = "Year", ylab = "Dhufish Size (kg)", xaxt = "n", yaxt = "n", bty = "n", xaxs = "i", yaxs = "i")
axis(1, at = seq(0, 110, by = 10), labels = seq(1900, 2010, by = 10), las = 1, cex.axis = 0.8, lwd = 1)
axis(2, las = 1, cex.axis = 0.8, lwd = 1)


x_pred <- seq(0, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

##### plot lines and confidence intervals for each zone #####   

###rename intercept column to match code below - saving and re-reading seemed to change the column name 
coefficients_df$"(Intercept)" <- coefficients_df$X.Intercept.

###openPDF


pdf(file="outputs/bootsrap_plots_intervals_only.pdf", width= 11.5, height = 11.5 )
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


# for (i in 1:length(intercepts)) {
#   x <- c(0, 107)
#   y <- intercepts[i] + slopes[i] * x
#   lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02))
# }


##pop and lm mean lines

x_pred <- seq(0, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

y_pred <- y_pred <- (mean(intercepts)) + (mean(slopes) * x_pred)   ##mean bootstrap
lines(x_pred, y_pred, col = "green", lwd = 1, lty=1)

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

##plot all lm's    

intercepts <- (coefficients_df[, "(Intercept)"])+(coefficients_df[, "Zonenearshore_north"])
slopes <- (coefficients_df[, "yyyy"]) + (coefficients_df[, "yyyy.Zonenearshore_north"])


# for (i in 1:length(intercepts)) {
#   x <- c(50, 107)
#   y <- intercepts[i] + slopes[i] * x
#   lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
# }

##plot pop and bootsrap means

x_pred <- seq(50, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

y_pred <- y_pred <- (mean(intercepts)) + (mean(slopes) * x_pred)   ##mean bootstrap
lines(x_pred, y_pred, col = "green", lwd = 1, lty=1)

##confidence intervals

predictions_df <- data.frame(matrix(NA, nrow = nrow(coefficients_df), ncol = 110))

for (i in 1:nrow(coefficients_df)) {
  for (year in 1:110) {
    prediction <- (coefficients_df[i, "(Intercept)"])+(coefficients_df[i, "Zonenearshore_north"]) + 
      ((coefficients_df[i, "yyyy"]+coefficients_df[i, "yyyy.Zonenearshore_north"]) * year)
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


# for (i in 1:length(intercepts)) {
#   x <- c(0, 107)
#   y <- intercepts[i] + slopes[i] * x
#   lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
# }

x_pred <- seq(0, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

y_pred <- y_pred <- (mean(intercepts)) + (mean(slopes) * x_pred)   ##mean bootstrap
lines(x_pred, y_pred, col = "green", lwd = 1, lty=1)

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


# for (i in 1:length(intercepts)) {
#   x <- c(50, 107)
#   y <- intercepts[i] + slopes[i] * x
#   lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
# }

x_pred <- seq(50, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

y_pred <- y_pred <- (mean(intercepts)) + (mean(slopes) * x_pred)   ##mean bootstrap
lines(x_pred, y_pred, col = "green", lwd = 1, lty=1)

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


# for (i in 1:length(intercepts)) {
#   x <- c(50, 107)
#   y <- intercepts[i] + slopes[i] * x
#   lines(x, y, lwd = 0.02, col = rgb(1, 0, 0, 0.02)) 
# }

x_pred <- seq(50, 110, length.out = 11)                             ##pop lm from yyyy 50
y_pred <- predict(lm_pop, newdata = data.frame(yyyy = x_pred))
lines(x_pred, y_pred, col = "blue", lwd = 1, lty=1)

y_pred <- y_pred <- (mean(intercepts)) + (mean(slopes) * x_pred)   ##mean bootstrap
lines(x_pred, y_pred, col = "green", lwd = 1, lty=1)

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



###legend and image for plots

plot(1:10, rnorm(10) * 1:10)
legend("bottomleft", legend = c("population mean", "entry2"), bty = "n",
       lwd = 2, cex = 1.2, col = c("black", "blue", "red"), lty = c(1, 1, NA), pch = c(NA, NA, 8))





### savePDF
dev.off()
