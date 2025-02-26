
source("R/data_filtering.R")

ggplot() +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

breaks <- c(1900, 1958, 1965, 1989, 2006, 2011)
dat$period <- cut(dat$yyyy, breaks = breaks,labels = c("A: 1900-1958", "B: 1959-1965", "C: 1966-1989", "D: 1990-2006", "E: 2007-2011"))
glimpse(dat)

ggplot(dat, aes(x = yyyy, y = largest.dhufish.kg)) +
  geom_point(aes(color = period)) +  # Colour the points by period  # Separate the plot by period
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))



###
#####shade periods

# Create a dataframe for shading periods
periods <- data.frame(
  xmin = c(1900, 1958, 1965, 1989, 2006),
  xmax = c(1958, 1965, 1989, 2006, 2011),
  period = c("A: 1900-1958", "B: 1959-1965", "C: 1966-1989", "D: 1990-2006", "E: 2007-2011")
)

ggplot() +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg)) +
  geom_rect(data = periods, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = period), alpha = 0.2) +
  scale_fill_viridis_d(option = "viridis", alpha = 0.7) +
  #scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink", "lightgray", "lightyellow")) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(fill = "Period",
       x = "Year", 
       y = "Dhufish size (kg)")



