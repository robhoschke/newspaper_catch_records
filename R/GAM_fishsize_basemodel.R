##model selection 

source("R/data_filtering.R")

glimpse(dat)
str(df_centroid)
df_centroid <- as.data.frame(dat)

df_selected <- dplyr::select(df_centroid, yyyy, distance)
correlate(df_selected)


as.data
gam_model1 <-  gam(largest.dhufish.kg ~ s(yyyy,  k=4, bs="cr"), 
                 family = gaussian(link = "identity"), data = dat)

gam_model2 <-  gam(largest.dhufish.kg ~  s(yyyy, k=4, bs="cr") + s(distance, k=4, bs="cr"),
                 family = gaussian(link = "identity"), data = dat)

gam_model3 <- gam(largest.dhufish.kg ~ ti(yyyy, distance, k=4, bs="cr") + s(yyyy, k=4, bs="cr") + s(distance, k=4, bs="cr"), 
                 family = gaussian(link = "identity"), data = dat)



summary(gam_model1)
AIC(gam_model1)
glance(gam_model1)
augment(gam_model1)

summary(gam_model2)
AIC(gam_model2)

summary(gam_model3)
AIC(gam_model3)

summary(gam_model4)
AIC(gam_model4)


gam_model4 <- gam(largest.dhufish.kg ~ s(yyyy, by = Zone, k=4, bs="cr") + s(yyyy, k=4, bs="cr"), 
                  family = gaussian(link = "identity"), data = dat)

gam_model4 <- gam(largest.dhufish.kg ~ s(yyyy,  k=4, bs="cr") + s(Zone, k=4, bs="cr"), 
                  family = gaussian(link = "identity"), data = dat)


##### simple GAM fish size time #####
gam_mod <- gam(largest.dhufish.kg ~ s(yyyy,  k=4, bs="cr"), 
                  family = gaussian(link = "identity"), data = dat)
summary(gam_mod)
AIC(gam_mod)

yyyy_seq <- seq(0, 107, length.out = 108)
yyyy_pred <- data.frame(yyyy = yyyy_seq + 1904)
yyyy_pred$fit <- predict(gam_mod, newdata = yyyy_pred, se.fit = TRUE)$fit
yyyy_pred$se <- predict(gam_mod, newdata = yyyy_pred, se.fit = TRUE)$se.fit
yyyy_pred$lwr <- yyyy_pred$fit - 1.96 * yyyy_pred$se
yyyy_pred$upr <- yyyy_pred$fit + 1.96 * yyyy_pred$se

ggplot()+
  geom_ribbon(data = yyyy_pred, aes(x = yyyy, ymin = lwr, ymax = upr) ,fill="coral", alpha=0.7)+
  geom_line(data = yyyy_pred, aes(x = yyyy, y = fit)) +
  geom_rug(data = dat, aes(x = yyyy, y = largest.dhufish.kg), position="jitter" , alpha = 0.4, sides="b")+
  #geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  scale_y_continuous(limits = c(7.5, 20)) +
  labs(
       x = "Year",
       y = "Dhufish weight (kg)") +
  theme(legend.position = "none")
