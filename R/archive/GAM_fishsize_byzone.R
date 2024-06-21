

####don'y need this script

#base model using centroid data
#source


tst<-  gam(largest.dhufish.kg ~ s(yyyy, by = Zone, k=4, bs="cr") + s(distance, k=4, bs="cr"),
           family = gaussian(link = "identity"),  data = dat)

####gam by zone####
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

p <- ggplot() +
  geom_line(data=pred_df, aes(x = yyyy, y = fit)) +
  geom_ribbon(data = pred_df, aes(x = yyyy, y = fit, ymin = lwr, ymax = upr), alpha = 0.2, fill="blue") +
  geom_point(data = dat, aes(x = yyyy, y = largest.dhufish.kg), size=0.8) +
  labs(title = "Effect of year by zone", x = "yyyy", y = "largest.dhufish.kg") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~Zone, scales = "fixed")



