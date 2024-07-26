##model selection 

source("R/data_filtering.R")
library(corrr)
library(dplyr)
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


