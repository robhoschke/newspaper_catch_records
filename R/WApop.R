library(scales)
head(WApop)
WApop
ggplot()+
  geom_point(data = WApop, aes(x=yyyy, y=Pop, xmin = 1900)) +
  theme_bw()

ggplot() +
  geom_point(data = WApop, aes(x = yyyy, y = Pop)) +
  scale_x_continuous(limits = c(1829, NA)) +
  scale_y_continuous(labels=label_comma())+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(size = 16))+
  labs(x = "Year", y = "Human population")
