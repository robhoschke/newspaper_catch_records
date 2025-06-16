###beeswarm plot of shore-based versus boat-based catches##


install.packages('ggbeeswarm')
library(ggbeeswarm)

glimpse(dhu_records)
unique(dhu_records$spear)
summary(dhu_records$method)


# Convert NAs to a placeholder value for plotting ("Not Available")
dhu_records$method[is.na(dhu_records$method)] <- "Unspecified"

dhu_records$method <- dplyr::recode(dhu_records$method, 
                             "boat" = "boat-based", 
                             "shore" = "shore-based")

# Reorder the factor levels with "boat" at the top, followed by "shore", and "Not Available" at the bottom
dhu_records$method <- factor(dhu_records$method, levels = c("boat-based", "shore-based", "Unspecified"))

# Plot with updated factor levels and reversed y-axis order
ggplot() +
  geom_beeswarm(data = dhu_records, aes(x = yyyy, y = method, colour = method, shape = spear, size = spear)) +
  scale_size_manual(values = c("FALSE" = 0.9, "TRUE" = 2.5)) +  
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17)) +  
  scale_colour_manual(values = c("shore-based" = "deepskyblue2", "Unspecified" = "darkgray", "boat-based" = "salmon")) + 
  labs(x = "Year", y = "Method", size = "Spearfishing", shape = "Spearfishing") +  
  guides(colour = "none") +  
  theme_classic()  +  
  scale_y_discrete(limits = rev(levels(dhu_records$method))) +  
  scale_x_continuous(limits = c(1900, 2010), breaks = seq(1900,2010, by = 10)) +
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Increase y-axis title size and add space to the right
        axis.line.y = element_blank(),
        legend.position = "none")  +

  
  annotate("point", x = max(dhu_records$yyyy) + 10, y = 2, shape = 17, size = 3, colour = "black") +  
  annotate("text", x = max(dhu_records$yyyy) + 15, y = 2, label = "Spearfishing", hjust = 0, size = 5)  






