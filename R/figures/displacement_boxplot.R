#box plot
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
#---------------------------------
data <- read.csv("./results/SLD/flat_temporal_SLD.csv")

displacement <- append(data$sampled_SLD, data$rarefied_SLD)
type <- append(rep("Sampled", times = 56), rep("Sampling-standardised", times = 56))

data <- cbind.data.frame(displacement, as.factor(type))
#---------------------------------

p1 <- ggplot(data, aes(x = type, y = displacement)) + 
  geom_boxplot(aes(fill = type), notch = TRUE) + 
  scale_fill_brewer(palette="Blues") +
  labs(x = "Type", y = "Displacement", title = "Flat-type") +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        legend.position = "none",
        aspect.ratio = 0.6)
p1

#---------------------------------
data <- read.csv("./results/SLD/unimodal_temporal_SLD.csv")

displacement <- append(data$sampled_SLD, data$rarefied_SLD)
type <- append(rep("Sampled", times = 56), rep("Sampling-standardised", times = 56))

data <- cbind.data.frame(displacement, as.factor(type))
#---------------------------------

p2 <- ggplot(data, aes(x = type, y = displacement)) + 
  geom_boxplot(aes(fill = type), notch = TRUE) + 
  scale_fill_brewer(palette="Blues") +
  labs(x = "Type", y = "Displacement", title = "Unimodal-type") +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        legend.position = "none",
        aspect.ratio = 0.6)
p2

#---------------------------------
data <- read.csv("./results/SLD/bimodal_temporal_SLD.csv")

displacement <- append(data$sampled_SLD, data$rarefied_SLD)
type <- append(rep("Sampled", times = 56), rep("Sampling-standardised", times = 56))

data <- cbind.data.frame(displacement, as.factor(type))
#---------------------------------

p3 <- ggplot(data, aes(x = type, y = displacement)) + 
  geom_boxplot(aes(fill = type), notch = TRUE) + 
  scale_fill_brewer(palette="Blues") +
  labs(x = "Type", y = "Displacement", title = "Bimodal-type") +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        legend.position = "none",
        aspect.ratio = 0.6)
p3

#---------------------------------

p <- ggarrange(p1, p2,p3,
               ncol=3, nrow=1, widths = c(1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/displacement_box_plot.png", plot = p, width = 300, height = 75, units = "mm", dpi = 600, scale = 1.7)

#---------------------------------
data <- read.csv("./results/SLD/LBG_type_SLD.csv")

displacement <- c(data$simulated_SLD_UB, data$sampled_SLD_UB, data$rarefied_SLD_UB)
type <- c(rep("Simulated", times = 56), rep("Sampled", times = 56), rep("Sampling-standardised", times = 56))

data <- cbind.data.frame(displacement, as.factor(type))
#---------------------------------

p4 <- ggplot(data, aes(x = type, y = displacement)) + 
  geom_boxplot(aes(fill = type), notch = TRUE) + 
  scale_fill_brewer(palette="Blues") +
  labs(x = "Type", y = "Displacement", title = "Unimodal and bimodal") +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        legend.position = "none",
        aspect.ratio = 0.6)
p4

#---------------------------------
