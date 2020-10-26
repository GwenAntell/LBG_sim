#global_div_plot
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods$abbr[1] <- NA

MST <- read.csv("./results/MST/global_MST.csv")
ssc <- read.csv("./results/SSC/global_SSC.csv")

#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

p1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.25, ymax= 5), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.25, yend = 5), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.25, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.25, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.125, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = ssc, aes(x = mid_age, y = ssc), colour = col[1], size = 1.2) +
  geom_point(data = ssc, aes(x = mid_age, y = ssc), shape = 21, colour= "black", fill = col[1], size = 3) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Spatial sampling coverage (%)") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, hjust = 0.5),
        aspect.ratio = 0.6)

p1

p2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-5000, ymax= 110000), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -5000, yend = 110000), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -5000, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -5000, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -2500, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = MST, aes(x = mid_age, y = MST), colour = col[1], size = 1.2) +
  geom_point(data = MST, aes(x = mid_age, y = MST), shape = 21, colour= "black", fill = col[1], size = 3) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "Summed MST length (km)") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, hjust = 0.5),
        aspect.ratio = 0.6)

p2

p <- ggarrange(p1, p2,
               ncol=1, nrow=2, widths = c(1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/global_sampling_plot.png", plot = p, width = 120, height = 150, units = "mm", dpi = 600, scale = 1.7)
