#global_div_plot
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods$abbr[1] <- NA

unimodal <- read.csv("./results/global/unimodal_global_div.csv")
bimodal <- read.csv("./results/global/bimodal_global_div.csv")
flat <- read.csv("./results/global/flat_global_div.csv")

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]
#---------------------------------

flat_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 350), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 350), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), colour = "black", alpha=1)  +
  geom_ribbon(data = flat, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = flat, aes(x = mid_age, y = mean_richness), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = mean_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Sampled richness", title = "Flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)

flat_plot
#---------------------------------

unimodal_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 350), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 350), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), colour = "black", alpha=1)  +
  geom_ribbon(data = unimodal, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = unimodal, aes(x = mid_age, y = mean_richness), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = mean_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Sampled richness", title = "Unimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold",  hjust = 0.5),
        aspect.ratio = 0.6)

unimodal_plot

#---------------------------------

bimodal_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 350), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 350), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), colour = "black", alpha=1)  +
  geom_ribbon(data = bimodal, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = bimodal, aes(x = mid_age, y = mean_richness), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = mean_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "Sampled richness", title = "Bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold",  hjust = 0.5),
        aspect.ratio = 0.6)

bimodal_plot


p <- ggarrange(flat_plot, unimodal_plot, bimodal_plot,
               ncol=1, nrow=3, widths = c(1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/global_div_plot.png", plot = p, width = 100, height = 200, units = "mm", dpi = 600, scale = 1.7)
