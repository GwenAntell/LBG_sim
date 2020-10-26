#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods$abbr[1] <- NA
s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

####################
data <- read.csv("./results/max_lat/unimodal_type.csv")

unimodal_plot_samp <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_fill_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "Sampled", title = "Unimodal-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

unimodal_plot_samp

unimodal_plot_rare <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_fill_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "", y = "", subtitle = "Sampling-standardised", title = "Unimodal-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

unimodal_plot_rare

####################
####################
data <- read.csv("./results/max_lat/bimodal_type.csv")

bimodal_plot_samp <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_fill_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "Sampled", title = "Bimodal-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

bimodal_plot_samp

bimodal_plot_rare <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_fill_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "Time (Ma)", y = "", subtitle = "Sampling-standardised", title = "Bimodal-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

bimodal_plot_rare

####################
####################
data <- read.csv("./results/max_lat/flat_type.csv")

flat_plot_samp <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "rarefied"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_fill_manual(values=c(col[1], col[2]), breaks = c("simulated", "sampled")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "Sampled", title = "Flat-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

flat_plot_samp

flat_plot_rare <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =-90, yend = 90), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-90, ymax= 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-100, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  #geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin=-30, ymax= 30), linetype = 1, colour = NA, fill="black", alpha=0.15)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -95, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, colour = type), size = 1.2, alpha = 0.8) +
  geom_point(data = subset(data, type != "sampled"), aes(x = mid_age, y = mid, fill = type), shape = 21, colour= "black", size = 4, alpha = 0.8) +
  scale_colour_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_fill_manual(values=c(col[1], col[3]), breaks = c("simulated", "rarefied")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-100, 90)) +
  labs(x = "", y = "", subtitle = "Sampling-standardised", title = "Flat-type") +
  theme(panel.background = element_blank(),
        legend.position = "none",
                plot.margin = margin(0.25,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        aspect.ratio = 0.6)

flat_plot_rare

####################

p <- ggarrange(flat_plot_samp, flat_plot_rare, unimodal_plot_samp, unimodal_plot_rare, bimodal_plot_samp, bimodal_plot_rare, 
               ncol=2, nrow=3, widths = c(1,1,1,1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/max_lat_plot.png", plot = p, width = 220, height = 220, units = "mm", dpi = 600, scale = 1.7)
