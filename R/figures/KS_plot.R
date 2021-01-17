#KS_plot
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods$abbr[1] <- NA

unimodal <- read.csv("./results/KS_test/unimodal_temporal_KS_test.csv")
unimodal$sig_samp <- NA
unimodal$sig_samp[which(unimodal$sampled_ks_sig <= 0.05)] <- col[1]

unimodal$sig_rare <- NA
unimodal$sig_rare[which(unimodal$rarefied_ks_sig <= 0.05)] <- col[1]

bimodal <- read.csv("./results/KS_test/bimodal_temporal_KS_test.csv")
bimodal$sig_samp <- NA
bimodal$sig_samp[which(bimodal$sampled_ks_sig <= 0.05)] <- col[1]

bimodal$sig_rare <- NA
bimodal$sig_rare[which(bimodal$rarefied_ks_sig <= 0.05)] <- col[1]

flat <- read.csv("./results/KS_test/flat_temporal_KS_test.csv")
flat$sig_samp <- NA
flat$sig_samp[which(flat$sampled_ks_sig <= 0.05)] <- col[1]

flat$sig_rare <- NA
flat$sig_rare[which(flat$rarefied_ks_sig <= 0.05)] <- col[1]

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]
#---------------------------------

flat_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = flat, aes(x = mid_age, y = sampled_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = sampled_ks_stat), shape = 21, colour= "black", fill = flat$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = expression(bold(paste("Kolmogorov-Smirnov score (", italic("D"), ")"))), title = "Flat-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

flat_plot_1
#---------------------------------

flat_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = flat, aes(x = mid_age, y = rarefied_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = rarefied_ks_stat), shape = 21, colour= "black", fill = flat$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Kolmogorov-Smirnov score (", italic("D"), ")"))), title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

flat_plot_2
#---------------------------------

unimodal_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = unimodal, aes(x = mid_age, y = sampled_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = sampled_ks_stat), shape = 21, colour= "black", fill = unimodal$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Unimodal-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

unimodal_plot_1
#---------------------------------

unimodal_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = unimodal, aes(x = mid_age, y = rarefied_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = rarefied_ks_stat), shape = 21, colour= "black", fill = unimodal$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

unimodal_plot_2
#---------------------------------

bimodal_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = bimodal, aes(x = mid_age, y = sampled_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = sampled_ks_stat), shape = 21, colour= "black", fill = bimodal$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Bimodal-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

bimodal_plot_1
#---------------------------------

bimodal_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-0.075, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -0.075, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.075, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0375, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = bimodal, aes(x = mid_age, y = rarefied_ks_stat), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = rarefied_ks_stat), shape = 21, colour= "black", fill = bimodal$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

bimodal_plot_2
#---------------------------------

p <- ggarrange(flat_plot_1, unimodal_plot_1, bimodal_plot_1,flat_plot_2, unimodal_plot_2, bimodal_plot_2,
               ncol=3, nrow=2, widths = c(1,1,1,1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/KS_plot.png", plot = p, width = 200, height = 95, units = "mm", dpi = 600, scale = 2.2)
