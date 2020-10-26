#pearson_plot
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods$abbr[1] <- NA

unimodal <- read.csv("./results/corr_test/unimodal_temporal_corr_test.csv")
unimodal$sig_samp <- NA
unimodal$sig_samp[which(unimodal$sampled_corr_sig <= 0.05)] <- col[1]

unimodal$sig_rare <- NA
unimodal$sig_rare[which(unimodal$rarefied_corr_sig <= 0.05)] <- col[1]

bimodal <- read.csv("./results/corr_test/bimodal_temporal_corr_test.csv")
bimodal$sig_samp <- NA
bimodal$sig_samp[which(bimodal$sampled_corr_sig <= 0.05)] <- col[1]

bimodal$sig_rare <- NA
bimodal$sig_rare[which(bimodal$rarefied_corr_sig <= 0.05)] <- col[1]

flat <- read.csv("./results/corr_test/flat_temporal_corr_test.csv")
flat$sig_samp <- NA
flat$sig_samp[which(flat$sampled_corr_sig <= 0.05)] <- col[1]

flat$sig_rare <- NA
flat$sig_rare[which(flat$rarefied_corr_sig <= 0.05)] <- col[1]

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]
#---------------------------------

flat_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = flat, aes(x = mid_age, y = sampled_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = sampled_corr_stat), shape = 21, colour= "black", fill = flat$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = expression(bold(paste("Pearson's coefficient (", italic("r"), ")"))), title = "Flat-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

flat_plot_1
#---------------------------------

flat_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = flat, aes(x = mid_age, y = rarefied_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = rarefied_corr_stat), shape = 21, colour= "black", fill = flat$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Pearson's coefficient (", italic("r"), ")"))), title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

flat_plot_2
#---------------------------------

unimodal_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = unimodal, aes(x = mid_age, y = sampled_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = sampled_corr_stat), shape = 21, colour= "black", fill = flat$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Unimodal-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

unimodal_plot_1
#---------------------------------

unimodal_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = unimodal, aes(x = mid_age, y = rarefied_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = rarefied_corr_stat), shape = 21, colour= "black", fill = flat$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

unimodal_plot_2
#---------------------------------

bimodal_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = bimodal, aes(x = mid_age, y = sampled_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = sampled_corr_stat), shape = 21, colour= "black", fill = flat$sig_samp, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Bimodal-type", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

bimodal_plot_1
#---------------------------------

bimodal_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = bimodal, aes(x = mid_age, y = rarefied_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = rarefied_corr_stat), shape = 21, colour= "black", fill = flat$sig_rare, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

bimodal_plot_2
#---------------------------------

p <- ggarrange(flat_plot_1, unimodal_plot_1, bimodal_plot_1,flat_plot_2, unimodal_plot_2, bimodal_plot_2,
               ncol=3, nrow=2, widths = c(1,1,1,1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/pearson_corr_plot.png", plot = p, width = 290, height = 135, units = "mm", dpi = 600, scale = 1.7)

#---------------------------------
#TYPE PLOTS
#---------------------------------
#unimodal and flat

type <- read.csv("./results/corr_test/LBG_type_UF_temporal_corr_test.csv")
type$sim_sig <- NA
type$sim_sig[which(type$sim_corr_sig <= 0.05)] <- col[1]

type$samp_sig <- NA
type$samp_sig[which(type$samp_corr_sig <= 0.05)] <- col[2]

type$rare_sig <- NA
type$rare_sig[which(type$rare_corr_sig <= 0.05)] <- col[3]
#---------------------------------

type_plot_1 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = sim_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = sim_corr_stat), shape = 21, colour= "black", fill = type$sim_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = expression(bold(paste("Pearson's coefficient (", italic("r"), ")"))), title = "Simulated", subtitle = "Unimodal- and flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_1
#---------------------------------
#---------------------------------

type_plot_2 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Sampled", subtitle = "Unimodal- and flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_2
#---------------------------------

type_plot_3 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "Sampling-standardised", subtitle = "Unimodal- and flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 2),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_3
#---------------------------------
#unimodal and bimodal

type <- read.csv("./results/corr_test/LBG_type_UB_temporal_corr_test.csv")
type$sim_sig <- NA
type$sim_sig[which(type$sim_corr_sig <= 0.05)] <- col[1]

type$samp_sig <- NA
type$samp_sig[which(type$samp_corr_sig <= 0.05)] <- col[2]

type$rare_sig <- NA
type$rare_sig[which(type$rare_corr_sig <= 0.05)] <- col[3]
#---------------------------------

type_plot_4 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = sim_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = sim_corr_stat), shape = 21, colour= "black", fill = type$sim_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = expression(bold(paste("Pearson's coefficient (", italic("r"), ")"))), title = "", subtitle = "Unimodal- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_4
#---------------------------------
#---------------------------------

type_plot_5 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "", subtitle = "Unimodal- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_5
#---------------------------------

type_plot_6 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "", title = "", subtitle = "Unimodal- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_6
#---------------------------------
#bimodal and flat

type <- read.csv("./results/corr_test/LBG_type_BF_temporal_corr_test.csv")
type$sim_sig <- NA
type$sim_sig[which(type$sim_corr_sig <= 0.05)] <- col[1]

type$samp_sig <- NA
type$samp_sig[which(type$samp_corr_sig <= 0.05)] <- col[2]

type$rare_sig <- NA
type$rare_sig[which(type$rare_corr_sig <= 0.05)] <- col[3]
#---------------------------------

type_plot_7 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = sim_corr_stat), colour = col[1], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = sim_corr_stat), shape = 21, colour= "black", fill = type$sim_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Pearson's coefficient (", italic("r"), ")"))), title = "", subtitle = "Flat- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_7
#---------------------------------
#---------------------------------

type_plot_8 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  #geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Flat- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_8
#---------------------------------

type_plot_9 <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-1.15, ymax= 1.1), linetype = 0, color="grey90", alpha=0.1)  +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -1.15, yend = 1.1), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -1.15, ymax= -1), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -1.075, label = abbr), colour = "black", alpha=1)  +
  #geom_line(data = type, aes(x = mid_age, y = samp_corr_stat), colour = col[2], size = 1.2) +
  #geom_point(data = type, aes(x = mid_age, y = samp_corr_stat), shape = 21, colour= "black", fill = type$samp_sig, size = 2.5) +
  geom_line(data = type, aes(x = mid_age, y = rare_corr_stat), colour = col[3], size = 1.2) +
  geom_point(data = type, aes(x = mid_age, y = rare_corr_stat), shape = 21, colour= "black", fill = type$rare_sig, size = 2.5) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Flat- and bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.25,0.25,0.25, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        aspect.ratio = 0.6)

type_plot_9

#---------------------------------

p <- ggarrange(type_plot_1, type_plot_2, type_plot_3,type_plot_4, type_plot_5, type_plot_6,type_plot_7, type_plot_8, type_plot_9,
               ncol=3, nrow=3, widths = c(1,1,1,1,1,1,1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)


ggsave("./figures/pearson_corr_type_plot.png", plot = p, width = 300, height = 210, units = "mm", dpi = 600, scale = 1.5)
 
