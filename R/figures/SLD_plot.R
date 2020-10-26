#---------------------------------
library(ggplot2)
library(ggpubr)
#---------------------------------
stages <- read.csv("./data/raw_data/stages.csv")
periods <- read.csv("./data/raw_data/periods.csv")
periods$abbr[1] <- NA
col <- c("#d95f02", "#7570b3")
#---------------------------------
data <- read.csv("./results/SLD/flat_temporal_SLD.csv")

sampled <- data[,c("sampled_SLD", "mid_age")]
sampled$type <- "Sampled"
names(sampled)[1] <- "SLD"

rarefied <- data[,c("rarefied_SLD", "mid_age")]
rarefied$type <- "Sampling-standardised"
names(rarefied)[1] <- "SLD"

data <- rbind.data.frame(sampled, rarefied)
#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

p1 <- ggplot(data = data, aes(colour = type)) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = max_age, xend = max_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=0, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-0.15, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.15, ymax= 00), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -0.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = data, aes(x = mid_age, y = SLD, colour = factor(type, levels = c("Simulated", "Sampled", "Sampling-standardised"))), size = 1.2, alpha = 0.9) +
  scale_colour_manual(values=c(col[1], col[2], col[3]), guide = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values=c("darkgrey")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.15, 2)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Total displacement (", italic("D"), ")"))), title = "Flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)

p1

#---------------------------------
#---------------------------------
data <- read.csv("./results/SLD/unimodal_temporal_SLD.csv")

sampled <- data[,c("sampled_SLD", "mid_age")]
sampled$type <- "Sampled"
names(sampled)[1] <- "SLD"

rarefied <- data[,c("rarefied_SLD", "mid_age")]
rarefied$type <- "Sampling-standardised"
names(rarefied)[1] <- "SLD"

data <- rbind.data.frame(sampled, rarefied)
#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

p2 <- ggplot(data = data, aes(colour = type)) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = max_age, xend = max_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=0, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-0.15, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.15, ymax= 00), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -0.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = data, aes(x = mid_age, y = SLD, colour = factor(type, levels = c("Simulated", "Sampled", "Sampling-standardised"))), size = 1.2, alpha = 0.9) +
  scale_colour_manual(values=c(col[1], col[2], col[3]), guide = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values=c("darkgrey")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.15, 2)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Total displacement (", italic("D"), ")"))), title = "Unimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)

p2

#---------------------------------
#---------------------------------
data <- read.csv("./results/SLD/bimodal_temporal_SLD.csv")

sampled <- data[,c("sampled_SLD", "mid_age")]
sampled$type <- "Sampled"
names(sampled)[1] <- "SLD"

rarefied <- data[,c("rarefied_SLD", "mid_age")]
rarefied$type <- "Sampling-standardised"
names(rarefied)[1] <- "SLD"

data <- rbind.data.frame(sampled, rarefied)
#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

p3 <- ggplot(data = data, aes(colour = type)) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = max_age, xend = max_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=0, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-0.15, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.15, ymax= 00), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -0.075, label = abbr), colour = "black", alpha=1)  +
  geom_line(data = data, aes(x = mid_age, y = SLD, colour = factor(type, levels = c("Simulated", "Sampled", "Sampling-standardised"))), size = 1.2, alpha = 0.9) +
  scale_colour_manual(values=c(col[1], col[2], col[3]), guide = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values=c("darkgrey")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.15, 2)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Total displacement (", italic("D"), ")"))), title = "Bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.margin=unit(0, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)

p3

#---------------------------------
p <- ggarrange(p1, p2, p3,
               ncol=1, nrow=3, widths = c(1,1,1), labels = "auto", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave(file="./figures/SLD_LBG.png", plot = p, width = 100, height = 210, units = "mm", dpi = 300, scale = 1.7)
#ggsave(file="./figures/SLD_LBG_type.pdf", plot = p, width = 190, height = 130, units = "mm", dpi = 300, scale = 1.25)
