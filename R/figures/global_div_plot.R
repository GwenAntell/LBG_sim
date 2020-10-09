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
ssc <- read.csv("./results/SSC/global_SSC.csv")
source("./R/functions/equations.R")
#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

fit <- lm(flat$median_richness~ssc$ssc)

flat_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 400), linetype = 0, color="grey90", alpha=0.1)  +
  geom_ribbon(data = ssc, aes(ymin = 0, ymax = ssc*60, x = mid_age), fill = col[3], alpha = 0.45) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 400), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), colour = "black", alpha=1)  +
  geom_ribbon(data = flat, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = flat, aes(x = mid_age, y = median_richness), colour = col[1], size = 1.2) +
  geom_point(data = flat, aes(x = mid_age, y = median_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  annotate("text", x = 245, y = 380, label = equation(fit), parse = TRUE) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Sampled richness", title = "Flat-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, hjust = 0.5),
        aspect.ratio = 0.6)

flat_plot
flat_plot <- flat_plot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 400, 100), labels = seq(0, 400, 100), limits = c(-20, 400), sec.axis = sec_axis(~./60, name = "SSC (%)"))
flat_plot

fit <- lm(unimodal$median_richness~ssc$ssc)

unimodal_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 400), linetype = 0, color="grey90", alpha=0.1)  +
  geom_ribbon(data = ssc, aes(ymin = 0, ymax = ssc*60, x = mid_age), fill = col[3], alpha = 0.45) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 400), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), colour = "black", alpha=1)  +
  geom_ribbon(data = unimodal, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = unimodal, aes(x = mid_age, y = median_richness), colour = col[1], size = 1.2) +
  geom_point(data = unimodal, aes(x = mid_age, y = median_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  annotate("text", x = 245, y = 380, label = equation(fit), parse = TRUE) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "", y = "Sampled richness", title = "Unimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, hjust = 0.5),
        aspect.ratio = 0.6)

unimodal_plot
unimodal_plot <- unimodal_plot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 400, 100), labels = seq(0, 400, 100), limits = c(-20, 400), sec.axis = sec_axis(~./60, name = "SSC (%)"))
unimodal_plot

fit <- lm(bimodal$median_richness~ssc$ssc)

bimodal_plot <- ggplot() +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=-20, ymax= 400), linetype = 0, color="grey90", alpha=0.1)  +
  geom_ribbon(data = ssc, aes(ymin = 0, ymax = ssc*60, x = mid_age), fill = col[3], alpha = 0.45) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -20, yend = 400), linetype = 2, size = 1, color = "grey80") +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -20, ymax= 0), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -10, label = abbr), size = 3.5, colour = "black", alpha=1)  +
  geom_ribbon(data = bimodal, aes(ymin = CI.Lower, ymax = CI.Upper, x = mid_age), fill = col[1], alpha = 0.45) +
  geom_line(data = bimodal, aes(x = mid_age, y = median_richness), colour = col[1], size = 1.2) +
  geom_point(data = bimodal, aes(x = mid_age, y = median_richness), shape = 21, colour= "black", fill = col[1], size = 3) +
  annotate("text", x = 245, y = 380, label = equation(fit), parse = TRUE) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 300, 50), labels = seq(0, 300, 50)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Time (Ma)", y = "Sampled richness", title = "Bimodal-type") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.left = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 16, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, hjust = 0.5),
        aspect.ratio = 0.6)

bimodal_plot
bimodal_plot <- bimodal_plot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 400, 100), labels = seq(0, 400, 100), limits = c(-20, 400), sec.axis = sec_axis(~./60, name = "SSC (%)"))
bimodal_plot


p <- ggarrange(flat_plot, unimodal_plot, bimodal_plot,
               ncol=1, nrow=3, widths = c(1,1,1), labels = "AUTO", align = "v", font.label = list(size = 18), label.x = 0.07)

p
ggsave("./figures/global_div_plot.png", plot = p, width = 100, height = 190, units = "mm", dpi = 300, scale = 1.7)
