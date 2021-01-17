#---------------------------------
library(ggplot2)
library(ggpubr)
library(ggrepel)
#---------------------------------
stages <- read.csv("./data/raw_data/stages.csv")
periods <- read.csv("./data/raw_data/periods.csv")
periods$abbr[1] <- NA
col <- c("#1b9e77", "#d95f02", "#7570b3")
#---------------------------------
data <- read.csv("./results/SLD/LBG_type_SLD.csv")
simulated <- data[,c("simulated_SLD_UB", "mid_age")]
simulated$type <- "Simulated"
names(simulated)[1] <- "SLD"

sampled <- data[,c("sampled_SLD_UB", "mid_age")]
sampled$type <- "Sampled"
names(sampled)[1] <- "SLD"

rarefied <- data[,c("rarefied_SLD_UB", "mid_age")]
rarefied$type <- "Sampling-standardised "
names(rarefied)[1] <- "SLD"

data <- rbind.data.frame(simulated, sampled, rarefied)

vec <- c(40, 39, 38, 20, 17, 12, 9)

highlight <- stages[vec,]
highlight$SLD <- sampled$SLD[vec]
highlight$SLD2 <- rarefied$SLD[vec]
highlight$sld_min <- apply(highlight[,c("SLD", "SLD2")], 1, FUN=min)
#---------------------------------

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

p <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = max_age, xend = max_age, y =0, yend = Inf), linetype = 2, size = 1, color = "grey90") +
  geom_rect(data = throwing_shade, mapping=aes(xmin=min_age, xmax=max_age, ymin=0, ymax= Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin=-0.1, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.1, ymax= 00), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=mid_age, y= -0.05, label = abbr), colour = "black", alpha=1)  +
  #geom_ribbon(data = skewness, aes(ymin = 0, ymax = skew/3, x = mid_age, fill = "Skewness"), colour = NA, alpha = 0.75) +
  geom_line(data = data, aes(x = mid_age, y = SLD, colour = factor(type, levels = c("Simulated", "Sampled", "Sampling-standardised "))), size = 1.2, alpha = 0.9) +
  geom_text_repel(data = highlight, aes(x = mid_age, y = sld_min, label = stringr::str_to_title(name)),
                  nudge_y       = -0.17,
                  nudge_x       = c(-30, -30, 30, -5, 2, 2, 2),
                  segment.size  = 0.6,
                  segment.color = "grey50",
                  direction     = "x") +
  scale_colour_manual(values=c(col[1], col[2], col[3]), guide = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values=c("darkgrey")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.1, 2)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Total displacement (", italic("D"), ")"))), title = "") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.893, 0.9195),
        #legend.background = element_rect(colour = "black", fill = "white"),
        legend.background = element_blank(),
        #legend.key.size = unit(0.5, "cm"),
        #legend.key.width = unit(0.7, "cm"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.margin=unit(2, "cm"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 4),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.6)

p

ggsave(file="./figures/SLD_LBG_type.png", plot = p, width = 190, height = 130, units = "mm", dpi = 600, scale = 1.25)
#ggsave(file="./figures/SLD_LBG_type.pdf", plot = p, width = 190, height = 130, units = "mm", dpi = 300, scale = 1.25)
