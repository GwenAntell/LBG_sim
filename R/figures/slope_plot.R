# Lewis A. Jones
#---------------------------------
library(ggplot2)
library(ggpubr)
library(cowplot)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
unimodal <- read.csv("./results/slope/unimodal_slope_estimates.csv")
bimodal <- read.csv("./results/slope/bimodal_slope_estimates.csv")

periods$abbr[1] <- NA
#---------------------------------
seq <- seq(2, 56, 2)
rect <- stages[seq,]

A <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (SimulatedN-SimulatedN.sig), ymax = (SimulatedN+SimulatedN.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (SimulatedN-SimulatedN.sig), ymax = (SimulatedN+SimulatedN.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (SimulatedN), col = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (SimulatedN), col = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (SimulatedN)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (SimulatedN)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "", y = "", title = "Northern Hemisphere", subtitle = "Simulated") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

A

B <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (SimulatedS-SimulatedS.sig), ymax = (SimulatedS+SimulatedS.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (SimulatedS-SimulatedS.sig), ymax = (SimulatedS+SimulatedS.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (SimulatedS), col = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (SimulatedS), col = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (SimulatedS)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (SimulatedS)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "", y = "", title = "Southern Hemisphere", subtitle = "Simulated") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

B

C <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (SampledN-SampledN.sig), ymax = (SampledN+SampledN.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (SampledN-SampledN.sig), ymax = (SampledN+SampledN.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (SampledN), col = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (SampledN), col = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (SampledN)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (SampledN)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "", y = "Slope estimate of proportional latitudinal richness", title = "", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 3),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        aspect.ratio = 0.5)

C

D <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (SampledS-SampledS.sig), ymax = (SampledS+SampledS.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (SampledS-SampledS.sig), ymax = (SampledS+SampledS.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (SampledS), col = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (SampledS), col = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (SampledS)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (SampledS)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "", y = "", title = "", subtitle = "Sampled") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

D

E <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (RarefiedN-RarefiedN.sig), ymax = (RarefiedN+RarefiedN.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (RarefiedN-RarefiedN.sig), ymax = (RarefiedN+RarefiedN.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (RarefiedN), col = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (RarefiedN), col = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (RarefiedN)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (RarefiedN)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        plot.background = element_rect(fill = NA, color = NA),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

E

Fplot <- ggplot() +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -Inf, yend = Inf), linetype = 2, size = 1.25, color = "grey90") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = rect, mapping=aes(xmin=min_age, xmax=max_age, ymin = -Inf, ymax = Inf), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin=300, xmax=0, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin=min_age, xmax=max_age, ymin= -0.045, ymax= -0.04), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_age+max_age)/2, y= -0.0425, label = abbr), size = 3, colour = "black", alpha=1)  +
  scale_colour_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  scale_fill_manual(values=c(col[1], col[2]), labels = c("Unimodal", "Bimodal")) +
  geom_ribbon(data = unimodal, aes(x = mid_age, ymin = (RarefiedS-RarefiedS.sig), ymax = (RarefiedS+RarefiedS.sig)), colour = NA, fill = col[1], size = 1.25, alpha = 0.5) +
  geom_ribbon(data = bimodal, aes(x = mid_age, ymin = (RarefiedS-RarefiedS.sig), ymax = (RarefiedS+RarefiedS.sig)), colour = NA, fill = col[2], size = 1.25, alpha = 0.5) +
  geom_line(data = unimodal, aes(x = mid_age, y = (RarefiedS), colour = col[1]), size = 1.25, alpha = 0.95) +
  geom_line(data = bimodal, aes(x = mid_age, y = (RarefiedS), colour = col[2]), size = 1.25, alpha = 0.95) +
  #geom_point(data = unimodal, aes(x = mid_age, y = (RarefiedS)), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 0.95) +
  #geom_point(data = bimodal, aes(x = mid_age, y = (RarefiedS)), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 0.95) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), limits = c(-0.045, 0.04), breaks = seq(-0.04, 0.04, 0.02), labels = seq(-0.04, 0.04, 0.02)) +
  labs(x = "Time (Ma)", y = "", title = "", subtitle = "Sampling-standardised") +
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = NA, color = NA),
        plot.margin = margin(0.125,0.125,0.125,0.125, "cm"),
        #panel.grid.minor.y = element_line(colour = "grey90"),
        #panel.grid.minor.x = element_line(colour = "grey90"),
        #panel.grid.major.y = element_line(colour = "grey90"),
        #panel.grid.major.x = element_line(colour = "grey90"),
        legend.position = c(0.15, 0.9),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 1),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = 0),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.5)

Fplot

plot <- plot_grid(A, B, C, D, E, Fplot, labels="auto", ncol=2, nrow = 3, align = "v", font.label = list(size = 14), label_x = .125, label_y = 1.01)
#plot

ggsave("./figures/slope_plot.png", plot = plot, width = 165, height = 150, units = "mm", dpi = 600, scale = 1.75)
#ggsave("./figures/slope_plot.pdf", plot = plot, width = 180, height = 150, units = "mm", dpi = 600, scale = 2)

