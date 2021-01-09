# Lewis A. Jones
# March 2020
#---------------------------------
library(ggplot2)
library(ggpubr)
library(viridis)
library(grid)
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load stage bins

#---------------------------------
for(i in 1:nrow(periods)){
  for(j in 1:nrow(stages)){
    if(stages$mid_age[j] <= periods$max_age[i] & stages$mid_age[j] >= periods$min_age[i]){
      stages$periodcol[j] <- paste(periods$color[i])}
  }
}
#---------------------------------

FlatSim <- read.csv("./results/compiled_LBGs/flat_simulated.csv")
FlatSamp <- read.csv("./results/compiled_LBGs/flat_sampled.csv")
FlatCR <- read.csv("./results/compiled_LBGs/flat_rarefied.csv")
FlatCR$prop_richness[which(FlatCR$prop_richness == 0)] <- NA
FlatSim$SimSamp <- FlatSamp$prop_richness - FlatSim$prop_richness
FlatSim$SimRare <- FlatCR$prop_richness - FlatSim$prop_richness

#---------------------------------
#---------------------------------

UnimodalSim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
UnimodalSamp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
UnimodalCR <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
UnimodalCR$prop_richness[which(UnimodalCR$prop_richness == 0)] <- NA
UnimodalSim$SimSamp <- UnimodalSamp$prop_richness - UnimodalSim$prop_richness
UnimodalSim$SimRare <- UnimodalCR$prop_richness - UnimodalSim$prop_richness


#---------------------------------
#---------------------------------

BimodalSim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
BimodalSamp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
BimodalCR <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")
BimodalCR$prop_richness[which(BimodalCR$prop_richness == 0)] <- NA
BimodalSim$SimSamp <- BimodalSamp$prop_richness - BimodalSim$prop_richness 
BimodalSim$SimRare <-  BimodalCR$prop_richness - BimodalSim$prop_richness

#---------------------------------

FlatSimSamp <- ggplot() + 
  geom_tile(data = FlatSim, aes(x = mid_age, y = mid, fill = SimSamp, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Flat-type", subtitle = "Sampled") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


FlatSimSamp <- FlatSimSamp + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

FlatSimSamp

#---------------------------------

FlatSimRare <- ggplot() + 
  geom_tile(data = FlatSim, aes(x = mid_age, y = mid, fill = SimRare, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


FlatSimRare <- FlatSimRare + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

FlatSimRare

#---------------------------------
UnimodalSimSamp <- ggplot() + 
  geom_tile(data = UnimodalSim, aes(x = mid_age, y = mid, fill = SimSamp, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Unimodal-type", subtitle = "Sampled") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


UnimodalSimSamp <- UnimodalSimSamp + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

UnimodalSimSamp

#---------------------------------

UnimodalSimRare <- ggplot() + 
  geom_tile(data = UnimodalSim, aes(x = mid_age, y = mid, fill = SimRare, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


UnimodalSimRare <- UnimodalSimRare + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

UnimodalSimRare

#---------------------------------
BimodalSimSamp <- ggplot() + 
  geom_tile(data = BimodalSim, aes(x = mid_age, y = mid, fill = SimSamp, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Bimodal-type", subtitle = "Sampled") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


BimodalSimSamp <- BimodalSimSamp + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

BimodalSimSamp

#---------------------------------

BimodalSimRare <- ggplot() + 
  geom_tile(data = BimodalSim, aes(x = mid_age, y = mid, fill = SimRare, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.2), labels = seq(-0.3,0.7,by=0.2)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.5), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 12, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


BimodalSimRare <- BimodalSimRare + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Residual proportional richness", title.position = "top", title.vjust = -5.5,  title.hjust = 0.5))

BimodalSimRare

#---------------------------------
#---------------------------------

p <- ggarrange(FlatSimSamp, UnimodalSimSamp,  BimodalSimSamp, FlatSimRare, UnimodalSimRare,
               BimodalSimRare,
               ncol=3, nrow=2, widths = c(1,1,1,1,1,1), common.legend = TRUE, font.label = list(size = 12), legend="bottom", labels = "auto", align = "v", label.x = 0.15)

p
ggsave("./figures/heat_map_residual.png", plot = p, width = 300, height = 160, units = "mm", dpi = 600)
#ggsave("./figures/heat_map_residual.pdf", plot = p, width = 14.9606, height = 8.26772, useDingbats = FALSE)
