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

FlatSimSamp <- FlatSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
FlatSimCR <- FlatSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
FlatSimSamp$difference <- FlatSamp$prop_richness - FlatSim$prop_richness 
FlatSimCR$difference <- FlatCR$prop_richness  - FlatSim$prop_richness 

#---------------------------------
#---------------------------------

UnimodalSim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
UnimodalSamp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
UnimodalCR <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
UnimodalCR$prop_richness[which(UnimodalCR$prop_richness == 0)] <- NA

UnimodalSimSamp <- UnimodalSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
UnimodalSimCR <- UnimodalSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
UnimodalSimSamp$difference <- UnimodalSamp$prop_richness - UnimodalSim$prop_richness 
UnimodalSimCR$difference <- UnimodalCR$prop_richness  - UnimodalSim$prop_richness 

#---------------------------------
#---------------------------------

BimodalSim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
BimodalSamp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
BimodalCR <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")
BimodalCR$prop_richness[which(BimodalCR$prop_richness == 0)] <- NA

BimodalSimSamp <- BimodalSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
BimodalSimCR <- BimodalSim[,c("name", "max_age", "mid_age", "min_age","duration", "abbr", "color", "bin", "max", "mid", "min", "hemisphere")]
BimodalSimSamp$difference <- BimodalSamp$prop_richness - BimodalSim$prop_richness 
BimodalSimCR$difference <- BimodalCR$prop_richness  - BimodalSim$prop_richness 

#---------------------------------
  
FlatSimSampHeat <- ggplot() + 
    geom_tile(data = FlatSimSamp, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
    scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
    geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
    geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
    scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
    scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
    coord_fixed(ratio = 0.8) +
    labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampled", title = "Flat-type") +
    theme(
      axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
      axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
      axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = NA),
      axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = "black"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
      legend.position="bottom",
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.width = unit(c(3.5), "cm"),
      legend.key.height = unit(c(0.75), "cm"),
      legend.background = element_blank(),
      legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
      plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
      panel.background=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())
  
  
FlatSimSampHeat <- FlatSimSampHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
FlatSimSampHeat

#---------------------------------

FlatSimCRHeat <- ggplot() + 
  geom_tile(data = FlatSimCR, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampling-standardised", title = "") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = "black"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


FlatSimCRHeat <- FlatSimCRHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
FlatSimCRHeat

#---------------------------------

UnimodalSimSampHeat <- ggplot() + 
  geom_tile(data = UnimodalSimSamp, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampled", title = "Unimodal-type") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = NA),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


UnimodalSimSampHeat <- UnimodalSimSampHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
UnimodalSimSampHeat

#---------------------------------

UnimodalSimCRHeat <- ggplot() + 
  geom_tile(data = UnimodalSimCR, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampling-standardised", title = "") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


UnimodalSimCRHeat <- UnimodalSimCRHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
UnimodalSimCRHeat

#---------------------------------

BimodalSimSampHeat <- ggplot() + 
  geom_tile(data = BimodalSimSamp, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampled", title = "Bimodal-type") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = NA),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


BimodalSimSampHeat <- BimodalSimSampHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
BimodalSimSampHeat

#---------------------------------

BimodalSimCRHeat <- ggplot() + 
  geom_tile(data = BimodalSimCR, aes(x = mid_age, y = mid, fill = difference, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(-0.3, 0.7), breaks=seq(-0.3,0.7,by=0.25), labels = seq(-0.3,0.7,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), subtitle = "Sampling-standardised", title = "") +
  theme(
    axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
    axis.text.y=element_text(size = 10, hjust = 0, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = NA),
    plot.subtitle = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


BimodalSimCRHeat <- BimodalSimCRHeat + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness change", title.position = "top", title.hjust = 0.5))
BimodalSimCRHeat
#---------------------------------

p <- ggarrange(FlatSimSampHeat, UnimodalSimSampHeat, BimodalSimSampHeat,
               FlatSimCRHeat, UnimodalSimCRHeat, BimodalSimCRHeat,
               ncol=3, nrow=2, widths = c(1,1,1,1,1,1), common.legend = TRUE, legend="bottom", labels = "AUTO", align = "v")

p

png("./figures/fig_3.png",width = 380, height = 210, units = "mm", res = 300)
p
grid.text("Under-represented", x = unit(0.20, "npc"), y = unit(0.058, "npc"), gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Over-represented", x = unit(0.795, "npc"), y = unit(0.058, "npc"), gp = gpar(fontsize = 14, fontface = "bold"))
dev.off()

pdf("./figures/fig_3.pdf", width = 14.9606, height = 8.26772, useDingbats = FALSE)
p
grid.text("Under-represented", x = unit(0.20, "npc"), y = unit(0.058, "npc"), gp = gpar(fontsize = 14, fontface = "bold"))
grid.text("Over-represented", x = unit(0.795, "npc"), y = unit(0.058, "npc"), gp = gpar(fontsize = 14, fontface = "bold"))
dev.off()
