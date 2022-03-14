# Lewis A. Jones
# March 2020

# GSA - until DEMs/shelf area masks available, not possible to plot
# LDGs for 'sampled' or 'rarefied' derivations of simulations

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
# FlatSamp <- read.csv("./results/compiled_LBGs/flat_sampled.csv")
# FlatCR <- read.csv("./results/compiled_LBGs/flat_rarefied.csv")
# FlatCR$prop_richness[which(FlatCR$prop_richness == 0)] <- NA

#---------------------------------
#---------------------------------

UnimodalSim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
# UnimodalSamp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
# UnimodalCR <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
# UnimodalCR$prop_richness[which(UnimodalCR$prop_richness == 0)] <- NA

#---------------------------------
#---------------------------------

BimodalSim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
# BimodalSamp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
# BimodalCR <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")
# BimodalCR$prop_richness[which(BimodalCR$prop_richness == 0)] <- NA

#---------------------------------

FlatSim_plot <- ggplot() + 
  geom_tile(data = FlatSim, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Flat-type", subtitle = "Simulated") +
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
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


FlatSim_plot <- FlatSim_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))

FlatSim_plot

#---------------------------------

# FlatSamp_plot <- ggplot() + 
#   geom_tile(data = FlatSamp, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampled") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = "black"),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# FlatSamp_plot <- FlatSamp_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))
# FlatSamp_plot

#---------------------------------
 
# FlatCR_plot <- ggplot() + 
#   geom_tile(data = FlatCR, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = "black"),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# FlatCR_plot <- FlatCR_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))
# FlatCR_plot

#---------------------------------
#---------------------------------

UnimodalSim_plot <- ggplot() + 
  geom_tile(data = UnimodalSim, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Unimodal-type", subtitle = "Simulated") +
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
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


UnimodalSim_plot <- UnimodalSim_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))

UnimodalSim_plot

#---------------------------------

# UnimodalSamp_plot <- ggplot() + 
#   geom_tile(data = UnimodalSamp, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampled") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# UnimodalSamp_plot <- UnimodalSamp_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))
# UnimodalSamp_plot

#---------------------------------

# UnimodalCR_plot <- ggplot() + 
#   geom_tile(data = UnimodalCR, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# UnimodalCR_plot <- UnimodalCR_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))
# UnimodalCR_plot

#---------------------------------
#---------------------------------

BimodalSim_plot <- ggplot() + 
  geom_tile(data = BimodalSim, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
  geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "Bimodal-type", subtitle = "Simulated") +
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
    legend.key.height = unit(c(0.75), "cm"),
    legend.background = element_blank(),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


BimodalSim_plot <- BimodalSim_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6,  title.hjust = 0.5))

BimodalSim_plot

#---------------------------------

# BimodalSamp_plot <- ggplot() + 
#   geom_tile(data = BimodalSamp, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampled") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = NA),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# BimodalSamp_plot <- BimodalSamp_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6, title.hjust = 0.5))
# BimodalSamp_plot

#---------------------------------

# BimodalCR_plot <- ggplot() + 
#   geom_tile(data = BimodalCR, aes(x = mid_age, y = mid, fill = prop_richness, width = duration, height = 15), colour = "black") +
#   scale_fill_viridis(option = "D", direction = 1, limits=c(0, 1), breaks=seq(0,1,by=0.25), labels = seq(0,1,by=0.25)) +
#   geom_tile(data = stages, aes(x = mid_age, y = 97.5, width = duration, height = 15), colour = NA, fill= stages$periodcol)+
#   geom_text(data = periods, aes(x = mid_age, y = 97.5, label = abbr), color = "black", size = 2.75) +
#   scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
#   scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 3)) +
#   coord_fixed(ratio = 0.8) +
#   labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), title = "", subtitle = "Sampling-standardised") +
#   theme(
#     axis.text.x=element_text(size = 10, vjust = -2, angle = 0),
#     axis.text.y=element_text(size = 10, hjust = 1, angle = 0),
#     axis.title.x=element_text(size = 12, face = "bold", vjust = -4, colour = "black"),
#     axis.title.y=element_text(size = 12, face = "bold", vjust = 4, colour = NA),
#     plot.subtitle = element_text(size = 12, hjust = 0.5, face = "bold"),
#     legend.position="bottom",
#     plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
#     legend.text = element_text(size = 12),
#     legend.key.width = unit(c(3.5), "cm"),
#     legend.key.height = unit(c(0.75), "cm"),
#     legend.background = element_blank(),
#     legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
#     plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
#     panel.background=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank())
# 
# 
# BimodalCR_plot <- BimodalCR_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Proportional richness", title.position = "top", title.vjust = -6, title.hjust = 0.5))
# BimodalCR_plot

#---------------------------------

#---------------------------------

p <- ggarrange(FlatSim_plot, UnimodalSim_plot, BimodalSim_plot, 
               # FlatSamp_plot, UnimodalSamp_plot, BimodalSamp_plot, 
               # FlatCR_plot, UnimodalCR_plot, BimodalCR_plot, 
               ncol=3, # nrow=3, widths = c(1,1,1,1,1,1,1,1,1), 
               common.legend = TRUE, font.label = list(size = 12), legend="bottom", labels = "auto", align = "v", label.x = 0.15)

p
ggsave("./results/GSA/heat_map.png", plot = p, width = 280, height = 75, units = "mm", dpi = 900)
# ggsave("./figures/heat_map.png", plot = p, width = 280, height = 220, units = "mm", dpi = 900)

#ggsave("./figures/heat_map.pdf", plot = p, width = 14.9606, height = 8.26772, useDingbats = FALSE)