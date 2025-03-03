# Lewis A. Jones
# March 2020
#---------------------------------
library(ggplot2)
library(ggpubr)
library(viridis)
library(grid)
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load stage bins
MST <- read.csv("./results/MST/lat_MST.csv")
MST$MST[is.na(MST$MST)] <- 0
MST$MST[MST$MST == 0] <- NA


#---------------------------------
for(i in 1:nrow(periods)){
  for(j in 1:nrow(stages)){
    if(stages$mid_age[j] <= periods$max_age[i] & stages$mid_age[j] >= periods$min_age[i]){
      stages$periodcol[j] <- paste(periods$color[i])}
  }
}
#---------------------------------


MST_plot <- ggplot() + 
  geom_tile(data = MST, aes(x = mid_age, y = mid, fill = MST, width = duration, height = 15), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits=c(0, 28000), breaks=seq(0,28000,by=4000), labels = seq(0,28000,by=4000)) +
  geom_tile(data = stages, aes(x = mid_age, y = 95, width = duration, height = 10), colour = NA, fill= stages$periodcol)+
  geom_text(data = periods, aes(x = mid_age, y = 95, label = abbr), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 45)) +
  scale_x_reverse(expand=c(0,0), breaks = seq(0, 298.900, 50), limits = c(298.900, 2.588)) +
  coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")")))) +
  theme(
    axis.text.x=element_text(size = 16, vjust = -1, angle = 0),
    axis.text.y=element_text(size = 16, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 16, face = "bold", vjust = -4, colour = "black"),
    axis.title.y=element_text(size = 16, face = "bold", vjust = 4, colour = "white"),
    plot.subtitle = element_text(size = 16, hjust = 0.5, face = "bold"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 10, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3.5), "cm"),
    legend.key.height = unit(c(0.8), "cm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_text(size = 16, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.15,0.15,0.15,0.35), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank())


MST_plot <- MST_plot + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Summed MST length (km)", title.position = "top", title.hjust = 0.5, title.vjust = -7.5))

MST_plot

ggsave("./figures/heat_map_mst.png",width = 210, height = 140, units = "mm", dpi = 900)

