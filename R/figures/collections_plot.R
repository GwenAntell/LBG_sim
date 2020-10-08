# Lewis A. Jones
#---------------------------------
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(cowplot)
library(plyr)
#---------------------------------
col <- c("#1b9e77", "#d95f02", "#7570b3") #colours for plots
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins for plotting

files <- list.files(path="./data/raw_data/binned_collections/masked/", pattern = ".csv")
#---------------------------------

collections <- data.frame()
for(i in files){
  name <- tools::file_path_sans_ext(i)
  tmp <- read.csv(paste("./data/raw_data/binned_collections/masked/", i, sep = ""))
  tmp$binned_interval <- name
  collections <- rbind.data.frame(collections, tmp)
}


collections <- subset(collections, !is.na(lat) | !is.na(lng)) #remove collections without coordinates
collections <- subset(collections, !is.na(paleolat) | !is.na(paleolng)) #remove collections without coordinates

#---------------------------------

#compute modern centroid and bin data
modern <- lapply(1:length(stages$name), function(f){
  name <- as.character(stages$name[f])
  tmp <- subset(collections, binned_interval == name)
  xy <- tmp[,c("lng", "lat")]
  xyN <- subset(xy, lat >= 0)
  xyS <- subset(xy, lat <= 0)
  
  cent.N <- median(xyN$lat)
  quant <- quantile(xyN$lat, probs = c(0.025, 0.975))
  CI.Lower.N <- quant[1]
  CI.Upper.N <- quant[2]
  
  cent.S <- median(xyS$lat)
  quant <- quantile(xyS$lat, probs = c(0.025, 0.975))
  CI.Lower.S <- quant[1]
  CI.Upper.S <- quant[2]
  
  
  cent <- cbind.data.frame(cent.N, CI.Lower.N, CI.Upper.N, cent.S, CI.Lower.S, CI.Upper.S, name)
  cent
})

modern <- do.call(rbind.data.frame, modern)
modern <- join(x = stages, y = modern, by = "name", match = "all") #join data to stage info
#---------------------------------

#compute palaeo centroid and bin data
palaeo <- lapply(1:length(stages$name), function(f){
  name <- as.character(stages$name[f])
  tmp <- subset(collections, binned_interval == name)
  xy <- tmp[,c("paleolng", "paleolat")]
  xyN <- subset(xy, paleolat >= 0)
  xyS <- subset(xy, paleolat <= 0)
  
  cent.N <- median(xyN$paleolat)
  quant <- quantile(xyN$paleolat, probs = c(0.025, 0.975))
  CI.Lower.N <- quant[1]
  CI.Upper.N <- quant[2]
  
  cent.S <- median(xyS$paleolat)
  quant <- quantile(xyS$paleolat, probs = c(0.025, 0.975))
  CI.Lower.S <- quant[1]
  CI.Upper.S <- quant[2]
  
  
  cent <- cbind.data.frame(cent.N, CI.Lower.N, CI.Upper.N, cent.S, CI.Lower.S, CI.Upper.S, name)
  cent
})

palaeo <- do.call(rbind.data.frame, palaeo)
palaeo <- join(x = stages, y = palaeo, by = "name", match = "all") #join data to stage info
#---------------------------------

modern$type <- c("Latitude")
palaeo$type <- c("Palaeolatitude")
cent <- rbind.data.frame(modern, palaeo)
#---------------------------------
#Centroid plot

s <- seq(2, 56, 2)
throwing_shade <- stages[s,]

centroidplot <- ggplot(data = cent, aes(colour = type)) +
  geom_segment(data = periods, mapping=aes(x = min_age, xend = min_age, y = -110, yend = 90), linetype = 2, size = 1.25, color = "grey90") +
  geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y = 0, yend = 0), linetype = 1, size = 0.5, color = "black") +
  #geom_segment(data = periods, mapping=aes(x = 300, xend = 0, y =0, yend = 0), linetype = 1, size = 1, color = "black") +
  geom_rect(data = throwing_shade, mapping=aes(xmin = min_age, xmax = max_age, ymin = -110, ymax = 90), linetype = 0, color="grey90", alpha=0.1)  +
  geom_rect(data = periods, mapping=aes(xmin = 300, xmax=0, ymin= -110, ymax= -90), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = periods, mapping=aes(xmin = min_age, xmax= max_age, ymin= -110, ymax= -90), linetype = 1, colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x = mid_age, y= -100, label = abbr), colour = "black", alpha=1)  +

  geom_line(data = cent, aes(x = mid_age, y = cent.N, colour = type), size = 1.2, alpha = 1) +
  geom_line(data = cent, aes(x = mid_age, y = cent.S, colour = type), size = 1.2, alpha = 1) +

  scale_colour_manual(values=c(col[1], col[2]), labels = c("Modern latitudinal centroid", "Palaeolatitudinal centroid")) +
  scale_x_reverse(expand=c(0,0), limits = c(300, 0)) +
  scale_y_continuous(expand=c(0,0), breaks = seq(-90, 90, 30)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Latitude (",degree,")"))), title = "") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.25,0.25,1,0.25, "cm"),
        legend.position = "top",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA, fill = NA),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
        axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        aspect.ratio = 0.3)

centroidplot
#---------------------------------
library(raster)
library(sp)
library(mapproj)
#---------------------------------

ras <- raster(res = 0.5)
#---------------------------------
#load rasters and aggregate

PermianRas <- raster("./data/rasters/Wordian.nc")
PermianRas <- resample(PermianRas, ras)
PermianRas[PermianRas < 200] <- 1
PermianRas[PermianRas >= 200] <- 2
PermianRas[is.na(PermianRas)] <- 0
PermianRas <- as(PermianRas, "SpatialPixelsDataFrame")
PermianRas <- as.data.frame(PermianRas)
colnames(PermianRas) <- c("value", "x", "y")

TriassicRas <- raster("./data/rasters/Ladinian.nc")
TriassicRas <- resample(TriassicRas, ras)
TriassicRas[TriassicRas < 200] <- 1
TriassicRas[TriassicRas >= 200] <- 2
TriassicRas[is.na(TriassicRas)] <- 0
TriassicRas <- as(TriassicRas, "SpatialPixelsDataFrame")
TriassicRas <- as.data.frame(TriassicRas)
colnames(TriassicRas) <- c("value", "x", "y")

JurassicRas <- raster("./data/rasters/Aalenian.nc")
JurassicRas <- resample(JurassicRas, ras)
JurassicRas[JurassicRas < 200] <- 1
JurassicRas[JurassicRas >= 200] <- 2
JurassicRas[is.na(JurassicRas)] <- 0
JurassicRas <- as(JurassicRas, "SpatialPixelsDataFrame")
JurassicRas <- as.data.frame(JurassicRas)
colnames(JurassicRas) <- c("value", "x", "y")

CretaceousRas <- raster("./data/rasters/Albian.nc")
CretaceousRas <- resample(CretaceousRas, ras)
CretaceousRas[CretaceousRas < 200] <- 1
CretaceousRas[CretaceousRas >= 200] <- 2
CretaceousRas[is.na(CretaceousRas)] <- 0
CretaceousRas <- as(CretaceousRas, "SpatialPixelsDataFrame")
CretaceousRas <- as.data.frame(CretaceousRas)
colnames(CretaceousRas) <- c("value", "x", "y")

PaleogeneRas <- raster("./data/rasters/Lutetian.nc")
PaleogeneRas <- resample(PaleogeneRas, ras)
PaleogeneRas[PaleogeneRas < 200] <- 1
PaleogeneRas[PaleogeneRas >= 200] <- 2
PaleogeneRas[is.na(PaleogeneRas)] <- 0
PaleogeneRas <- as(PaleogeneRas, "SpatialPixelsDataFrame")
PaleogeneRas <- as.data.frame(PaleogeneRas)
colnames(PaleogeneRas) <- c("value", "x", "y")

NeogeneRas <- raster("./data/rasters/Serravallian.nc")
NeogeneRas <- resample(NeogeneRas, ras)
NeogeneRas[NeogeneRas < 200] <- 1
NeogeneRas[NeogeneRas >= 200] <- 2
NeogeneRas[is.na(NeogeneRas)] <- 0
NeogeneRas <- as(NeogeneRas, "SpatialPixelsDataFrame")
NeogeneRas <- as.data.frame(NeogeneRas)
colnames(NeogeneRas) <- c("value", "x", "y")

#---------------------------------
#load period binned collections

PermianColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Permian/Permian.csv")
TriassicColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Triassic/Triassic.csv")
JurassicColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Jurassic/Jurassic.csv")
CretaceousColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Cretaceous/Cretaceous.csv")
PaleogeneColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Paleogene/Paleogene.csv")
NeogeneColl <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Figures/Maps/Neogene/Neogene.csv")
#---------------------------------
#calculate centroid for each period

PermianCent <- cbind.data.frame(mean(PermianColl$x, na.rm = TRUE), mean(PermianColl$y, na.rm = TRUE))
colnames(PermianCent) <- c("x", "y")
TriassicCent <- cbind.data.frame(mean(TriassicColl$x, na.rm = TRUE), mean(TriassicColl$y, na.rm = TRUE))
colnames(TriassicCent) <- c("x", "y")
JurassicCent <- cbind.data.frame(mean(JurassicColl$x, na.rm = TRUE), mean(JurassicColl$y, na.rm = TRUE))
colnames(JurassicCent) <- c("x", "y")
CretaceousCent <- cbind.data.frame(mean(CretaceousColl$x, na.rm = TRUE), mean(CretaceousColl$y, na.rm = TRUE))
colnames(CretaceousCent) <- c("x", "y")
PaleogeneCent <- cbind.data.frame(mean(PaleogeneColl$x, na.rm = TRUE), mean(PaleogeneColl$y, na.rm = TRUE))
colnames(PaleogeneCent) <- c("x", "y")
NeogeneCent <- cbind.data.frame(mean(NeogeneColl$x, na.rm = TRUE), mean(NeogeneColl$y, na.rm = TRUE))
colnames(NeogeneCent) <- c("x", "y")

#---------------------------------
#generate plots for each period

PermianPlot <- ggplot() +  
  geom_tile(data=PermianRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = PermianColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = PermianCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  labs(title = "Permian") +
  coord_map(projection = "mollweide") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

TriassicPlot <- ggplot() +  
  geom_tile(data=TriassicRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = TriassicColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = TriassicCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  coord_map(projection = "mollweide") +
  labs(title = "Triassic") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

JurassicPlot <- ggplot() +  
  geom_tile(data=JurassicRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = JurassicColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = JurassicCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  coord_map(projection = "mollweide") +
  labs(title = "Jurassic") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

CretaceousPlot <- ggplot() +  
  geom_tile(data=CretaceousRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = CretaceousColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = CretaceousCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  coord_map(projection = "mollweide") +
  labs(title = "Cretaceous") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

PaleogenePlot <- ggplot() +  
  geom_tile(data=PaleogeneRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = PaleogeneColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = PaleogeneCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  #scale_fill_manual(values=c('#999999','#E69F00', '#56B4E9')) +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  coord_map(projection = "mollweide") +
  labs(title = "Paleogene") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

NeogenePlot <- ggplot() +  
  geom_tile(data=NeogeneRas, aes(x=x, y=y, fill=as.factor(value)), alpha=0.8) + 
  geom_point(data = NeogeneColl, aes(x=paleolng, y=paleolat), shape = 21, size = 1, colour = "black", fill = "black") +
  geom_point(data = NeogeneCent, aes(x=x, y=y), shape = 21, size = 3, colour = "black", fill = "#de2d26") +
  scale_fill_manual(values=c('#66b266','#bbe1f6', '#56b4e9')) +
  coord_map(projection = "mollweide") +
  labs(title = "Neogene") +
  theme(plot.margin = unit(c(0.25,0.25,0.25,0.25), "cm"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2, size = 16, face = "bold"),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

#create panel plot

first_row <- plot_grid(centroidplot, labels = c('A'),  nrow = 1, label_size = 18)
second_row <- plot_grid(PermianPlot, TriassicPlot,labels = c('B', 'C'), nrow = 1, label_size = 18)
third_row <- plot_grid(JurassicPlot, CretaceousPlot, labels = c('D', 'E'), nrow = 1, label_size = 18)
fourth_row <- plot_grid(PaleogenePlot, NeogenePlot, labels = c('F', 'G'), nrow = 1, label_size = 18)
gg_all <- plot_grid(first_row, second_row, third_row, fourth_row, labels=c('', ''), rel_heights = c(1.5, 1, 1, 1), ncol=1, nrow = 4, align = "v")

#ggsave("./figures/fig_2.png", plot = gg_all, width = 150, height = 150, units = "mm", dpi = 300, scale = 2)
ggsave("./figures/fig_2.pdf", plot = gg_all, width = 150, height = 150, units = "mm", dpi = 300, scale = 2)

