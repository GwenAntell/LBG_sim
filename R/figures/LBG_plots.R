#LBG plots
# Lewis A. Jones
#---------------------------------
library(ggplot2)
library(ggpubr)
dir.create("./results/compiled_LBGs/plots/")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
col <- c("#1b9e77", "#d95f02", "#7570b3")
ssc <- read.csv("./results/SSC/lat_SSC.csv")
#---------------------------------

FlatSim <- read.csv("./results/compiled_LBGs/flat_simulated.csv")
FlatSamp <- read.csv("./results/compiled_LBGs/flat_sampled.csv")
FlatCR <- read.csv("./results/compiled_LBGs/flat_rarefied.csv")
FlatCR$prop_richness[which(FlatCR$prop_richness == 0)] <- NA

#---------------------------------

UnimodalSim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
UnimodalSamp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
UnimodalCR <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
UnimodalCR$prop_richness[which(UnimodalCR$prop_richness == 0)] <- NA

#---------------------------------

BimodalSim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
BimodalSamp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
BimodalCR <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")
BimodalCR$prop_richness[which(BimodalCR$prop_richness == 0)] <- NA

#---------------------------------


intervals <- as.vector(stages$name)

for(i in intervals){
  
  age <- subset(stages, intervals == i)
  mid_age <- round(age$mid_age, digits = 2)
  tmp.ssc <- subset(ssc, name == i)
  
  #---------------------------------
  Flat.sim <- subset(FlatSim, name == i)
  Flat.samp <- subset(FlatSamp, name == i)
  Flat.CR <- subset(FlatCR, name == i)
  
  Unimodal.sim <- subset(UnimodalSim, name == i)
  Unimodal.samp <- subset(UnimodalSamp, name == i)
  Unimodal.CR <- subset(UnimodalCR, name == i)
  
  Bimodal.sim <- subset(BimodalSim, name == i)
  Bimodal.samp <- subset(BimodalSamp, name == i)
  Bimodal.CR <- subset(BimodalCR, name == i)
  #---------------------------------
  
  unimodal.simplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Unimodal.sim, aes(x = mid, y = prop_richness), colour = col[1], size = 1.2, alpha = 0.8) +
    geom_point(data = Unimodal.sim, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Unimodal-type", subtitle = "Simulated") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  unimodal.simplot <- unimodal.simplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(trans = ~.*20, name = "SSC (%)"))
  unimodal.simplot
  
  unimodal.sampplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Unimodal.samp, aes(x = mid, y = prop_richness), colour = col[2], size = 1.2, alpha = 0.8) +
    geom_point(data = Unimodal.samp, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Unimodal-type", subtitle = "Sampled") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  unimodal.sampplot <- unimodal.sampplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(trans = ~.*20, name = "SSC (%)"))
  unimodal.sampplot
  
  unimodal.CRplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Unimodal.CR, aes(x = mid, y = prop_richness), colour = col[3], size = 1.2, alpha = 0.8) +
    geom_point(data = Unimodal.CR, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[3], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Unimodal-type", subtitle = "Rarefied") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  unimodal.CRplot <- unimodal.CRplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  unimodal.CRplot
  
  flat.simplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Flat.sim, aes(x = mid, y = prop_richness), colour = col[1], size = 1.2, alpha = 0.8) +
    geom_point(data = Flat.sim, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Flat-type", subtitle = "Simulated") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  flat.simplot <- flat.simplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  flat.simplot
  
  flat.sampplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Flat.samp, aes(x = mid, y = prop_richness), colour = col[2], size = 1.2, alpha = 0.8) +
    geom_point(data = Flat.samp, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Flat-type", subtitle = "Sampled") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  flat.sampplot
  flat.sampplot <- flat.sampplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  flat.sampplot
  
  flat.CRplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Flat.CR, aes(x = mid, y = prop_richness), colour = col[3], size = 1.2, alpha = 0.8) +
    geom_point(data = Flat.CR, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[3], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Flat-type", subtitle = "Rarefied") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  flat.CRplot
  flat.CRplot <- flat.CRplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  flat.CRplot
  
  bimodal.simplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Bimodal.sim, aes(x = mid, y = prop_richness), colour = col[1], size = 1.2, alpha = 0.8) +
    geom_point(data = Bimodal.sim, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[1], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Bimodal-type", subtitle = "Simulated") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y.left = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  
  bimodal.simplot <- bimodal.simplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(trans = ~.*20, name = "SSC (%)"))
  bimodal.simplot
  
  bimodal.sampplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Bimodal.samp, aes(x = mid, y = prop_richness), colour = col[2], size = 1.2, alpha = 0.8) +
    geom_point(data = Bimodal.samp, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[2], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Bimodal-type", subtitle = "Sampled") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y.left = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1, colour = NA),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          
          aspect.ratio = 0.6)
  
  bimodal.sampplot
  bimodal.sampplot <- bimodal.sampplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  bimodal.sampplot
  
  bimodal.CRplot <- ggplot() +
    geom_ribbon(data = tmp.ssc, aes(ymin = 0, ymax = ssc/20, x = mid), fill = "darkgrey", alpha = 0.5) +
    geom_segment(data = NULL, mapping=aes(x = -23.26, xend = -23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_segment(data = NULL, mapping=aes(x = 23.26, xend = 23.26, y =0, yend = Inf), linetype = 2, size = 1, color = "darkgrey") +
    geom_line(data = Bimodal.CR, aes(x = mid, y = prop_richness), colour = col[3], size = 1.2, alpha = 0.8) +
    geom_point(data = Bimodal.CR, aes(x = mid, y = prop_richness), shape = 21, colour= "black", fill = col[3], size = 3, alpha = 1) +
    scale_x_continuous(expand=c(0,0), breaks = seq(-90, 90, 30), labels = seq(-90, 90, 30), limits = c(-90, 90)) +
    scale_y_continuous(expand=c(0,0), breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), limits = c(0, 1)) +
    labs(y = "Standardised richness", x = expression(bold(paste("Latitude (",degree,")"))), title = "Bimodal-type", subtitle = "Rarefied") +
    theme(panel.background = element_blank(),
          plot.margin = margin(0.15,0.5,0.15,0.5, "cm"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
          axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
          axis.title.y.left = element_text(size = 16, face = "bold", vjust = 5, colour = NA),
          axis.title.y.right = element_text(size = 16, face = "bold", vjust = 5),
          axis.title.x = element_text(size = 16, face = "bold", vjust = -1),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5, colour = NA),
          plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
          aspect.ratio = 0.6)
  
  bimodal.CRplot <- bimodal.CRplot + scale_y_continuous(expand=c(0,0), breaks = seq(0, 1.1, 0.25), labels = seq(0, 1.1, 0.25), limits = c(0, 1.1), sec.axis = sec_axis(~.*20, name = "SSC (%)"))
  bimodal.CRplot
  
  combine <- ggarrange(flat.simplot, unimodal.simplot, bimodal.simplot,
                       flat.sampplot, unimodal.sampplot, bimodal.sampplot, 
                       flat.CRplot, unimodal.CRplot, bimodal.CRplot, 
                       ncol=3, nrow=3, widths = c(1,1,1,1,1,1,1,1,1), labels = "auto", align = "v")
  plot_name <- stringr::str_to_title(i)
  combine <- annotate_figure(combine, fig.lab = plot_name, top = " ", fig.lab.pos = "top.left", fig.lab.size = 18)
  ggsave(file=paste("./results/compiled_LBGs/plots/",mid_age,"_", i, ".png", sep =""), plot = combine, width = 210, height = 150, units = "mm", dpi = 300, scale = 1.7)
}
