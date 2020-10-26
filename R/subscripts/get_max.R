#get max

dir.create("./results/max_lat/")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
col <- c("#1b9e77", "#d95f02", "#7570b3")
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

#flat
master <- data.frame()
for(i in stages$name){
  tmp <- subset(FlatSim, name == i)
  n <- which.max(tmp$prop_richness)
  simulated <- tmp[n,]
  
  tmp <- subset(FlatSamp, name == i)
  n <- which.max(tmp$prop_richness)
  sampled <- tmp[n,]
  
  tmp <- subset(FlatCR, name == i)
  n <- which.max(tmp$prop_richness)
  rarefied <- tmp[n,]
  
  data <- rbind.data.frame(simulated, sampled, rarefied)
  
  data$type <- c("simulated", "sampled", "rarefied")
  
  master <- rbind.data.frame(master, data)
}

write.csv(master, "./results/max_lat/flat_type.csv", row.names = FALSE)
#---------------------------------

#unimodal
master <- data.frame()
for(i in stages$name){
  tmp <- subset(UnimodalSim, name == i)
  n <- which.max(tmp$prop_richness)
  simulated <- tmp[n,]
  
  tmp <- subset(UnimodalSamp, name == i)
  n <- which.max(tmp$prop_richness)
  sampled <- tmp[n,]
  
  tmp <- subset(UnimodalCR, name == i)
  n <- which.max(tmp$prop_richness)
  rarefied <- tmp[n,]
  
  data <- rbind.data.frame(simulated, sampled, rarefied)
  
  data$type <- c("simulated", "sampled", "rarefied")
  
  master <- rbind.data.frame(master, data)
}

write.csv(master, "./results/max_lat/unimodal_type.csv", row.names = FALSE)

#---------------------------------

#bimodal
master <- data.frame()
for(i in stages$name){
  tmp <- subset(BimodalSim, name == i)
  n <- which.max(tmp$prop_richness)
  simulated <- tmp[n,]
  
  tmp <- subset(BimodalSamp, name == i)
  n <- which.max(tmp$prop_richness)
  sampled <- tmp[n,]
  
  tmp <- subset(BimodalCR, name == i)
  n <- which.max(tmp$prop_richness)
  rarefied <- tmp[n,]
  
  data <- rbind.data.frame(simulated, sampled, rarefied)
  
  data$type <- c("simulated", "sampled", "rarefied")
  
  master <- rbind.data.frame(master, data)
}

write.csv(master, "./results/max_lat/bimodal_type.csv", row.names = FALSE)

  