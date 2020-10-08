#calcualte SLD
source("./R/functions/get_SLD.R")
dir.create("./results/SLD/")
stages <- read.csv("./data/raw_data/stages.csv")

#---------------------
#unimodal
#---------------------
simulated <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
sampled <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
rarefied <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")

master <- data.frame()

for(i in stages$name){
  name <- i
  
  sim <- subset(simulated, name == i)
  samp <- subset(sampled, name == i)
  rare <- subset(rarefied, name == i)
  
  sampled_SLD <- get_SLD(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_SLD <- get_SLD(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_SLD, rarefied_SLD)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/SLD/unimodal_temporal_SLD.csv", row.names = FALSE)
#---------------------
#bimodal
#---------------------
simulated <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
sampled <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
rarefied <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")

master <- data.frame()

for(i in stages$name){
  name <- i
  
  sim <- subset(simulated, name == i)
  samp <- subset(sampled, name == i)
  rare <- subset(rarefied, name == i)
  
  sampled_SLD <- get_SLD(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_SLD <- get_SLD(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_SLD, rarefied_SLD)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/SLD/bimodal_temporal_SLD.csv", row.names = FALSE)
#---------------------
#flat
#---------------------
simulated <- read.csv("./results/compiled_LBGs/flat_simulated.csv")
sampled <- read.csv("./results/compiled_LBGs/flat_sampled.csv")
rarefied <- read.csv("./results/compiled_LBGs/flat_rarefied.csv")

master <- data.frame()

for(i in stages$name){
  name <- i
  
  sim <- subset(simulated, name == i)
  samp <- subset(sampled, name == i)
  rare <- subset(rarefied, name == i)
  
  sampled_SLD <- get_SLD(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_SLD <- get_SLD(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_SLD, rarefied_SLD)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/SLD/flat_temporal_SLD.csv", row.names = FALSE)
#---------------------
#LBG type
#---------------------

unimodal_sim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
bimodal_sim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")

unimodal_samp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
bimodal_samp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")

unimodal_rare <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
bimodal_rare <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")


master <- data.frame()

for(i in stages$name){
  name <- i
  
  simulated_SLD <- get_SLD(x = subset(unimodal_sim, name == i)$prop_richness, y = subset(bimodal_sim, name == i)$prop_richness)
  sampled_SLD <- get_SLD(x = subset(unimodal_samp, name == i)$prop_richness, y = subset(bimodal_samp, name == i)$prop_richness)
  rarefied_SLD <- get_SLD(x = subset(unimodal_rare, name == i)$prop_richness, y = subset(bimodal_rare, name == i)$prop_richness)
  
  tmp <- cbind.data.frame(name, simulated_SLD, sampled_SLD, rarefied_SLD)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/SLD/LBG_type_SLD.csv", row.names = FALSE)