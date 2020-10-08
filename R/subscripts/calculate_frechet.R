#calcualte Frechet distance
dir.create("./results/Frechet/")
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
  
  sampled_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_frechet, rarefied_frechet)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/Frechet/unimodal_temporal_frechet.csv", row.names = FALSE)
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
  
  sampled_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_frechet, rarefied_frechet)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/Frechet/bimodal_temporal_frechet.csv", row.names = FALSE)
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
  
  sampled_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = samp$prop_richness)
  rarefied_frechet <- TSdist::FrechetDistance(x = sim$prop_richness, y = rare$prop_richness)
  
  tmp <- cbind.data.frame(name, sampled_frechet, rarefied_frechet)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/Frechet/flat_temporal_frechet.csv", row.names = FALSE)
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

  simulated_frechet <- TSdist::FrechetDistance(x = subset(unimodal_sim, name == i)$prop_richness, y = subset(bimodal_sim, name == i)$prop_richness)
  sampled_frechet <- TSdist::FrechetDistance(x = subset(unimodal_samp, name == i)$prop_richness, y = subset(bimodal_samp, name == i)$prop_richness)
  rarefied_frechet <- TSdist::FrechetDistance(x = subset(unimodal_rare, name == i)$prop_richness, y = subset(bimodal_rare, name == i)$prop_richness)
  
  tmp <- cbind.data.frame(name, simulated_frechet, sampled_frechet, rarefied_frechet)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/Frechet/LBG_type_frechet.csv", row.names = FALSE)




