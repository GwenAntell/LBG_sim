#calcualte LM
dir.create("./results/R2/")
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
  
  sampled_r2 <- summary(lm(sim$prop_richness~samp$prop_richness))$r.squared
  sampled_pval <- summary(lm(sim$prop_richness~samp$prop_richness))$coefficients[8]
  
  rarefied_r2 <- summary(lm(sim$prop_richness~rare$prop_richness))$r.squared
  rarefied_pval <- summary(lm(sim$prop_richness~rare$prop_richness))$coefficients[8]
  
  tmp <- cbind.data.frame(name, sampled_r2, sampled_pval, rarefied_r2, rarefied_pval)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/R2/unimodal_temporal_R2.csv", row.names = FALSE)
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
  
  sampled_r2 <- summary(lm(sim$prop_richness~samp$prop_richness))$r.squared
  sampled_pval <- summary(lm(sim$prop_richness~samp$prop_richness))$coefficients[8]
  
  rarefied_r2 <- summary(lm(sim$prop_richness~rare$prop_richness))$r.squared
  rarefied_pval <- summary(lm(sim$prop_richness~rare$prop_richness))$coefficients[8]
  
  tmp <- cbind.data.frame(name, sampled_r2, sampled_pval, rarefied_r2, rarefied_pval)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/R2/bimodal_temporal_R2.csv", row.names = FALSE)
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
  
  sampled_r2 <- summary(lm(sim$prop_richness~samp$prop_richness))$r.squared
  sampled_pval <- summary(lm(sim$prop_richness~samp$prop_richness))$coefficients[8]
  
  rarefied_r2 <- summary(lm(sim$prop_richness~rare$prop_richness))$r.squared
  rarefied_pval <- summary(lm(sim$prop_richness~rare$prop_richness))$coefficients[8]
  
  tmp <- cbind.data.frame(name, sampled_r2, sampled_pval, rarefied_r2, rarefied_pval)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/R2/flat_temporal_R2.csv", row.names = FALSE)
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
  
  simulated_r2 <- summary(lm(subset(unimodal_sim, name == i)$prop_richness~subset(bimodal_sim, name == i)$prop_richness))$r.squared
  simulated_pval <- summary(lm(subset(unimodal_sim, name == i)$prop_richness~subset(bimodal_sim, name == i)$prop_richness))$coefficients[8]
  
  sampled_r2 <- summary(lm(subset(unimodal_samp, name == i)$prop_richness~subset(bimodal_samp, name == i)$prop_richness))$r.squared
  sampled_pval <- summary(lm(subset(unimodal_samp, name == i)$prop_richness~subset(bimodal_samp, name == i)$prop_richness))$coefficients[8]
  
  rarefied_r2 <- summary(lm(subset(unimodal_rare, name == i)$prop_richness~subset(bimodal_rare, name == i)$prop_richness))$r.squared
  rarefied_pval <- summary(lm(subset(unimodal_rare, name == i)$prop_richness~subset(bimodal_rare, name == i)$prop_richness))$coefficients[8]
  
  tmp <- cbind.data.frame(name, simulated_r2, simulated_pval, sampled_r2, sampled_pval, rarefied_r2, rarefied_pval)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/R2/LBG_type_R2.csv", row.names = FALSE)
