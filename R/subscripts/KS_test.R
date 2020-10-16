#KS test
source("./R/functions/get_proportional.R")

dir.create("./results/KS_test/")
stages <- read.csv("./data/raw_data/stages.csv")
stages$name <- tolower(stages$name)

remove <- c("pre_industrial.csv")

simulated <- list.files(path = "./results/unimodal/LBGs/", pattern = ".csv")
simulated <- simulated[!simulated %in% remove]

sampled <- list.files(path = "./results/unimodal/LBGs/Sampled/", pattern = ".csv")
sampled <- unique(stringr::str_remove(string = sampled, pattern = "_sampled"))

rarefied <- list.files(path = "./results/unimodal/LBGs/Rarefied/", pattern = ".csv")


#type
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim_unimodal <- read.csv(paste("./results/unimodal/LBGs/", simulated[i], sep = ""))
  sim_bimodal <- read.csv(paste("./results/bimodal/LBGs/", simulated[i], sep = ""))
  
  samp_unimodal <- read.csv(paste("./results/unimodal/LBGs/Sampled/", sampled[i], sep = ""))
  samp_bimodal <- read.csv(paste("./results/bimodal/LBGs/Sampled/", sampled[i], sep = ""))
  
  rare_unimodal <- read.csv(paste("./results/unimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  rare_bimodal <- read.csv(paste("./results/bimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim_unimodal$prop_richness <- get_proportional(sim_unimodal$mean_richness)
  sim_bimodal$prop_richness <- get_proportional(sim_bimodal$mean_richness)
  
  samp_unimodal$prop_richness <- get_proportional(samp_unimodal$mean_richness)
  samp_bimodal$prop_richness <- get_proportional(samp_bimodal$mean_richness)
  
  rare_unimodal$prop_richness <- get_proportional(rare_unimodal$mean_richness)
  rare_bimodal$prop_richness <- get_proportional(rare_bimodal$mean_richness)
  
  sim_unimodal$prop_richness[is.na(sim_bimodal$prop_richness)] <- 0
  sim_bimodal$prop_richness[is.na(sim_bimodal$prop_richness)] <- 0
  samp_unimodal$prop_richness[is.na(samp_bimodal$prop_richness)] <- 0
  samp_bimodal$prop_richness[is.na(samp_bimodal$prop_richness)] <- 0
  rare_unimodal$prop_richness[is.na(rare_bimodal$prop_richness)] <- 0
  rare_bimodal$prop_richness[is.na(rare_bimodal$prop_richness)] <- 0
  
  sim_ks_stat <- ks.test(sim_unimodal$prop_richness, sim_bimodal$prop_richness)$statistic
  sim_ks_sig <- ks.test(sim_unimodal$prop_richness, sim_bimodal$prop_richness)$p.value
  
  samp_ks_stat <- ks.test(samp_unimodal$prop_richness, samp_bimodal$prop_richness)$statistic
  samp_ks_sig <- ks.test(samp_unimodal$prop_richness, samp_bimodal$prop_richness)$p.value
  
  rare_ks_stat <- ks.test(rare_unimodal$prop_richness, rare_bimodal$prop_richness)$statistic
  rare_ks_sig <- ks.test(rare_unimodal$prop_richness, rare_bimodal$prop_richness)$p.value
  
  tmp <- cbind.data.frame(name, sim_ks_stat, sim_ks_sig, samp_ks_stat, samp_ks_sig, rare_ks_stat, rare_ks_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/KS_test/LBG_type_temporal_KS_test.csv", row.names = FALSE)

#unimodal
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim <- read.csv(paste("./results/unimodal/LBGs/", simulated[i], sep = ""))
  samp <- read.csv(paste("./results/unimodal/LBGs/Sampled/", sampled[i], sep = ""))
  rare <- read.csv(paste("./results/unimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim$prop_richness <- get_proportional(sim$mean_richness)
  samp$prop_richness <- get_proportional(samp$mean_richness)
  rare$prop_richness <- get_proportional(rare$mean_richness)
  
  sim$prop_richness[is.na(sim$prop_richness)] <- 0 
  samp$prop_richness[is.na(samp$prop_richness)] <- 0 
  rare$prop_richness[is.na(rare$prop_richness)] <- 0 
  
  sampled_ks_stat <- ks.test(sim$prop_richness, samp$prop_richness)$statistic
  sampled_ks_sig <- ks.test(sim$prop_richness, samp$prop_richness)$p.value
  
  rarefied_ks_stat <- ks.test(sim$prop_richness, rare$prop_richness)$statistic
  rarefied_ks_sig <- ks.test(sim$prop_richness, rare$prop_richness)$p.value
  
  tmp <- cbind.data.frame(name, sampled_ks_stat, sampled_ks_sig, rarefied_ks_stat, rarefied_ks_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/KS_test/unimodal_temporal_KS_test.csv", row.names = FALSE)

#bimodal
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim <- read.csv(paste("./results/bimodal/LBGs/", simulated[i], sep = ""))
  samp <- read.csv(paste("./results/bimodal/LBGs/Sampled/", sampled[i], sep = ""))
  rare <- read.csv(paste("./results/bimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim$prop_richness <- get_proportional(sim$mean_richness)
  samp$prop_richness <- get_proportional(samp$mean_richness)
  rare$prop_richness <- get_proportional(rare$mean_richness)
  
  sim$prop_richness[is.na(sim$prop_richness)] <- 0 
  samp$prop_richness[is.na(samp$prop_richness)] <- 0 
  rare$prop_richness[is.na(rare$prop_richness)] <- 0 
  
  sampled_ks_stat <- ks.test(sim$prop_richness, samp$prop_richness)$statistic
  sampled_ks_sig <- ks.test(sim$prop_richness, samp$prop_richness)$p.value
  
  rarefied_ks_stat <- ks.test(sim$prop_richness, rare$prop_richness)$statistic
  rarefied_ks_sig <- ks.test(sim$prop_richness, rare$prop_richness)$p.value
  
  tmp <- cbind.data.frame(name, sampled_ks_stat, sampled_ks_sig, rarefied_ks_stat, rarefied_ks_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/KS_test/bimodal_temporal_KS_test.csv", row.names = FALSE)

#flat
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim <- read.csv(paste("./results/flat/LBGs/", simulated[i], sep = ""))
  samp <- read.csv(paste("./results/flat/LBGs/Sampled/", sampled[i], sep = ""))
  rare <- read.csv(paste("./results/flat/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim$prop_richness <- get_proportional(sim$mean_richness)
  samp$prop_richness <- get_proportional(samp$mean_richness)
  rare$prop_richness <- get_proportional(rare$mean_richness)
  
  sim$prop_richness[is.na(sim$prop_richness)] <- 0 
  samp$prop_richness[is.na(samp$prop_richness)] <- 0 
  rare$prop_richness[is.na(rare$prop_richness)] <- 0 
  
  sampled_ks_stat <- ks.test(sim$prop_richness, samp$prop_richness)$statistic
  sampled_ks_sig <- ks.test(sim$prop_richness, samp$prop_richness)$p.value
  
  rarefied_ks_stat <- ks.test(sim$prop_richness, rare$prop_richness)$statistic
  rarefied_ks_sig <- ks.test(sim$prop_richness, rare$prop_richness)$p.value
  
  tmp <- cbind.data.frame(name, sampled_ks_stat, sampled_ks_sig, rarefied_ks_stat, rarefied_ks_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/KS_test/flat_temporal_KS_test.csv", row.names = FALSE)