#corr test
source("./R/functions/get_proportional.R")

dir.create("./results/corr_test/")
stages <- read.csv("./data/raw_data/stages.csv")
stages$name <- tolower(stages$name)

remove <- c("pre_industrial.csv")

simulated <- list.files(path = "./results/unimodal/LBGs/", pattern = ".csv")
simulated <- simulated[!simulated %in% remove]

sampled <- list.files(path = "./results/unimodal/LBGs/Sampled/", pattern = ".csv")
sampled <- unique(stringr::str_remove(string = sampled, pattern = "_sampled"))

rarefied <- list.files(path = "./results/unimodal/LBGs/Rarefied/", pattern = ".csv")


#type unimodal and bimodal
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
  
  sim_unimodal$prop_richness[is.na(sim_unimodal$prop_richness)] <- 0
  sim_bimodal$prop_richness[is.na(sim_bimodal$prop_richness)] <- 0
  samp_unimodal$prop_richness[is.na(samp_unimodal$prop_richness)] <- 0
  samp_bimodal$prop_richness[is.na(samp_bimodal$prop_richness)] <- 0
  rare_unimodal$prop_richness[is.na(rare_unimodal$prop_richness)] <- NA
  rare_bimodal$prop_richness[is.na(rare_bimodal$prop_richness)] <- NA
  
  sim_corr_stat <- cor.test(sim_unimodal$prop_richness, sim_bimodal$prop_richness, method = "pearson")$estimate
  sim_corr_sig <- cor.test(sim_unimodal$prop_richness, sim_bimodal$prop_richness, method = "pearson")$p.value
  
  samp_corr_stat <- cor.test(samp_unimodal$prop_richness, samp_bimodal$prop_richness, method = "pearson")$estimate
  samp_corr_sig <- cor.test(samp_unimodal$prop_richness, samp_bimodal$prop_richness, method = "pearson")$p.value
  
  rare_corr_stat <- cor.test(rare_unimodal$prop_richness, rare_bimodal$prop_richness, method = "pearson")$estimate
  rare_corr_sig <- cor.test(rare_unimodal$prop_richness, rare_bimodal$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sim_corr_stat, sim_corr_sig, samp_corr_stat, samp_corr_sig, rare_corr_stat, rare_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/LBG_type_UB_temporal_corr_test.csv", row.names = FALSE)

#type unimodal and flat
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim_unimodal <- read.csv(paste("./results/unimodal/LBGs/", simulated[i], sep = ""))
  sim_flat <- read.csv(paste("./results/flat/LBGs/", simulated[i], sep = ""))
  
  samp_unimodal <- read.csv(paste("./results/unimodal/LBGs/Sampled/", sampled[i], sep = ""))
  samp_flat <- read.csv(paste("./results/flat/LBGs/Sampled/", sampled[i], sep = ""))
  
  rare_unimodal <- read.csv(paste("./results/unimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  rare_flat <- read.csv(paste("./results/flat/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim_unimodal$prop_richness <- get_proportional(sim_unimodal$mean_richness)
  sim_flat$prop_richness <- get_proportional(sim_flat$mean_richness)
  
  samp_unimodal$prop_richness <- get_proportional(samp_unimodal$mean_richness)
  samp_flat$prop_richness <- get_proportional(samp_flat$mean_richness)
  
  rare_unimodal$prop_richness <- get_proportional(rare_unimodal$mean_richness)
  rare_flat$prop_richness <- get_proportional(rare_flat$mean_richness)
  
  sim_unimodal$prop_richness[is.na(sim_flat$prop_richness)] <- 0
  sim_flat$prop_richness[is.na(sim_flat$prop_richness)] <- 0
  samp_unimodal$prop_richness[is.na(samp_flat$prop_richness)] <- 0
  samp_flat$prop_richness[is.na(samp_flat$prop_richness)] <- 0
  rare_unimodal$prop_richness[is.na(rare_flat$prop_richness)] <- NA
  rare_flat$prop_richness[is.na(rare_flat$prop_richness)] <- NA
  
  sim_corr_stat <- cor.test(sim_unimodal$prop_richness, sim_flat$prop_richness, method = "pearson")$estimate
  sim_corr_sig <- cor.test(sim_unimodal$prop_richness, sim_flat$prop_richness, method = "pearson")$p.value
  
  samp_corr_stat <- cor.test(samp_unimodal$prop_richness, samp_flat$prop_richness, method = "pearson")$estimate
  samp_corr_sig <- cor.test(samp_unimodal$prop_richness, samp_flat$prop_richness, method = "pearson")$p.value
  
  rare_corr_stat <- cor.test(rare_unimodal$prop_richness, rare_flat$prop_richness, method = "pearson")$estimate
  rare_corr_sig <- cor.test(rare_unimodal$prop_richness, rare_flat$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sim_corr_stat, sim_corr_sig, samp_corr_stat, samp_corr_sig, rare_corr_stat, rare_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/LBG_type_UF_temporal_corr_test.csv", row.names = FALSE)

#type bimodal and flat
master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim_flat <- read.csv(paste("./results/flat/LBGs/", simulated[i], sep = ""))
  sim_bimodal <- read.csv(paste("./results/bimodal/LBGs/", simulated[i], sep = ""))
  
  samp_flat <- read.csv(paste("./results/flat/LBGs/Sampled/", sampled[i], sep = ""))
  samp_bimodal <- read.csv(paste("./results/bimodal/LBGs/Sampled/", sampled[i], sep = ""))
  
  rare_flat <- read.csv(paste("./results/flat/LBGs/Rarefied/", rarefied[i], sep = ""))
  rare_bimodal <- read.csv(paste("./results/bimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim_flat$prop_richness <- get_proportional(sim_flat$mean_richness)
  sim_bimodal$prop_richness <- get_proportional(sim_bimodal$mean_richness)
  
  samp_flat$prop_richness <- get_proportional(samp_flat$mean_richness)
  samp_bimodal$prop_richness <- get_proportional(samp_bimodal$mean_richness)
  
  rare_flat$prop_richness <- get_proportional(rare_flat$mean_richness)
  rare_bimodal$prop_richness <- get_proportional(rare_bimodal$mean_richness)
  
  sim_flat$prop_richness[is.na(sim_bimodal$prop_richness)] <- 0
  sim_bimodal$prop_richness[is.na(sim_bimodal$prop_richness)] <- 0
  samp_flat$prop_richness[is.na(samp_bimodal$prop_richness)] <- 0
  samp_bimodal$prop_richness[is.na(samp_bimodal$prop_richness)] <- 0
  rare_flat$prop_richness[is.na(rare_bimodal$prop_richness)] <- NA
  rare_bimodal$prop_richness[is.na(rare_bimodal$prop_richness)] <- NA
  
  sim_corr_stat <- cor.test(sim_flat$prop_richness, sim_bimodal$prop_richness, method = "pearson")$estimate
  sim_corr_sig <- cor.test(sim_flat$prop_richness, sim_bimodal$prop_richness, method = "pearson")$p.value
  
  samp_corr_stat <- cor.test(samp_flat$prop_richness, samp_bimodal$prop_richness, method = "pearson")$estimate
  samp_corr_sig <- cor.test(samp_flat$prop_richness, samp_bimodal$prop_richness, method = "pearson")$p.value
  
  rare_corr_stat <- cor.test(rare_flat$prop_richness, rare_bimodal$prop_richness, method = "pearson")$estimate
  rare_corr_sig <- cor.test(rare_flat$prop_richness, rare_bimodal$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sim_corr_stat, sim_corr_sig, samp_corr_stat, samp_corr_sig, rare_corr_stat, rare_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/LBG_type_BF_temporal_corr_test.csv", row.names = FALSE)

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
  rare$prop_richness[is.na(rare$prop_richness)] <- NA 
  
  sampled_corr_stat <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$estimate
  sampled_corr_sig <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$p.value
  
  rarefied_corr_stat <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$estimate
  rarefied_corr_sig <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sampled_corr_stat, sampled_corr_sig, rarefied_corr_stat, rarefied_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/unimodal_temporal_corr_test.csv", row.names = FALSE)

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
  rare$prop_richness[is.na(rare$prop_richness)] <- NA 
  
  sampled_corr_stat <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$estimate
  sampled_corr_sig <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$p.value
  
  rarefied_corr_stat <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$estimate
  rarefied_corr_sig <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sampled_corr_stat, sampled_corr_sig, rarefied_corr_stat, rarefied_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/bimodal_temporal_corr_test.csv", row.names = FALSE)

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
  rare$prop_richness[is.na(rare$prop_richness)] <- NA 
  
  sampled_corr_stat <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$estimate
  sampled_corr_sig <- cor.test(sim$prop_richness, samp$prop_richness, method = "pearson")$p.value
  
  rarefied_corr_stat <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$estimate
  rarefied_corr_sig <- cor.test(sim$prop_richness, rare$prop_richness, method = "pearson")$p.value
  
  tmp <- cbind.data.frame(name, sampled_corr_stat, sampled_corr_sig, rarefied_corr_stat, rarefied_corr_sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/corr_test/flat_temporal_corr_test.csv", row.names = FALSE)