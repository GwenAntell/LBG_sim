#calculate_slope
#calcualte Frechet distance
source("./R/functions/get_proportional.R")

dir.create("./results/Slope/")
stages <- read.csv("./data/raw_data/stages.csv")
stages$name <- tolower(stages$name)

remove <- c("pre_industrial.csv")

simulated <- list.files(path = "./results/unimodal/LBGs/", pattern = ".csv")
simulated <- simulated[!simulated %in% remove]

sampled <- list.files(path = "./results/unimodal/LBGs/Sampled/", pattern = ".csv")
sampled <- unique(stringr::str_remove(string = sampled, pattern = "_sampled"))

rarefied <- list.files(path = "./results/unimodal/LBGs/Rarefied/", pattern = ".csv")

master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim <- read.csv(paste("./results/unimodal/LBGs/", simulated[i], sep = ""))
  samp <- read.csv(paste("./results/unimodal/LBGs/Sampled/", sampled[i], sep = ""))
  rare <- read.csv(paste("./results/unimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim$prop_richness <- get_proportional(sim$mean_richness)
  sim$prop_richness[sim$prop_richness == 0] <- NA
  samp$prop_richness <- get_proportional(samp$mean_richness)
  samp$prop_richness[samp$prop_richness == 0] <- NA
  rare$prop_richness <- get_proportional(rare$mean_richness)
  rare$prop_richness[rare$prop_richness == 0] <- NA
  
  simN <- subset(sim, hemisphere == "N")
  simS <- subset(sim, hemisphere == "S")
  sampN <- subset(samp, hemisphere == "N")
  sampS <- subset(samp, hemisphere == "S")
  rareN <- subset(rare, hemisphere == "N")
  rareS <- subset(rare, hemisphere == "S")
  #simulated
  if(sum(!is.na(simN$prop_richness)) < 4) {SimulatedN <- NA; SimulatedN.sig <- NA}
  else{SimulatedN <- summary(lm(simN$prop_richness~simN$mid))$coefficients[2]
  SimulatedN.sig <- summary(lm(simN$prop_richness~simN$mid))$coefficients[4]}
  
  if(sum(!is.na(simS$prop_richness)) < 4) {SimulatedS <- NA; SimulatedS.sig <- NA}
  else{SimulatedS <- summary(lm(simS$prop_richness~simS$mid))$coefficients[2]
  SimulatedS.sig <- summary(lm(simS$prop_richness~simS$mid))$coefficients[4]}
  
  #sampled
  if(sum(!is.na(sampN$prop_richness)) < 4) {SampledN <- NA; SampledN.sig <- NA}
  else{SampledN <- summary(lm(sampN$prop_richness~sampN$mid))$coefficients[2]
  SampledN.sig <- summary(lm(sampN$prop_richness~sampN$mid))$coefficients[4]}
  
  if(sum(!is.na(sampS$prop_richness)) < 4) {SampledS <- NA; SampledS.sig <- NA}
  else{SampledS <- summary(lm(sampS$prop_richness~sampS$mid))$coefficients[2]
  SampledS.sig <- summary(lm(sampS$prop_richness~sampS$mid))$coefficients[4]}
  
  #rarefied
  if(sum(!is.na(rareN$prop_richness)) < 4) {RarefiedN <- NA; RarefiedN.sig <- NA}
  else{RarefiedN <- summary(lm(rareN$prop_richness~rareN$mid))$coefficients[2]
  RarefiedN.sig <- summary(lm(rareN$prop_richness~rareN$mid))$coefficients[4]}
  
  if(sum(!is.na(rareS$prop_richness)) < 4) {RarefiedS <- NA; RarefiedS.sig <- NA}
  else{RarefiedS <- summary(lm(rareS$prop_richness~rareS$mid))$coefficients[2]
  RarefiedS.sig <- summary(lm(rareS$prop_richness~rareS$mid))$coefficients[4]}
  

  
  tmp <- cbind.data.frame(name, SimulatedN, SimulatedN.sig, SimulatedS, SimulatedS.sig, SampledN, SampledN.sig, SampledS, SampledS.sig, RarefiedN, RarefiedN.sig, RarefiedS, RarefiedS.sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/slope/unimodal_slope_estimates.csv", row.names = FALSE)

master <- data.frame()

for(i in 1:length(simulated)){
  name <- tools::file_path_sans_ext(simulated[i])
  
  sim <- read.csv(paste("./results/bimodal/LBGs/", simulated[i], sep = ""))
  samp <- read.csv(paste("./results/bimodal/LBGs/Sampled/", sampled[i], sep = ""))
  rare <- read.csv(paste("./results/bimodal/LBGs/Rarefied/", rarefied[i], sep = ""))
  
  sim$prop_richness <- get_proportional(sim$mean_richness)
  sim$prop_richness[sim$prop_richness == 0] <- NA
  samp$prop_richness <- get_proportional(samp$mean_richness)
  samp$prop_richness[samp$prop_richness == 0] <- NA
  rare$prop_richness <- get_proportional(rare$mean_richness)
  rare$prop_richness[rare$prop_richness == 0] <- NA
  
  simN <- subset(sim, hemisphere == "N")
  simS <- subset(sim, hemisphere == "S")
  sampN <- subset(samp, hemisphere == "N")
  sampS <- subset(samp, hemisphere == "S")
  rareN <- subset(rare, hemisphere == "N")
  rareS <- subset(rare, hemisphere == "S")
  #simulated
  if(sum(!is.na(simN$prop_richness)) < 4) {SimulatedN <- NA; SimulatedN.sig <- NA}
  else{SimulatedN <- summary(lm(simN$prop_richness~simN$mid))$coefficients[2]
  SimulatedN.sig <- summary(lm(simN$prop_richness~simN$mid))$coefficients[4]}
  
  if(sum(!is.na(simS$prop_richness)) < 4) {SimulatedS <- NA; SimulatedS.sig <- NA}
  else{SimulatedS <- summary(lm(simS$prop_richness~simS$mid))$coefficients[2]
  SimulatedS.sig <- summary(lm(simS$prop_richness~simS$mid))$coefficients[4]}
  #sampled
  if(sum(!is.na(sampN$prop_richness)) < 4) {SampledN <- NA; SampledN.sig <- NA}
  else{SampledN <- summary(lm(sampN$prop_richness~sampN$mid))$coefficients[2]
  SampledN.sig <- summary(lm(sampN$prop_richness~sampN$mid))$coefficients[4]}
  
  if(sum(!is.na(sampS$prop_richness)) < 4) {SampledS <- NA; SampledS.sig <- NA}
  else{SampledS <- summary(lm(sampS$prop_richness~sampS$mid))$coefficients[2]
  SampledS.sig <- summary(lm(sampS$prop_richness~sampS$mid))$coefficients[4]}
  
  #rarefied
  if(sum(!is.na(rareN$prop_richness)) < 4) {RarefiedN <- NA; RarefiedN.sig <- NA}
  else{RarefiedN <- summary(lm(rareN$prop_richness~rareN$mid))$coefficients[2]
  RarefiedN.sig <- summary(lm(rareN$prop_richness~rareN$mid))$coefficients[4]}
  
  if(sum(!is.na(rareS$prop_richness)) < 4) {RarefiedS <- NA; RarefiedS.sig <- NA}
  else{RarefiedS <- summary(lm(rareS$prop_richness~rareS$mid))$coefficients[2]
  RarefiedS.sig <- summary(lm(rareS$prop_richness~rareS$mid))$coefficients[4]}
  
  tmp <- cbind.data.frame(name, SimulatedN, SimulatedN.sig, SimulatedS, SimulatedS.sig, SampledN, SampledN.sig, SampledS, SampledS.sig, RarefiedN, RarefiedN.sig, RarefiedS, RarefiedS.sig)
  master <- rbind.data.frame(master, tmp)
  
}

master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/slope/bimodal_slope_estimates.csv", row.names = FALSE)
