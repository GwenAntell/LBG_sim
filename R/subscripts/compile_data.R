#compile data

source("./R/functions/get_proportional.R")
stages <- read.csv("./data/raw_data/stages.csv")
files <- list.files(path = "./results/unimodal/", pattern = ".csv")
files <- subset(files, files != "pre_industrial.csv")

dir.create("./results/compiled_LBGs/")

type <- c("unimodal", "bimodal", "flat")

#simulated
for(t in type){
#compile LBGs
master <- data.frame()

  for(i in files){
    name <- tools::file_path_sans_ext(i)
    tmp <- read.csv(paste("./results/", t, "/LBGs/", i, sep = ""))
    tmp$prop_richness <- get_proportional(tmp$mean_richness)
    tmp <- cbind.data.frame(name, tmp)
    master <- rbind.data.frame(master, tmp)
  }
  
  master <- plyr::join(x = master, y = stages, by = "name")
  write.csv(master, paste("./results/compiled_LBGs/", t, "_simulated.csv", sep = ""), row.names = FALSE)
}

#sampled
for(t in type){
  #compile LBGs
  master <- data.frame()
  
  for(i in files){
    name <- tools::file_path_sans_ext(i)
    tmp <- read.csv(paste("./results/", t, "/LBGs/Sampled/", i, sep = ""))
    tmp$prop_richness <- get_proportional(tmp$mean_richness)
    tmp <- cbind.data.frame(name, tmp)
    master <- rbind.data.frame(master, tmp)
  }
  
  master <- plyr::join(x = master, y = stages, by = "name")
  write.csv(master, paste("./results/compiled_LBGs/", t, "_sampled.csv", sep = ""), row.names = FALSE)
}

#rarefied
for(t in type){
  #compile LBGs
  master <- data.frame()
  
  for(i in files){
    name <- tools::file_path_sans_ext(i)
    tmp <- read.csv(paste("./results/", t, "/LBGs/Rarefied/", i, sep = ""))
    tmp$prop_richness <- get_proportional(tmp$mean_richness)
    tmp <- cbind.data.frame(name, tmp)
    master <- rbind.data.frame(master, tmp)
  }
  
  master <- plyr::join(x = master, y = stages, by = "name")
  write.csv(master, paste("./results/compiled_LBGs/", t, "_rarefied.csv", sep = ""), row.names = FALSE)
}