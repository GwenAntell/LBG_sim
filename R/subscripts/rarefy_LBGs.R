#rarefy occurrences

source("./R/options.R")
source("./R/functions/rarefy.R")

files <- list.files(path = "./results/unimodal/LBGs/Sampled/", pattern = "_sampled.csv")

dir.create(path = "./results/unimodal/LBGs/Rarefied/")

library(parallel)

cl <- makeCluster(detectCores()-2)

clusterExport(cl=cl, varlist = list("rarefy_iterations", "rarefy_sample", "lat_bin", "libs", "rarefy"), envir=environment())

clusterEvalQ(cl = cl, expr = lapply(libs, require, character.only = TRUE)) 

parLapply(cl = cl, files, function(i){
  data <- read.csv(paste("./results/unimodal/LBGs/Sampled/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  png(paste("./results/unimodal/LBGs/Rarefied/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- rarefy(data = data, bandsize = lat_bin, sample = rarefy_sample, iterations = rarefy_iterations)
  write.csv(LBG, paste("./results/unimodal/LBGs/Rarefied/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
})

files <- list.files(path = "./results/bimodal/LBGs/Sampled/", pattern = "_sampled.csv")

dir.create(path = "./results/bimodal/LBGs/Rarefied/")

parLapply(cl = cl, files, function(i){
  data <- read.csv(paste("./results/bimodal/LBGs/Sampled/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  png(paste("./results/bimodal/LBGs/Rarefied/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- rarefy(data = data, bandsize = lat_bin, sample = rarefy_sample, iterations = rarefy_iterations)
  write.csv(LBG, paste("./results/bimodal/LBGs/Rarefied/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
})

files <- list.files(path = "./results/flat/LBGs/Sampled/", pattern = "_sampled.csv")

dir.create(path = "./results/flat/LBGs/Rarefied/")

parLapply(cl = cl, files, function(i){
  data <- read.csv(paste("./results/flat/LBGs/Sampled/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  png(paste("./results/flat/LBGs/Rarefied/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- rarefy(data = data, bandsize = lat_bin, sample = rarefy_sample, iterations = rarefy_iterations)
  write.csv(LBG, paste("./results/flat/LBGs/Rarefied/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
})

stopCluster(cl)

