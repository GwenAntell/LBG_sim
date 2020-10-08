#compute LBGs
source("./R/functions/get_LBG.R")
source("./R/options.R")

files <- list.files(path = "./results/unimodal/", pattern = ".csv")
dir.create(path = "./results/unimodal/LBGs/")

for(i in files){
  data <- read.csv(paste("./results/unimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/unimodal/LBGs/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/unimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}

files <- list.files(path = "./results/bimodal/", pattern = ".csv")
dir.create(path = "./results/bimodal/LBGs/")

for(i in files){
  data <- read.csv(paste("./results/bimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/bimodal/LBGs/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/bimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}

files <- list.files(path = "./results/flat/", pattern = ".csv")
dir.create(path = "./results/flat/LBGs/")

for(i in files){
  data <- read.csv(paste("./results/flat/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/flat/LBGs/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/flat/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}
