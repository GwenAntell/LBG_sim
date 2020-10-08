#Sample occurrences and compute LBGs

source("./R/functions/get_LBG.R")
source("./R/functions/sample_occ.R")
source("./R/options.R")

remove <- c("pre_industrial.csv")

files <- list.files(path = "./results/unimodal/", pattern = ".csv")
files <- files[!files %in% remove]

dir.create(path = "./results/unimodal/LBGs/Sampled/")


for(i in files){
  data <- read.csv(paste("./results/unimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  collections <- read.csv(paste("./data/raw_data/binned_collections/masked/", name, ".csv", sep = ""))
  data <- sample_occ(data = data, window = collections, res = res) 
  png(paste("./results/unimodal/LBGs/Sampled/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/unimodal/LBGs/Sampled/", name, ".csv", sep = ""), row.names = FALSE)
  write.csv(data, paste("./results/unimodal/LBGs/Sampled/", name, "_sampled.csv", sep = ""), row.names = FALSE)
  dev.off()
}

files <- list.files(path = "./results/bimodal/", pattern = ".csv")
files <- files[!files %in% remove]

dir.create(path = "./results/bimodal/LBGs/Sampled/")

for(i in files){
  data <- read.csv(paste("./results/bimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  collections <- read.csv(paste("./data/raw_data/binned_collections/masked/", name, ".csv", sep = ""))
  data <- sample_occ(data = data, window = collections, res = res) 
  png(paste("./results/bimodal/LBGs/Sampled/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/bimodal/LBGs/Sampled/", name, ".csv", sep = ""), row.names = FALSE)
  write.csv(data, paste("./results/bimodal/LBGs/Sampled/", name, "_sampled.csv", sep = ""), row.names = FALSE)
  dev.off()
}

files <- list.files(path = "./results/flat/", pattern = ".csv")
files <- files[!files %in% remove]

dir.create(path = "./results/flat/LBGs/Sampled/")

for(i in files){
  data <- read.csv(paste("./results/flat/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  collections <- read.csv(paste("./data/raw_data/binned_collections/masked/", name, ".csv", sep = ""))
  data <- sample_occ(data = data, window = collections, res = res) 
  png(paste("./results/flat/LBGs/Sampled/", name, ".png", sep = ""), width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/flat/LBGs/Sampled/", name, ".csv", sep = ""), row.names = FALSE)
  write.csv(data, paste("./results/flat/LBGs/Sampled/", name, "_sampled.csv", sep = ""), row.names = FALSE)
  dev.off()
}