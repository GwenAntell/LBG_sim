#compute LBGs
library(matrixStats) # GSA - necessary for something in here, likely get_LBG function

source("./R/functions/get_LBG.R")
source("./R/options.R")

# UNIMODAL
files <- list.files(path = "./results/GSA/unimodal/", pattern = ".csv")
        # list.files(path = "./results/unimodal/", pattern = ".csv")
# dir.create(path = "./results/unimodal/LBGs/") # GSA - dir exists
# GSA - Each csv loose in /unimodal is named simply a stage (e.g. Aalenian.csv)
# and contains x-y coords for a given rep (1-100) and taxon id (1-1,000)

# This loop takes the stage-level csv and calculates richness per lat bin, 
# as a csv and plot exported to /LBGs subfolder with the same name (stage alone).
# The unimodal.R script is modified to run without stage-level area masking, 
# so a single csv is output and this loop runs with an index of i = 1
for(i in files){
  data <- read.csv(paste("./results/GSA/unimodal/", i, sep = ""))
         # read.csv(paste("./results/unimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/GSA/unimodal/LBGs/", name, ".png", sep = ""), 
#  png(paste("./results/unimodal/LBGs/", name, ".png", sep = ""), 
      width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/GSA/unimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
# write.csv(LBG, paste("./results/unimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}

# BIMODAL
files <- list.files(path = "./results/GSA/bimodal/", pattern = ".csv")
        # list.files(path = "./results/bimodal/", pattern = ".csv")
# dir.create(path = "./results/bimodal/LBGs/") # GSA - dir exists

for(i in files){
  data <- read.csv(paste("./results/GSA/bimodal/", i, sep = ""))
          # read.csv(paste("./results/bimodal/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/GSA/bimodal/LBGs/", name, ".png", sep = ""), 
    # paste("./results/bimodal/LBGs/", name, ".png", sep = ""), 
      width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/GSA/bimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  # write.csv(LBG, paste("./results/bimodal/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}

files <- list.files(path = "./results/GSA/flat/", pattern = ".csv")
        # list.files(path = "./results/flat/", pattern = ".csv")
# dir.create(path = "./results/flat/LBGs/") # GSA - dir exists

# FLAT
for(i in files){
  data <- read.csv(paste("./results/GSA/flat/", i, sep = ""))
        # read.csv(paste("./results/flat/", i, sep = ""))
  name <- tools::file_path_sans_ext(i)
  png(paste("./results/GSA/flat/LBGs/", name, ".png", sep = ""), 
    # paste("./results/flat/LBGs/", name, ".png", sep = ""), 
      width = 210, height = 150, units = "mm", res = 300)
  LBG <- get_LBG(data = data, id = "id", bandsize = lat_bin, reps = TRUE)
  write.csv(LBG, paste("./results/GSA/flat/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  # write.csv(LBG, paste("./results/flat/LBGs/", name, ".csv", sep = ""), row.names = FALSE)
  dev.off()
}
