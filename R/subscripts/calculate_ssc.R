#calculate SSC
source("./R/functions/get_ssc.R")
source("./R/options.R")

remove <- c("pre_industrial.csv")
files <- list.files(path = "./data/raw_data/binned_collections/", pattern = ".csv")
files <- files[!files %in% remove]

lat <- data.frame()
global <- data.frame()

for(i in files){
  name <- tools::file_path_sans_ext(i)
  data <- read.csv(paste("./data/raw_data/binned_collections/masked/", i, sep = ""))
  mask <- raster::raster(paste("./data/shallow_marine_grids/", name, ".asc", sep = ""))
  tmp1 <- get_ssc(data = data, bandsize = lat_bin, mask = mask, res = res)
  tmp2 <- get_ssc(data = data, bandsize = 180, mask = mask, res = res)
  tmp1 <- cbind.data.frame(tmp1, name)
  tmp2 <- cbind.data.frame(tmp2, name)
  lat <- rbind.data.frame(lat, tmp1)
  global <- rbind.data.frame(global, tmp2)
}

time_bins <- read.csv("./data/raw_data/stages.csv")
time_bins$name <- tolower(time_bins$name)

lat <- plyr::join(x = lat, y = time_bins, by = "name")
global <- plyr::join(x = global, y = time_bins, by = "name")

lat <- lat[order(lat$max_age),]
global <- global[order(global$max_age),]

dir.create("./results/SSC/")
write.csv(lat, "./results/SSC/lat_SSC.csv", row.names = FALSE)
write.csv(global, "./results/SSC/global_SSC.csv", row.names = FALSE)
