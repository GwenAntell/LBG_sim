#bin collections
collections <- read.csv("./data/raw_data/collections.csv")
collections$mid_ma <- (collections$max_ma + collections$min_ma)/2
time_bins <- read.csv("./data/raw_data/stages.csv")
dir.create("./data/raw_data/binned_collections/")
dir.create("./data/raw_data/binned_collections/masked/")

for(i in 1:nrow(time_bins)){
  name <- tolower(as.character(time_bins$name[i]))
  tmp <- subset(collections, mid_ma > time_bins$min_age[i] & mid_ma < time_bins$max_age[i])
  write.csv(tmp, paste("./data/raw_data/binned_collections/", name, ".csv", sep = ""))
  
  mask <- raster::raster(paste("./data/shallow_marine_grids/", name, ".asc", sep = ""))
  tmp$ext <- raster::extract(x = mask, y = tmp[,c("paleolng", "paleolat")])
  tmp <- subset(tmp, ext == 1)
  write.csv(tmp, paste("./data/raw_data/binned_collections/masked/", name, ".csv", sep = ""))
}
