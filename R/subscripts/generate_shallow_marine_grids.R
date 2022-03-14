# GSA - this script uses digital elevation maps to make shallow-marine masks
# but DEMs aren't committed to the repo so it's not possible to source.
# Mask files are also missing (should be in /shallow_marine_grids data subfolder)

#generate shallow marine grids
source("./R/options.R") #source options
#load functions
source("./R/functions/get_layers.R")
dir.create("./data/shallow_marine_grids/")
#load libraries
lapply(libs, require, character.only = TRUE) 
#load DEMs
layers <- c("./data/DEMs/") #get path
layers <- get_layers(layers = layers, agg = 2, fun = min, from = 0, to = 200, binary = TRUE) #convert rasters to 1 deg res with aggregation

r <- raster(res = res)

layers <- resample(layers, r)

path <- "./data/shallow_marine_grids/"
path <- paste(path, names(layers), ".asc", sep = "")

writeRaster(layers, filename = path, bylayer = TRUE, overwrite = TRUE)
