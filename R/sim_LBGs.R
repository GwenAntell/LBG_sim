#simulate LBG

source("./R/options.R") #source options
#load functions
source("./R/functions/LBG_type.R")
source("./R/functions/get_layers.R")
source("./R/functions/sim_dist.R")

#load libraries
lapply(libs, require, character.only = TRUE) 
#load DEMs
layers <- c("./data/DEMs/") #get path
layers <- get_layers(layers = layers, agg = 2, fun = min, from = 0, to = 200, binary = TRUE) #convert rasters to 1 deg res with aggregation

plot(layers$pre_industrial)

flat <- LBG_type(type = "flat", sd = 0, res = res)
unimodal <- LBG_type(type = "unimodal", sd = 20, res = res)
bimodal <- LBG_type(type = "bimodal", sd = 10, res = res)

layers <- resample(layers, unimodal)

flat <- raster::mask(x = flat, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid
unimodal <- raster::mask(x = unimodal, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid
bimodal <- raster::mask(x = bimodal, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid

names(flat) <- names(layers) #update layer names
names(unimodal) <- names(layers) #update layer names
names(bimodal) <- names(layers) #update layer names

plot(unimodal$pre_industrial)

#simulate each LBG
rstudioapi::jobRunScript(path = "./R/subscripts/flat.R", name = "flat", workingDir = getwd(), importEnv = TRUE)
rstudioapi::jobRunScript(path = "./R/subscripts/unimodal.R", name = "unimodal", workingDir = getwd(), importEnv = TRUE)
rstudioapi::jobRunScript(path = "./R/subscripts/bimodal.R", name = "bimodal", workingDir = getwd(), importEnv = TRUE)
