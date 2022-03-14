# This script feeds to flat.R, unimodal.R, and bimodal.R, 
# which together do the simulations and make csv's 
# in first-level /results folders for /flat, /unimodal, and /bimodal.
# The 'LBG analyses' scripts called in run_analyses.R use these csv's

#simulate LBG

# source("./R/subscripts/directories.R") #generate directories
# GSA - this is a 2-line script to create /figures and /results; skip if existing

source("./R/options.R") #source options
#load functions
source("./R/functions/LBG_type.R")
source("./R/functions/get_layers.R")
source("./R/functions/sim_dist.R")

#load libraries
lapply(libs, require, character.only = TRUE) 
library(beepr) # beep when each of the final sims at end of script is finished
#load DEMs # GSA - there is no /DEMs subfolder available at GitHub repo
# layers <- c("./data/DEMs/") #get path

# layers <- get_layers(layers = layers, agg = 2, fun = min, from = 0, to = 200, binary = TRUE) #convert rasters to 1 deg res with aggregation
  # GSA - Error in .rasterObjectFromFile(x, objecttype = "RasterBrick", ...) : 
  # Cannot create a RasterLayer object from this file. (file does not exist)
  # (see previous comment about DEMs data not committed)

flat <- LBG_type(type = "flat", sd = 0, res = res) #generate probability grid
unimodal <- LBG_type(type = "unimodal", sd = 20, res = res) #generate probability grid
bimodal <- LBG_type(type = "bimodal", sd = 10, res = res) #generate probability grid

# layers <- resample(layers, unimodal) #resample
# GSA - raster::resample transfers values between rasters of diff origin/proj/res

# flat <- raster::mask(x = flat, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid
# unimodal <- raster::mask(x = unimodal, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid
# bimodal <- raster::mask(x = bimodal, mask = layers,  maskvalue = NA) #mask probability grid by shallow marine grid

# names(flat) <- names(layers) #update layer names
# names(unimodal) <- names(layers) #update layer names
# names(bimodal) <- names(layers) #update layer names


#simulate each LBG
# GSA - each of these 3 scripts calls sim_dist to sim genus occs
# but weighted probs of occ placement seem off
source("./R/subscripts/unimodal.R") 
source("./R/subscripts/bimodal.R")
source("./R/subscripts/flat.R")

#run as job
#rstudioapi::jobRunScript(path = "./R/subscripts/unimodal.R", name = "unimodal", workingDir = getwd(), importEnv = TRUE)
#rstudioapi::jobRunScript(path = "./R/subscripts/flat.R", name = "flat", workingDir = getwd(), importEnv = TRUE)
#rstudioapi::jobRunScript(path = "./R/subscripts/bimodal.R", name = "bimodal", workingDir = getwd(), importEnv = TRUE)
