#options
libs = c("raster", "plyr", "dplyr", "stringr", "ggplot2", "ggpubr", "grid", "png", "viridis", "gridExtra") #libraries
res = 1 #spatial resolution of the study
reps = # 100 #number of replications for simulations
  50 # 100 reps of 1k species takes a LONG time
species = 1000 #number of species to generate in simulations
lat_bin = 15 #latitudinal bin size for analyses
rarefy_sample = 50 #sample size for sample-sized rarefaction
rarefy_iterations = 1000 #number of iterations for rarefaction