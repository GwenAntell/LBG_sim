#simulate LBG

source("./R/options.R") #source options
#load functions
source("./R/functions/LBG_type.R")
source("./R/functions/get_layers.R")
source("./R/functions/sim_dist.R")

#load libraries
lapply(libs, require, character.only = TRUE) 
#load DEMs
layers <- c("/Users/lewis_jones/Documents/GitHub/LBG_sim/data/DEMs") #get path
layers <- get_layers(layers = layers, agg = 2, fun = min, from = 0, to = 200, binary = TRUE) #convert rasters to 1 deg res with aggregation

plot(layers$pre_industrial)
layers <- layers$pre_industrial

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

###flat###
dir.create("./results/flat/")

for(i in 1:nlayers(flat)){
  r <- flat[[i]]
  name <- names(r)
  
  master <- data.frame()
  for(j in 1:reps){
    pb <- txtProgressBar(min = 0, max = reps, style = 3) 
    setTxtProgressBar(pb,j)  
    
    sim <- lapply(1:species, function(x){
      sim_dist(r = r)
    })
    names(sim) <- 1:species
    
    sim <- dplyr::bind_rows(sim, .id = "id")[,c("id", "x", "y")]
    sim <- data.frame(unique(sim))
    unique(sim$species)
    
    rep <- j
    sim <- cbind.data.frame(sim, rep)
    
    master <- rbind.data.frame(master, sim)
  }
  
  write.csv(x = master, file = paste("./results/flat/", name, ".csv", sep = ""), row.names = FALSE)
}

####unimodal###
dir.create("./results/unimodal/")
for(i in 1:nlayers(unimodal)){
  r <- unimodal[[i]]
  name <- names(r)
  
  master <- data.frame()
  for(j in 1:reps){
  pb <- txtProgressBar(min = 0, max = reps, style = 3) 
  setTxtProgressBar(pb,j)  
  
  sim <- lapply(1:species, function(x){
    sim_dist(r = r)
  })
  names(sim) <- 1:species

  sim <- dplyr::bind_rows(sim, .id = "id")[,c("id", "x", "y")]
  sim <- data.frame(unique(sim))
  unique(sim$species)
  
  rep <- j
  sim <- cbind.data.frame(sim, rep)
  
  master <- rbind.data.frame(master, sim)
  }
  
  write.csv(x = master, file = paste("./results/unimodal/", name, ".csv", sep = ""), row.names = FALSE)
}

####bimodal###
dir.create("./results/bimodal/")

for(i in 1:nlayers(bimodal)){
  r <- bimodal[[i]]
  name <- names(r)
  
  master <- data.frame()
  for(j in 1:reps){
    pb <- txtProgressBar(min = 0, max = reps, style = 3) 
    setTxtProgressBar(pb,j)  
    
    sim <- lapply(1:species, function(x){
      sim_dist(r = r)
    })
    names(sim) <- 1:species
    
    sim <- dplyr::bind_rows(sim, .id = "id")[,c("id", "x", "y")]
    sim <- data.frame(unique(sim))
    unique(sim$species)
    
    rep <- j
    sim <- cbind.data.frame(sim, rep)
    
    master <- rbind.data.frame(master, sim)
  }
  
  write.csv(x = master, file = paste("./results/bimodal/", name, ".csv", sep = ""), row.names = FALSE)
}



