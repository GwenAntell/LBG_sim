####bimodal###
# dir.create("./results/bimodal/") # GSA - dir already exists

library(parallel)
# GSA addition: time task and beep when done because simulations are so slow
pt1 <- proc.time()

cl <- makeCluster(detectCores()-1)

clusterExport(cl = cl, 
              varlist = list("reps", "species", "bimodal", "libs", "sim_dist"), 
              envir = environment())

clusterEvalQ(cl = cl, expr = lapply(libs, require, character.only = TRUE)) 

# GSA - meant to loop through layers = gradient masked by shelf area of each stage
# without DEM masks this is only 1 layer, so this 'loop' = index of 1
# for(i in 1:raster::nlayers(bimodal)){
  
  r <- bimodal # bimodal[[i]]
  name <- 'unmasked' #names(r)
  clusterExport(cl=cl, varlist = list("r"), envir=environment())
  master <- parLapply(cl = cl, 1:reps, function(j){
    
    sim <- lapply(1:species, function(x){
      sim_dist(r = r)
    })
    
    names(sim) <- 1:species
    
    sim <- dplyr::bind_rows(sim, .id = "id")[,c("id", "x", "y")]
    sim <- data.frame(unique(sim), row.names = NULL)
    
    return(sim)
  })
  
  names(master) <- 1:reps
  
  master <- dplyr::bind_rows(master, .id = "rep")
  
  write.csv(x = master, file = paste("./results/GSA/bimodal/", name, ".csv", sep = ""), row.names = FALSE)
#  write.csv(x = master, file = paste("./results/bimodal/", name, ".csv", sep = ""), row.names = FALSE)
#}

stopCluster(cl)
pt2 <- proc.time()
runtime <- (pt2 - pt1)/60
elaps <- round(runtime['elapsed'],4)
print(paste(elaps, 'min elapsed'))
beep()
