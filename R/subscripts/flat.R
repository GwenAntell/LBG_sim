###flat###
dir.create("./results/flat/")

library(parallel)

cl <- makeCluster(detectCores()-2)

clusterExport(cl=cl, varlist = list("reps", "species", "flat", "libs", "sim_dist"), envir=environment())

clusterEvalQ(cl = cl, expr = lapply(libs, require, character.only = TRUE)) 

for(i in 1:raster::nlayers(flat)){
  r <- flat[[i]]
  name <- names(r)
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
  
  write.csv(x = master, file = paste("./results/flat/", name, ".csv", sep = ""), row.names = FALSE)
}

stopCluster(cl)



