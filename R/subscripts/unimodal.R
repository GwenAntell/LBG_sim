####unimodal###
lapply(libs, require, character.only = TRUE) 

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
