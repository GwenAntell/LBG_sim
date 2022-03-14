LBG_type <- function(type, sd = 20, res = 1){
  
  if(type != "unimodal" & type != "bimodal" & type != "flat"){
    warning("type must be unimodal, bimodal or flat")
  }
  
  r <- raster::raster(res = res, val = 1)
  
  if(type == "unimodal"){
    x <- seq(from = -89, to = 90, by = 1)
    y <- dnorm(x, mean = 0, sd = sd)
    # GSA - repeat each val of y across a whole raster row (latitude)
    # another way to do it would be to draw a new prob for each cell
    # based on its distance from the equator (according to norm dist)
    r[1:nrow(r),] <- rep(y, each = ncol(r))
    
    # GSA - this step isn't strictly necessary 
    # since the prob arg in sample() doesn't require prob to sum to 1
    # (and rescaling from 0-1 means all values still won't sum to 1)
    # Also - the prob for the least-likely place will be forced to flat 0
    # unlikely to have any meaningful effect (prob no occs put there anyway), 
    # but presumably isn't desirable for study design
    r <- (r-raster::cellStats(r,"min")) / (raster::cellStats(r,"max")-raster::cellStats(r,"min"))
    plot(r, main = "Unimodal-type", legend = FALSE)
  }
  
  
  if(type == "bimodal"){
    x <- seq(from = 0, to = 89, by = 1)
    y <- dnorm(x, mean = 45, sd = sd)
    y <- append(y, y)
    r[1:nrow(r),] <- rep(y, each = ncol(r))
    r <- (r-raster::cellStats(r,"min")) / (raster::cellStats(r,"max")-raster::cellStats(r,"min"))
    plot(r, main = "Bimodal-type", legend = FALSE)
  }
  
  if(type == "flat"){
    plot(r, main = "Flat-type", col = "green3", legend = FALSE)
  }
  return(r)
}

