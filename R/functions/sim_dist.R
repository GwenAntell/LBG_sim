# GSA - function takes a raster of probabilities and
# returns a table of xy coords for simulated gen occs

sim_dist <- function(r){
  
  xy <- xyFromCell(r, 1:ncell(r))
  prob <- getValues(r)
  xy <- cbind(xy, prob)
  
  # GSA
  # This section assigns the lat/longitudinal breadth and n occs of a range
  # Why does longitude allow only up to 180 deg instead of 360?
  # Also ranges don't wrap around the prime meridian, which will artificially
  # truncate ranges that fall across it (a very thorny issue to fix though!!)
  n <- 300 #maximum number of occurrences
  maxlng <- 360 # 180
  maxlat <- 180 
  
  n <- seq(from = 2, to = n, by = 1)
  maxlng <- seq(from = 2, to = maxlng, by = 1)
  maxlat <- seq(from = 2, to = maxlat, by = 1)
  
  n <- sample(n, size = 1, replace = TRUE, prob = exp(-0.05 * n)) 
  #random number of occurrences based on exponential decay curve

  maxlng <- sample(maxlng, size = 1, replace = TRUE, 
                   prob = exp(-0.05 * maxlng)) 
  #random range based on exponential decay curve

  maxlat <- sample(maxlat, size = 1, replace = TRUE, 
                   prob = exp(-0.05 * maxlat)) 
  #random range based on exponential decay curve

  # (GSA - pick seed cell from raster of weighted prob, masked by shelf area)
  xy <- data.frame(na.omit(xy))
  orig <- xy[sample(nrow(xy), size = 1, replace = FALSE, prob = xy[,3]),] 
  #origination point of species
  orig <- orig[1:2]
  orig_cell <- cellFromXY(object = r, xy = orig)
  
  minlng <- floor(orig[,1] - (maxlng/2))
  maxlng <- ceiling(orig[,1] + (maxlng/2))
  
  minlat <- floor(orig[,2] - (maxlat/2))
  maxlat <- ceiling(orig[,2] + (maxlat/2))

  ext <- raster::extent(c(minlng, maxlng, minlat, maxlat))
  
  tmp <- r #create distance gradient for sampling n cells within home range
  tmp[] <- NA
  tmp[orig_cell] <- 0.5 
  # GSA - why 0.5 specifically? other cells will have val = thousands of km
  tmp <- crop(x = tmp, y = ext)
  
  # (GSA - raster::distance calculates dist from each NA cell to nearest non-NA)
  suppressWarnings(tmp <- raster::distance(tmp)) #create distance gradient
  
  # Does taking the negative of the 0-1 scale and adding the previous max+min value
  # (to prevent negative probabilities) give the same as simply inverse distance?
  # Probably not, but the difference may be too small to matter? (GSA)
#  tmp <- (tmp-raster::cellStats(tmp,"min")) / (raster::cellStats(tmp,"max")-raster::cellStats(tmp,"min"))  #rescale 0 to 1
  
#  rmax <- raster::cellStats(tmp, stat = "max", na.rm = TRUE, 
#                            asSample = FALSE)
  # GSA - asSample arg unnecessary; only used if the calculated stat is SD
#  rmin <- raster::cellStats(tmp, stat = "min", na.rm = TRUE, 
#                            asSample = FALSE)
#  tmp <- ((tmp - rmax) * -1) + rmin
  
  # 'prob' argument in sample() need not sum to 1, so rescaling is unnecessary
  # An alternative:
  tmp <- 1/tmp
  # central cell will have prob = Inf, so reset that to prob of nearest neighbor
  cntr <- which.max(tmp)
  tmp[cntr] <- 0
  tmp[cntr] <- cellStats(tmp, max)
  
  # GSA - mask out land area based on original raster
  clip <- raster::crop(x = r, y = tmp)
  tmp <- raster::mask(x = tmp, mask = clip)
  
  xy <- raster::xyFromCell(tmp, 1:ncell(tmp)) #xy dataframe of cells
  prob <- raster::getValues(tmp) #probability values within home range
  xy <- cbind(xy, prob) #bind
  xy <- data.frame(na.omit(xy))
  
  # GSA - are OBIS occurrences presence-absence? If not, would converting them
  # to 0-1 match fossil reality better (i.e. no abundance)?
  # If binary, change to replace = F to generate a number of *unique* occs
  # equal to n-val drawn from empirical dist of OBIS samples per genus
  xy <- xy[sample(nrow(xy), size = n, replace = TRUE, prob = xy[,3]),] #sample n occurrences
  xy <- round(xy, digits = 2) #round off data
  xy <- data.frame(xy[,1:2])
  #head(xy)
  #plot(r)
  #points(xy)
  return(xy) #return data
}
