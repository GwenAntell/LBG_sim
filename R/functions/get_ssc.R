get_ssc <- function(data, bandsize = 180, mask = NULL, res = 1){
  
  nbins <- 180/bandsize
  bin <- as.data.frame(seq(1, nbins, 1))
  max <- as.data.frame(rev(seq((-90+bandsize), 90, bandsize))) 
  min <- as.data.frame(rev(seq(-90, (90-bandsize), bandsize)))
  mid <- as.data.frame(rev(seq((-90+(bandsize/2)), (90-(bandsize/2)), bandsize)))
  hemisphere <- c("N", "S")
  hemisphere <- as.data.frame(rep(hemisphere, each = nbins/2))
  
  if(nrow(hemisphere) == 0){
    hemisphere <- "Global"
  }
  
  latbin <- cbind(bin, max, min, mid, hemisphere)
  colnames(latbin) <- c("bin", "max", "min", "mid", "hemisphere")
  
  x <- data$getech_lng
  y <- data$getech_lat
  z <- data$collection_no
  
  xyz <- cbind.data.frame(x, y, z)
  xyz <- na.omit(xyz)
  
  xyz <- sp::SpatialPointsDataFrame(coords = cbind.data.frame(xyz$x, xyz$y), data = xyz)
  
  r <- raster(res = res)
  ras <- rasterize(xyz, r, 'z', function(x, ...) length(unique(na.omit(x))))
  ras[!is.na(ras)] <- 1
  
  mask <- raster::resample(mask, ras)
  ras <- raster::mask(ras, mask)
  
  msk <- raster::rowSums(mask, na.rm = TRUE)
  rs <- raster::rowSums(ras, na.rm = TRUE)
  
  ssc <- vector()
  
  for(i in 1:(180/bandsize)){
    min <- (bandsize * i) - (bandsize -1)
    max <- bandsize * i
    sumRAS <- sum(rs[min:max])
    
    sumMSK <- sum(msk[min:max])
    sumRAS <- (sumRAS/sumMSK)*100
    
    ssc[i] <- sumRAS
  }
  
  latbin <- cbind.data.frame(latbin, ssc)
  plot(ssc~mid, latbin, type = "o",  main = "Latitudinal spatial sampling coverage", ylab = "Spatial sampling coverage (%)", xlab = expression(Latitude~(degree)), pch = 20, xlim = c(-90, 90), ylim = c(0, max(latbin$ssc, na.rm=TRUE)))
  return(latbin)
}
