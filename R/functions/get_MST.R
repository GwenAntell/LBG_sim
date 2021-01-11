get_MST <- function(data, bandsize = 15, res = 1){
  
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
  latbin$MST <- NA
  
  x <- data$getech_lng
  y <- data$getech_lat
  z <- data$collection_no
  
  xyz <- cbind.data.frame(x, y, z)
  xyz <- na.omit(xyz)
  
  xyz <- sp::SpatialPointsDataFrame(coords = cbind.data.frame(xyz$x, xyz$y), data = xyz)
  r <- raster(res = res)
  ras <- rasterize(xyz, r, 'z', function(x, ...) length(unique(na.omit(x))))
  ras[!is.na(ras)] <- 1
  
  cells <- raster::cellFromXY(ras, xyz)
  
  xy <- unique(data.frame(raster::xyFromCell(ras, cells)))
  
  for (i in 1:nrow(latbin)) {
    out <- subset(xy, y <= latbin[i, "max"] & y >= latbin[i, "min"])
    
    if(nrow(out) <= 1){latbin$MST[i] <- 0; next}
    
    out <- as.matrix.data.frame(out)
    gcdists <- geosphere::distm(out, fun = geosphere::distGeo)
    mst_sp <- vegan::spantree(gcdists)
    mst_length_sp <- sum(mst_sp$dist)/1000
    latbin$MST[i] <- mst_length_sp
  }
  
  latbin$MST <- round(latbin$MST, digits = 2)
  
  return(latbin)
}
