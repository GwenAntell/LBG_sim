sample_occ <- function(data, window, res){
  occ <- sp::SpatialPointsDataFrame(coords = cbind.data.frame(data$x, data$y), data = data)
  
  xyz <- window[c("paleolng", "paleolat", "collection_no")]
  xyz <- na.omit(xyz)
  xyz <- sp::SpatialPointsDataFrame(coords = cbind.data.frame(xyz$paleolng, xyz$paleolat), data = xyz)
  
  r <- raster::raster(res = res)
  ras <- raster::rasterize(xyz, r, 'collection_no', function(x, ...) length(unique(na.omit(x))))
  dat <- cbind.data.frame(occ,raster::extract(ras, occ, cellnumbers = TRUE, df = TRUE, method = "simple", na.rm = TRUE))
  dat <- subset(dat, !is.na(layer))
  dat <- dat[c("id", "x", "y", "rep","layer")]
  names(dat)[names(dat) == "layer"] <- "collections"
  return(dat)
}
