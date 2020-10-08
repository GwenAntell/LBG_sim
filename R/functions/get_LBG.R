get_LBG <- function(data, id = "id", bandsize = 15, reps = FALSE){
  nbins <- 180/bandsize
  bin <- as.data.frame(seq(1, nbins, 1))
  max <- as.data.frame(rev(seq((-90+bandsize), 90, bandsize))) 
  min <- as.data.frame(rev(seq(-90, (90-bandsize), bandsize)))
  mid <- as.data.frame(rev(seq((-90+(bandsize/2)), (90-(bandsize/2)), bandsize)))
  hemisphere <- c("N", "S")
  hemisphere <- as.data.frame(rep(hemisphere, each = nbins/2))
  latbin <- cbind(bin, max, min, mid, hemisphere)
  colnames(latbin) <- c("bin", "max", "min", "mid", "hemisphere")
  
  if(reps == TRUE){
    nreps <- unique(data$rep)
    tmpdat <- lapply(nreps, function(i){ #check is right set up
      subdat <- subset(data, rep == i)
      species_per_latbin <- vector("numeric") #count genus richness within each latitudinal bin
      for (i in 1:nrow(latbin)) {
        out <- subset(subdat, y <= latbin[i, "max"] & y >= latbin[i, "min"])
        species_per_latbin[i] <- length(unique(out[,id]))
      }
      species_per_latbin
    })
    tmpdat <- do.call(cbind, tmpdat)
    latbin$median_richness <- matrixStats::rowMedians(tmpdat)
    quant <- matrixStats::rowQuantiles(tmpdat, probs = c(0.025, 0.975))
    latbin$CI.Lower <- quant[,1]
    latbin$CI.Upper <- quant[,2]
    plot(median_richness~mid, latbin,  main = "Latitudinal biodiversity gradient", ylab = "Species richness", xlab = expression(Latitude~(degree)), pch = 20, ylim = c(min(latbin$CI.Lower), max(latbin$CI.Upper)))
    polygon(c(latbin$mid,rev(latbin$mid)),c(latbin$CI.Lower,rev(latbin$CI.Upper)),col = "grey75", border = FALSE)
    points(median_richness~mid, latbin, pch = 20)
    lines(median_richness~mid, latbin, pch = 20)
    lines(latbin$mid, latbin$CI.Lower, col="red",lty=2)
    lines(latbin$mid, latbin$CI.Upper, col="red",lty=2)
  }
  
  
  if(reps == FALSE){
    species_per_latbin <- vector("numeric") #count genus richness within each latitudinal bin
    for (i in 1:nrow(latbin)) {
      out <- subset(data, y <= latbin[i, "max"] & y >= latbin[i, "min"])
      species_per_latbin[i] <- length(unique(out[,id]))
    }
    latbin$richness <- species_per_latbin
    plot(richness~mid, latbin,  main = "Latitudinal biodiversity gradient", ylab = "Species richness", xlab = expression(Latitude~(degree)), type = "o", pch = 20)
  }
  
  
  return(latbin)
}

