rarefy <- function(data = data, bandsize = 15, sample = 50, iterations = 100){
  #make lat bins
  nbins <- 180/bandsize
  bin <- as.data.frame(seq(1, nbins, 1))
  max <- as.data.frame(rev(seq((-90+bandsize), 90, bandsize))) 
  min <- as.data.frame(rev(seq(-90, (90-bandsize), bandsize)))
  mid <- as.data.frame(rev(seq((-90+(bandsize/2)), (90-(bandsize/2)), bandsize)))
  hemisphere <- c("N", "S")
  hemisphere <- as.data.frame(rep(hemisphere, each = nbins/2))
  latbin <- cbind(bin, max, min, mid, hemisphere)
  colnames(latbin) <- c("bin", "max", "min", "mid", "hemisphere")
  
  #rarefy
  df <- data.frame()
  reps <- unique(data$rep)
  for(j in reps){
    subdat <- subset(data, rep == j)
    occurrences_per_latbin <- vector("numeric") #count occurrences within each latitudinal bin
  
    raredf <- lapply(1:iterations, function (x){
      
      for (i in 1:nrow(latbin)) {
        out <- subset(subdat, y <= latbin[i, "max"] & y >= latbin[i, "min"])
        out <- out[sample(nrow(out), size = sample, replace = TRUE),]
        occurrences_per_latbin[i] <- length(unique(out$id))
      }
      occurrences_per_latbin
    })
    
    raredf <- do.call(cbind, raredf)
    occurrences_per_latbin <- matrixStats::rowMedians(raredf)
  
      
  df[1:nrow(latbin),j] <- occurrences_per_latbin
  }
  
  df <- as.matrix.data.frame(df)
  latbin$median_richness <- matrixStats::rowMedians(df)
  quant <- matrixStats::rowQuantiles(df, probs = c(0.025, 0.975))
  latbin$CI.Lower <- quant[,1]
  latbin$CI.Upper <- quant[,2]
  
  rows <- which(latbin$median_richness == 0)
  
  plot(median_richness~mid, latbin,  main = "Latitudinal biodiversity gradient", ylab = "Species richness", xlab = expression(Latitude~(degree)), pch = 20, ylim = c(min(latbin$CI.Lower, na.rm = TRUE), max(latbin$CI.Upper, na.rm = TRUE)))
  polygon(c(latbin$mid,rev(latbin$mid)),c(latbin$CI.Lower,rev(latbin$CI.Upper)),col = "grey75", border = FALSE)
  points(median_richness~mid, latbin, pch = 20)
  lines(median_richness~mid, latbin, pch = 20)
  lines(latbin$mid, latbin$CI.Lower, col="red",lty=2)
  lines(latbin$mid, latbin$CI.Upper, col="red",lty=2)
  
  if(length(rows) >= 1){
    latbin$median_richness[rows] <- NA
    latbin$CI.Lower[rows] <- NA
    latbin$CI.Upper[rows] <- NA
  }
  
  return(latbin)
}
