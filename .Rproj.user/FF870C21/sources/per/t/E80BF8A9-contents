range_freq <- function(data,  metric = "latitude", breaks){
  
  if(metric == "latitude"){
    sp <- unique(data[,"id"])
    dat <- lapply(sp, function(i){
      tmp <- subset(data, id == i)
      mx <- max(tmp$y, na.rm = TRUE)
      mn <- min(tmp$y, na.rm = TRUE)
      species <- i
      range <- abs((mn)-(mx))
      output <- cbind.data.frame(species, range)
      output
    })
    names(dat) <- unique(data$id)
    output <- suppressWarnings(dplyr::bind_rows(dat))
    max <- max(output$range, na.rm = TRUE)
    brk <- seq(from = 0, to = (max+breaks), by = breaks)
    hist(output$range, main = "Latitudinal range", breaks = brk, col = "grey80", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Latitude~(degree))))
  }
  
  else if(metric == "longitude"){
    sp <- unique(data[,"id"])
    dat <- lapply(sp, function(i){
      tmp <- subset(data, id == i)
      mx <- max(tmp$x, na.rm = TRUE)
      mn <- min(tmp$x, na.rm = TRUE)
      species <- i
      range <- (mn-mx) %% 360
      if(range >= 180){range <- abs(range -360)}
      else{range <- abs(range)}
      output <- cbind.data.frame(species, range)
      output
    })
    names(dat) <- unique(data$id)
    output <- suppressWarnings(dplyr::bind_rows(dat))
    max <- max(output$range, na.rm = TRUE)
    brk <- seq(from = 0, to = (max+breaks), by = breaks)
    hist(output$range, main = "Longitudinal range", breaks = brk, col = "grey80", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Longitude~(degree))))
  }
  
  else if(metric == "GCD"){
    sp <- unique(data[,"id"])
    dat <- lapply(sp, function(i){
      tmp <- subset(data, id == i)
      tmp <- cbind.data.frame(tmp$x, tmp$y)
      colnames(tmp) <- c("x", "y")
      species <- i
      tmp <- data.matrix(tmp)
      range <- max(sp::spDists(tmp, tmp, longlat = TRUE), na.rm = TRUE)
      output <- cbind.data.frame(species, range)
      output
    })
    names(dat) <- unique(data$id)
    output <- suppressWarnings(dplyr::bind_rows(dat))
    max <- max(output$range, na.rm = TRUE)
    brk <- seq(from = 0, to = (max+breaks), by = breaks)
    
    hist(output$range, main = "Great Circle Distance range", breaks = brk, col = "grey80", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Range~(km))))
  }
  
  return(output)
}
