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
    h <- hist(output$range, main = "", breaks = brk, col = "#c51b8a", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Latitude~(degree))))
    output <- cbind.data.frame(h$breaks[2:length(h$breaks)], h$counts)
    colnames(output) <- c("breaks", "counts")
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
    h <- hist(output$range, main = "", breaks = brk, col = "#3182bd", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Longitude~(degree))))
    output <- cbind.data.frame(h$breaks[2:length(h$breaks)], h$counts)
    colnames(output) <- c("breaks", "counts")
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
    
    h <- hist(output$range, main = "", breaks = brk, col = "#2ca25f", freq = TRUE, ylab = expression(bold(Frequency)), xlab = expression(bold(Range~(km))))
    output <- cbind.data.frame(h$breaks[2:length(h$breaks)], h$counts)
    colnames(output) <- c("breaks", "counts")
  }
  
  return(output)
}
