get_layers <- function(layers, agg = "native", fun = mean, from = 0, to = 200, binary = TRUE){

    nme <- list.files(layers)
    layers <- raster::stack(paste(layers, "/", nme, sep = ""))
    nme <- basename(nme)
    nme <- tools::file_path_sans_ext(nme)
    names(layers) <- nme
    
    if(agg != "native"){
      layers <- raster::aggregate(layers, fact = agg, fun = fun)
    }
    
    stk <- sapply(1:nlayers(layers), function(x){
      r <- layers[[x]]
      r[r < from | r > to] <- NA
      if(binary == TRUE){
        r[!is.na(r)] <- 1
      }
      layers[[x]] <- r
    })
    layers <- stack(stk)
}
