library(raster)
r <- raster(res = 1, val = 1)
plot(r)

#unimodal
x <- seq(from = -90, to = 90, by = 1)
y <- dnorm(x, mean = 0, sd = 50)
plot(y)

r[1:nrow(r),] <- rep(y, each = ncol(r))
r <- (r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min"))
plot(r)

#bimodal
r <- raster(res = 1, val = 1)
plot(r)

x <- seq(from = 0, to = 90, by = 1)
y <- dnorm(x, mean = 45, sd = 15)
y <- append(y, y)
  
plot(y)

r[1:nrow(r),] <- rep(y, each = ncol(r))
r <- (r-cellStats(r,"min"))/(cellStats(r,"max")-cellStats(r,"min"))
plot(r)

