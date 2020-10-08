#range size frequency distributions

source("./R/functions/range_freq.R")
source("./R/options.R")

LatBreaks <- 10
LongBreaks <- 10
GCDBreaks <- 2000
ras <- raster(res = res, val = 1)

#ANTHOZOA#

data <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Data/obis_raw_data/Anthozoa.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Anthozoa <- data


png("./results/frequency_dist/empirical_range_freq.png", width = 200, height = 300, 
    units = "mm", res = 300)

par(mfrow=c(5,3))
Anthozoa.LAT <- range_freq(data = Anthozoa, metric = "latitude", breaks = LatBreaks)
title("A", adj = 0, cex.main = 1.25)
Anthozoa.LNG <- range_freq(data = Anthozoa, metric = "longitude", breaks = LongBreaks)
title("B", adj = 0, cex.main = 1.25)
Anthozoa.GCD <- range_freq(data = Anthozoa, metric = "GCD", breaks = GCDBreaks)
title("C", adj = 0, cex.main = 1.25)


#PORIFERA#

data <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Data/obis_raw_data/Porifera.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Porifera <- data

Porifera.LAT <- range_freq(data = Porifera, metric = "latitude", breaks = LatBreaks)
title("D", adj = 0, cex.main = 1.25)
Porifera.LNG <- range_freq(data = Porifera, metric = "longitude", breaks = LongBreaks)
title("E", adj = 0, cex.main = 1.25)
Porifera.GCD <- range_freq(data = Porifera, metric = "GCD", breaks = GCDBreaks)
title("F", adj = 0, cex.main = 1.25)

#BIVALVIA#

data <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Data/obis_raw_data/Bivalvia.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Bivalvia <- unique(data)

Bivalvia.LAT <- range_freq(data = Bivalvia, metric = "latitude", breaks = LatBreaks)
title("G", adj = 0, cex.main = 1.25)
Bivalvia.LNG <- range_freq(data = Bivalvia, metric = "longitude", breaks = LongBreaks)
title("H", adj = 0, cex.main = 1.25)
Bivalvia.GCD <- range_freq(data = Bivalvia, metric = "GCD", breaks = GCDBreaks)
title("I", adj = 0, cex.main = 1.25)

#BRACHIOPODA#

data <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Data/obis_raw_data/Brachiopoda.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Brachiopoda <- unique(data)

Brachiopoda.LAT <- range_freq(data = Brachiopoda, metric = "latitude", breaks = LatBreaks)
title("J", adj = 0, cex.main = 1.25)
Brachiopoda.LNG <- range_freq(data = Brachiopoda, metric = "longitude", breaks = LongBreaks)
title("K", adj = 0, cex.main = 1.25)
Brachiopoda.GCD <- range_freq(data = Brachiopoda, metric = "GCD", breaks = GCDBreaks)
title("L", adj = 0, cex.main = 1.25)

#ECHINOIDEA#

data <- read.csv("F:\\OneDrive - Imperial College London/Spatial sampling paper/Data/obis_raw_data/Echinoidea.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Echinoidea <- unique(data)

Echinoidea.LAT <- range_freq(data = Echinoidea, metric = "latitude", breaks = LatBreaks)
title("M", adj = 0, cex.main = 1.25)
Echinoidea.LNG <- range_freq(data = Echinoidea, metric = "longitude", breaks = LongBreaks)
title("N", adj = 0, cex.main = 1.25)
Echinoidea.GCD <- range_freq(data = Echinoidea, metric = "GCD", breaks = GCDBreaks)
title("O", adj = 0, cex.main = 1.25)

dev.off()

#SIMULATION#
data <- read.csv("./results/unimodal/pre_industrial.csv")
n <- sample(x = unique(data$rep), size = 1)
sample_sim <- subset(data, rep == n)
#range

png("./results/frequency_dist/sim_range_freq.png", width = 250, height = 100, 
    units = "mm", res = 300)

par(mfrow=c(1,3))

sim_lat <- range_freq(data = sample_sim, metric = "latitude", breaks = LatBreaks)
title("A", adj = 0, cex.main = 1.25)
sim_lng <- range_freq(data = sample_sim, metric = "longitude", breaks = LongBreaks)
title("B", adj = 0, cex.main = 1.25)
sim_GCD <- range_freq(data = sample_sim, metric = "GCD", breaks = GCDBreaks)
title("C", adj = 0, cex.main = 1.25)

dev.off()