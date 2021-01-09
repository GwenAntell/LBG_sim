#range size frequency distributions

source("./R/functions/range_freq.R")
source("./R/options.R")

dir.create("./results/frequency_dist/")
LatBreaks <- 10
LongBreaks <- 10
GCDBreaks <- 2000
ras <- raster(res = res, val = 1)

#ANTHOZOA#

data <- read.csv("./data/raw_data/OBIS/Anthozoa.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Anthozoa <- data


png("./figures/empirical_range_freq.png", width = 210, height = 290, 
    units = "mm", res = 300)

par(mfrow=c(5,3))
Anthozoa.LAT <- range_freq(data = Anthozoa, metric = "latitude", breaks = LatBreaks)
title("a", adj = 0, cex.main = 1.75)
Anthozoa.LNG <- range_freq(data = Anthozoa, metric = "longitude", breaks = LongBreaks)
title("b", adj = 0, cex.main = 1.75)
Anthozoa.GCD <- range_freq(data = Anthozoa, metric = "GCD", breaks = GCDBreaks)
title("c", adj = 0, cex.main = 1.75)


#PORIFERA#

data <- read.csv("./data/raw_data/OBIS/Porifera.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Porifera <- data

Porifera.LAT <- range_freq(data = Porifera, metric = "latitude", breaks = LatBreaks)
title("d", adj = 0, cex.main = 1.75)
Porifera.LNG <- range_freq(data = Porifera, metric = "longitude", breaks = LongBreaks)
title("e", adj = 0, cex.main = 1.75)
Porifera.GCD <- range_freq(data = Porifera, metric = "GCD", breaks = GCDBreaks)
title("f", adj = 0, cex.main = 1.75)

#BIVALVIA#

data <- read.csv("./data/raw_data/OBIS/Bivalvia.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Bivalvia <- unique(data)

Bivalvia.LAT <- range_freq(data = Bivalvia, metric = "latitude", breaks = LatBreaks)
title("g", adj = 0, cex.main = 1.75)
Bivalvia.LNG <- range_freq(data = Bivalvia, metric = "longitude", breaks = LongBreaks)
title("h", adj = 0, cex.main = 1.75)
Bivalvia.GCD <- range_freq(data = Bivalvia, metric = "GCD", breaks = GCDBreaks)
title("i", adj = 0, cex.main = 1.75)

#BRACHIOPODA#

data <- read.csv("./data/raw_data/OBIS/Brachiopoda.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Brachiopoda <- unique(data)

Brachiopoda.LAT <- range_freq(data = Brachiopoda, metric = "latitude", breaks = LatBreaks)
title("j", adj = 0, cex.main = 1.75)
Brachiopoda.LNG <- range_freq(data = Brachiopoda, metric = "longitude", breaks = LongBreaks)
title("k", adj = 0, cex.main = 1.75)
Brachiopoda.GCD <- range_freq(data = Brachiopoda, metric = "GCD", breaks = GCDBreaks)
title("l", adj = 0, cex.main = 1.75)

#ECHINOIDEA#

data <- read.csv("./data/raw_data/OBIS/Echinoidea.csv", sep =",")
data <- subset(data, !is.na(speciesid)) #remove data not identified to species level
data <- cbind.data.frame(as.numeric(data$decimalLongitude), as.numeric(data$decimalLatitude), data$speciesid)
colnames(data) <- c("x", "y", "id")
data <- cbind.data.frame(data, raster::extract(ras, data[,1:2], cellnumbers = TRUE))
data <- data[,c("id", "cells")]
data <- cbind.data.frame(data, (xyFromCell(object = ras, cell = data$cells)))
data <- unique(data)

Echinoidea <- unique(data)

Echinoidea.LAT <- range_freq(data = Echinoidea, metric = "latitude", breaks = LatBreaks)
title("m", adj = 0, cex.main = 1.75)
Echinoidea.LNG <- range_freq(data = Echinoidea, metric = "longitude", breaks = LongBreaks)
title("n", adj = 0, cex.main = 1.75)
Echinoidea.GCD <- range_freq(data = Echinoidea, metric = "GCD", breaks = GCDBreaks)
title("o", adj = 0, cex.main = 1.75)

dev.off()

#SIMULATION#
data <- read.csv("./results/unimodal/pre_industrial.csv")
n <- sample(x = unique(data$rep), size = 1)
sample_sim <- subset(data, rep == n)
#range

png("./figures/sim_range_freq.png", width = 250, height = 100, 
    units = "mm", res = 300)

par(mfrow=c(1,3))

sim_lat <- range_freq(data = sample_sim, metric = "latitude", breaks = LatBreaks)
title("a", adj = 0, cex.main = 1.75)
sim_lng <- range_freq(data = sample_sim, metric = "longitude", breaks = LongBreaks)
title("b", adj = 0, cex.main = 1.75)
sim_GCD <- range_freq(data = sample_sim, metric = "GCD", breaks = GCDBreaks)
title("c", adj = 0, cex.main = 1.75)

dev.off()

#Latitude

master <- data.frame()

master[1,1:5] <- unlist(ks.test(sim_lat$counts, Anthozoa.LAT$counts[1:nrow(sim_lat)]))
master[2,1:5] <- unlist(ks.test(sim_lat$counts, Porifera.LAT$counts[1:nrow(sim_lat)]))
master[3,1:5] <- unlist(ks.test(sim_lat$counts, Bivalvia.LAT$counts[1:nrow(sim_lat)]))
master[4,1:5] <- unlist(ks.test(sim_lat$counts, Brachiopoda.LAT$counts[1:nrow(sim_lat)]))
master[5,1:5] <- unlist(ks.test(sim_lat$counts, Echinoidea.LAT$counts[1:nrow(sim_lat)]))

master[6,1:5] <- unlist(ks.test(Anthozoa.LAT$counts[1:nrow(sim_lat)], Porifera.LAT$counts[1:nrow(sim_lat)]))
master[7,1:5] <- unlist(ks.test(Anthozoa.LAT$counts[1:nrow(sim_lat)], Bivalvia.LAT$counts[1:nrow(sim_lat)]))
master[8,1:5] <- unlist(ks.test(Anthozoa.LAT$counts[1:nrow(sim_lat)], Brachiopoda.LAT$counts[1:nrow(sim_lat)]))
master[9,1:5] <- unlist(ks.test(Anthozoa.LAT$counts[1:nrow(sim_lat)], Echinoidea.LAT$counts[1:nrow(sim_lat)]))

master[10,1:5] <- unlist(ks.test(Bivalvia.LAT$counts[1:nrow(sim_lat)], Porifera.LAT$counts[1:nrow(sim_lat)]))
master[11,1:5] <- unlist(ks.test(Bivalvia.LAT$counts[1:nrow(sim_lat)], Brachiopoda.LAT$counts[1:nrow(sim_lat)]))
master[12,1:5] <- unlist(ks.test(Bivalvia.LAT$counts[1:nrow(sim_lat)], Echinoidea.LAT$counts[1:nrow(sim_lat)]))

master[13,1:5] <- unlist(ks.test(Brachiopoda.LAT$counts[1:nrow(sim_lat)], Porifera.LAT$counts[1:nrow(sim_lat)]))
master[14,1:5] <- unlist(ks.test(Brachiopoda.LAT$counts[1:nrow(sim_lat)], Echinoidea.LAT$counts[1:nrow(sim_lat)]))

master[15,1:5] <- unlist(ks.test(Porifera.LAT$counts[1:nrow(sim_lat)], Echinoidea.LAT$counts[1:nrow(sim_lat)]))

colnames(master) <- names(unlist(ks.test(sim_lat$counts, Anthozoa.LAT$counts[1:nrow(sim_lat)])))

write.csv(master, "./results/frequency_dist/LAT_KS_test.csv", row.names = FALSE)

#Longitude

master <- data.frame()

master[1,1:5] <- unlist(ks.test(sim_lng$counts, Anthozoa.LNG$counts[1:nrow(sim_lng)]))
master[2,1:5] <- unlist(ks.test(sim_lng$counts, Porifera.LNG$counts[1:nrow(sim_lng)]))
master[3,1:5] <- unlist(ks.test(sim_lng$counts, Bivalvia.LNG$counts[1:nrow(sim_lng)]))
master[4,1:5] <- unlist(ks.test(sim_lng$counts, Brachiopoda.LNG$counts[1:nrow(sim_lng)]))
master[5,1:5] <- unlist(ks.test(sim_lng$counts, Echinoidea.LNG$counts[1:nrow(sim_lng)]))

master[6,1:5] <- unlist(ks.test(Anthozoa.LNG$counts[1:nrow(sim_lng)], Porifera.LNG$counts[1:nrow(sim_lng)]))
master[7,1:5] <- unlist(ks.test(Anthozoa.LNG$counts[1:nrow(sim_lng)], Bivalvia.LNG$counts[1:nrow(sim_lng)]))
master[8,1:5] <- unlist(ks.test(Anthozoa.LNG$counts[1:nrow(sim_lng)], Brachiopoda.LNG$counts[1:nrow(sim_lng)]))
master[9,1:5] <- unlist(ks.test(Anthozoa.LNG$counts[1:nrow(sim_lng)], Echinoidea.LNG$counts[1:nrow(sim_lng)]))

master[10,1:5] <- unlist(ks.test(Bivalvia.LNG$counts[1:nrow(sim_lng)], Porifera.LNG$counts[1:nrow(sim_lng)]))
master[11,1:5] <- unlist(ks.test(Bivalvia.LNG$counts[1:nrow(sim_lng)], Brachiopoda.LNG$counts[1:nrow(sim_lng)]))
master[12,1:5] <- unlist(ks.test(Bivalvia.LNG$counts[1:nrow(sim_lng)], Echinoidea.LNG$counts[1:nrow(sim_lng)]))

master[13,1:5] <- unlist(ks.test(Brachiopoda.LNG$counts[1:nrow(sim_lng)], Porifera.LNG$counts[1:nrow(sim_lng)]))
master[14,1:5] <- unlist(ks.test(Brachiopoda.LNG$counts[1:nrow(sim_lng)], Echinoidea.LNG$counts[1:nrow(sim_lng)]))

master[15,1:5] <- unlist(ks.test(Porifera.LNG$counts[1:nrow(sim_lng)], Echinoidea.LNG$counts[1:nrow(sim_lng)]))

colnames(master) <- names(unlist(ks.test(sim_lng$counts, Anthozoa.LNG$counts[1:nrow(sim_lng)])))

write.csv(master, "./results/frequency_dist/LNG_KS_test.csv", row.names = FALSE)

#GCD

master <- data.frame()

master[1,1:5] <- unlist(ks.test(sim_GCD$counts, Anthozoa.GCD$counts[1:nrow(sim_GCD)]))
master[2,1:5] <- unlist(ks.test(sim_GCD$counts, Porifera.GCD$counts[1:nrow(sim_GCD)]))
master[3,1:5] <- unlist(ks.test(sim_GCD$counts, Bivalvia.GCD$counts[1:nrow(sim_GCD)]))
master[4,1:5] <- unlist(ks.test(sim_GCD$counts, Brachiopoda.GCD$counts[1:nrow(sim_GCD)]))
master[5,1:5] <- unlist(ks.test(sim_GCD$counts, Echinoidea.GCD$counts[1:nrow(sim_GCD)]))

master[6,1:5] <- unlist(ks.test(Anthozoa.GCD$counts[1:nrow(sim_GCD)], Porifera.GCD$counts[1:nrow(sim_GCD)]))
master[7,1:5] <- unlist(ks.test(Anthozoa.GCD$counts[1:nrow(sim_GCD)], Bivalvia.GCD$counts[1:nrow(sim_GCD)]))
master[8,1:5] <- unlist(ks.test(Anthozoa.GCD$counts[1:nrow(sim_GCD)], Brachiopoda.GCD$counts[1:nrow(sim_GCD)]))
master[9,1:5] <- unlist(ks.test(Anthozoa.GCD$counts[1:nrow(sim_GCD)], Echinoidea.GCD$counts[1:nrow(sim_GCD)]))

master[10,1:5] <- unlist(ks.test(Bivalvia.GCD$counts[1:nrow(sim_GCD)], Porifera.GCD$counts[1:nrow(sim_GCD)]))
master[11,1:5] <- unlist(ks.test(Bivalvia.GCD$counts[1:nrow(sim_GCD)], Brachiopoda.GCD$counts[1:nrow(sim_GCD)]))
master[12,1:5] <- unlist(ks.test(Bivalvia.GCD$counts[1:nrow(sim_GCD)], Echinoidea.GCD$counts[1:nrow(sim_GCD)]))

master[13,1:5] <- unlist(ks.test(Brachiopoda.GCD$counts[1:nrow(sim_GCD)], Porifera.GCD$counts[1:nrow(sim_GCD)]))
master[14,1:5] <- unlist(ks.test(Brachiopoda.GCD$counts[1:nrow(sim_GCD)], Echinoidea.GCD$counts[1:nrow(sim_GCD)]))

master[15,1:5] <- unlist(ks.test(Porifera.GCD$counts[1:nrow(sim_GCD)], Echinoidea.GCD$counts[1:nrow(sim_GCD)]))

colnames(master) <- names(unlist(ks.test(sim_GCD$counts, Anthozoa.GCD$counts[1:nrow(sim_GCD)])))

write.csv(master, "./results/frequency_dist/GCD_KS_test.csv", row.names = FALSE)