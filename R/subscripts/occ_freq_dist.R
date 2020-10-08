#occurrence size frequency distributions

source("./R/functions/occ_freq.R")
source("./R/options.R")

dir.create("./results/frequency_dist/")
n_breaks <- 10
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

#SIMULATION#
data <- read.csv("./results/unimodal/pre_industrial.csv")
n <- sample(x = unique(data$rep), size = 1)
sample_sim <- subset(data, rep == n)

png("./results/frequency_dist/empirical_occ_freq.png", width = 210, height = 150, 
    units = "mm", res = 300)

par(mfrow=c(2,3))

Anthozoa_occ <- occ_freq(data = Anthozoa, breaks = n_breaks)
title("A", adj = 0, cex.main = 1.25)
Porifera_occ <- occ_freq(data = Porifera, breaks = n_breaks)
title("B", adj = 0, cex.main = 1.25)
Bivalvia_occ <- occ_freq(data = Bivalvia, breaks = n_breaks)
title("C", adj = 0, cex.main = 1.25)
Brachiopoda_occ <- occ_freq(data = Brachiopoda, breaks = n_breaks)
title("D", adj = 0, cex.main = 1.25)
Echinoidea_occ <- occ_freq(data = Echinoidea, breaks = n_breaks)
title("E", adj = 0, cex.main = 1.25)
Sim_occ <- occ_freq(data = sample_sim, breaks = n_breaks)
title("F", adj = 0, cex.main = 1.25)

dev.off()

master <- data.frame()

master[1,1:5] <- unlist(ks.test(Sim_occ$counts, Anthozoa_occ$counts[1:nrow(Sim_occ)]))
master[2,1:5] <- unlist(ks.test(Sim_occ$counts, Porifera_occ$counts[1:nrow(Sim_occ)]))
master[3,1:5] <- unlist(ks.test(Sim_occ$counts, Bivalvia_occ$counts[1:nrow(Sim_occ)]))
master[4,1:5] <- unlist(ks.test(Sim_occ$counts, Brachiopoda_occ$counts[1:nrow(Sim_occ)]))
master[5,1:5] <- unlist(ks.test(Sim_occ$counts, Echinoidea_occ$counts[1:nrow(Sim_occ)]))

master[6,1:5] <- unlist(ks.test(Anthozoa_occ$counts[1:nrow(Sim_occ)], Porifera_occ$counts[1:nrow(Sim_occ)]))
master[7,1:5] <- unlist(ks.test(Anthozoa_occ$counts[1:nrow(Sim_occ)], Bivalvia_occ$counts[1:nrow(Sim_occ)]))
master[8,1:5] <- unlist(ks.test(Anthozoa_occ$counts[1:nrow(Sim_occ)], Brachiopoda_occ$counts[1:nrow(Sim_occ)]))
master[9,1:5] <- unlist(ks.test(Anthozoa_occ$counts[1:nrow(Sim_occ)], Echinoidea_occ$counts[1:nrow(Sim_occ)]))

master[10,1:5] <- unlist(ks.test(Bivalvia_occ$counts[1:nrow(Sim_occ)], Porifera_occ$counts[1:nrow(Sim_occ)]))
master[11,1:5] <- unlist(ks.test(Bivalvia_occ$counts[1:nrow(Sim_occ)], Brachiopoda_occ$counts[1:nrow(Sim_occ)]))
master[12,1:5] <- unlist(ks.test(Bivalvia_occ$counts[1:nrow(Sim_occ)], Echinoidea_occ$counts[1:nrow(Sim_occ)]))

master[13,1:5] <- unlist(ks.test(Brachiopoda_occ$counts[1:nrow(Sim_occ)], Porifera_occ$counts[1:nrow(Sim_occ)]))
master[14,1:5] <- unlist(ks.test(Brachiopoda_occ$counts[1:nrow(Sim_occ)], Echinoidea_occ$counts[1:nrow(Sim_occ)]))

master[15,1:5] <- unlist(ks.test(Porifera_occ$counts[1:nrow(Sim_occ)], Echinoidea_occ$counts[1:nrow(Sim_occ)]))

colnames(master) <- names(unlist(ks.test(Sim_occ$counts, Anthozoa_occ$counts[1:nrow(Sim_occ)])))

write.csv(master, "./results/frequency_dist/occ_KS_test.csv", row.names = FALSE)
