#unimodal
data <- read.csv("./results/max_lat/unimodal_type.csv")

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

unimodal_samp <- (sum(sim$mid == samp$mid)/56)*100
unimodal_rare <- (sum(sim$mid == rare$mid)/56)*100

#bimodal
data <- read.csv("./results/max_lat/bimodal_type.csv")

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

bimodal_samp <- (sum(sim$mid == samp$mid)/56)*100
bimodal_rare <- (sum(sim$mid == rare$mid)/56)*100

#flat
data <- read.csv("./results/max_lat/flat_type.csv")

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

flat_samp <- (sum(sim$mid == samp$mid)/56)*100
flat_rare <- (sum(sim$mid == rare$mid)/56)*100

LBG_type <- c("Flat", "Unimodal", "Bimodal")
Sampled <- round(c(flat_samp, unimodal_samp, bimodal_samp), 2)
Sampling_standardised <- round(c(flat_rare, unimodal_rare, bimodal_rare), 2)

data <- cbind(LBG_type, Sampled, Sampling_standardised)

write.csv(data, "./results/max_lat/formatted_table_bin.csv", row.names = FALSE)

#correct zone

tropical <- c(0, 30)
temperate <- c(30, 60)
polar <- c(60, 90)

#unimodal
data <- read.csv("./results/max_lat/unimodal_type.csv")
data$mid <- abs(data$mid)
data$zone <- NA

data$zone[which(data$mid > tropical[1] & data$mid < tropical[2])] <- 1
data$zone[which(data$mid > temperate[1] & data$mid < temperate[2])] <- 2
data$zone[which(data$mid > polar[1] & data$mid < polar[2])] <- 3

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

unimodal_samp <- (sum(sim$zone == samp$zone)/56)*100
unimodal_rare <- (sum(sim$zone == rare$zone)/56)*100

#bimodal
data <- read.csv("./results/max_lat/bimodal_type.csv")
data$mid <- abs(data$mid)
data$zone <- NA

data$zone[which(data$mid > tropical[1] & data$mid < tropical[2])] <- 1
data$zone[which(data$mid > temperate[1] & data$mid < temperate[2])] <- 2
data$zone[which(data$mid > polar[1] & data$mid < polar[2])] <- 3

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

bimodal_samp <- (sum(sim$zone == samp$zone)/56)*100
bimodal_rare <- (sum(sim$zone == rare$zone)/56)*100


#flat
data <- read.csv("./results/max_lat/flat_type.csv")
data$mid <- abs(data$mid)
data$zone <- NA

data$zone[which(data$mid > tropical[1] & data$mid < tropical[2])] <- 1
data$zone[which(data$mid > temperate[1] & data$mid < temperate[2])] <- 2
data$zone[which(data$mid > polar[1] & data$mid < polar[2])] <- 3

sim <- subset(data, type == "simulated")
samp <- subset(data, type == "sampled")
rare <- subset(data, type == "rarefied")

flat_samp <- (sum(sim$zone == samp$zone)/56)*100
flat_rare <- (sum(sim$zone == rare$zone)/56)*100

LBG_type <- c("Flat", "Unimodal", "Bimodal")
Sampled <- round(c(flat_samp, unimodal_samp, bimodal_samp), 2)
Sampling_standardised <- round(c(flat_rare, unimodal_rare, bimodal_rare), 2)

data <- cbind(LBG_type, Sampled, Sampling_standardised)

write.csv(data, "./results/max_lat/formatted_table_zone.csv", row.names = FALSE)
