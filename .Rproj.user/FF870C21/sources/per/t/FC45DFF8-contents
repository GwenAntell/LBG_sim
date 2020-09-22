#frequency distributions

source("./R/functions/range_freq.R")
source("./R/functions/occ_freq.R")
data <- read.csv("./results/unimodal/pre_industrial.csv")
n <- sample(x = unique(data$rep), size = 1)
sample_sim <- subset(data, rep == n)
#range
sim_lat <- range_freq(data = sample_sim, metric = "latitude", breaks = 10)
sim_lng <- range_freq(data = sample_sim, metric = "longitude", breaks = 10)
sim_GCD <- range_freq(data = sample_sim, metric = "GCD", breaks = 2000)

#occurrence
sim_occ <- occ_freq(data = sample_sim, breaks = 10)
