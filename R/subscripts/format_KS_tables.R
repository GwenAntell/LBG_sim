#format K-S tables

data <- read.csv("./results/frequency_dist/occ_KS_test.csv")
data

Comparison <- c("Simulated and Anthozoa",
                "Simulated and Porifera",
                "Simulated and Bivalvia",
                "Simulated and Brachiopoda",
                "Simulated and Echinoidea",
                "Anthozoa and Porifera",
                "Anthozoa and Bivalvia",
                "Anthozoa and Brachiopoda",
                "Anthozoa and Echinoidea",
                "Bivalvia and Porifera",
                "Bivalvia and Brachiopoda",
                "Bivalvia and Echinoidea",
                "Brachiopoda and Porifera",
                "Brachiopoda and Echinoidea",
                "Porifera and Echinoidea")
D_Statistic <- round(data$statistic.D, digits = 3)
P <- round(data$p.value, digits = 3)
vec <- which(P <= 0.05)
P[vec] <- paste(P[vec], "*", sep = "")
vec <- which(P == "0*")
P[vec] <- paste("< 0.01*", sep = "")

data <- cbind.data.frame(Comparison, D_Statistic, P)
data[order(data$Comparison, decreasing = TRUE),]
write.csv(data, "./results/frequency_dist/occ_KS_test_formatted.csv", row.names = FALSE)


#Latitude
Latitude <- read.csv("./results/frequency_dist/LAT_KS_test.csv")
Latitude

Metric <- "Latitude"

Comparison <- c("Simulated and Anthozoa",
                "Simulated and Porifera",
                "Simulated and Bivalvia",
                "Simulated and Brachiopoda",
                "Simulated and Echinoidea",
                "Anthozoa and Porifera",
                "Anthozoa and Bivalvia",
                "Anthozoa and Brachiopoda",
                "Anthozoa and Echinoidea",
                "Bivalvia and Porifera",
                "Bivalvia and Brachiopoda",
                "Bivalvia and Echinoidea",
                "Brachiopoda and Porifera",
                "Brachiopoda and Echinoidea",
                "Porifera and Echinoidea")
D_Statistic <- round(Latitude$statistic.D, digits = 3)
P <- round(Latitude$p.value, digits = 3)
vec <- which(P <= 0.05)
P[vec] <- paste(P[vec], "*", sep = "")
vec <- which(P == "0*")
P[vec] <- paste("< 0.01*", sep = "")

Latitude <- cbind.data.frame(Comparison, Metric, D_Statistic, P)


#Longitude
Longitude <- read.csv("./results/frequency_dist/LNG_KS_test.csv")
Longitude

Metric <- "Longitude"

Comparison <- c("Simulated and Anthozoa",
                "Simulated and Porifera",
                "Simulated and Bivalvia",
                "Simulated and Brachiopoda",
                "Simulated and Echinoidea",
                "Anthozoa and Porifera",
                "Anthozoa and Bivalvia",
                "Anthozoa and Brachiopoda",
                "Anthozoa and Echinoidea",
                "Bivalvia and Porifera",
                "Bivalvia and Brachiopoda",
                "Bivalvia and Echinoidea",
                "Brachiopoda and Porifera",
                "Brachiopoda and Echinoidea",
                "Porifera and Echinoidea")
D_Statistic <- round(Longitude$statistic.D, digits = 3)
P <- round(Longitude$p.value, digits = 3)
vec <- which(P <= 0.05)
P[vec] <- paste(P[vec], "*", sep = "")
vec <- which(P == "0*")
P[vec] <- paste("< 0.01*", sep = "")

Longitude <- cbind.data.frame(Comparison, Metric, D_Statistic, P)

#Longitude
GCD <- read.csv("./results/frequency_dist/GCD_KS_test.csv")
Longitude

Metric <- "GCD"

Comparison <- c("Simulated and Anthozoa",
                "Simulated and Porifera",
                "Simulated and Bivalvia",
                "Simulated and Brachiopoda",
                "Simulated and Echinoidea",
                "Anthozoa and Porifera",
                "Anthozoa and Bivalvia",
                "Anthozoa and Brachiopoda",
                "Anthozoa and Echinoidea",
                "Bivalvia and Porifera",
                "Bivalvia and Brachiopoda",
                "Bivalvia and Echinoidea",
                "Brachiopoda and Porifera",
                "Brachiopoda and Echinoidea",
                "Porifera and Echinoidea")
D_Statistic <- round(GCD$statistic.D, digits = 3)
P <- round(GCD$p.value, digits = 3)
vec <- which(P <= 0.05)
P[vec] <- paste(P[vec], "*", sep = "")
vec <- which(P == "0*")
P[vec] <- paste("< 0.01*", sep = "")

GCD <- cbind.data.frame(Comparison, Metric, D_Statistic, P)

data <- rbind.data.frame(Latitude, Longitude, GCD)

data <- data[order(data$Comparison, decreasing = TRUE),]
write.csv(data, "./results/frequency_dist/range_KS_test_formatted.csv", row.names = FALSE)

