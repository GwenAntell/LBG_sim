#global diversity
source("./R/options.R")
stages <- read.csv("./data/raw_data/stages.csv")

files <- list.files(path = "./results/unimodal/LBGs/Sampled/", pattern = "_sampled.csv")
dir.create("./results/global/")

#unimodal
master <- data.frame()
for(i in files){
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  data <- read.csv(paste("./results/unimodal/LBGs/Sampled/", i, sep = ""))
  reps <- unique(data$rep)

  df <- lapply(reps, function (x){
    n <- x
    dat <- subset(data, rep == n)
    dat <- length(unique(dat$id))
    dat
  })

  df <- do.call(cbind, df)
  mean_richness <- mean(df)
  
  quant <- quantile(df, probs = c(0.025, 0.975))
  CI.Lower <- quant[1]
  CI.Upper <- quant[2]
  
  df <- cbind.data.frame(name, mean_richness, CI.Lower, CI.Upper)
  master <- rbind.data.frame(master, df)
}

rownames(master) <- c()
master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/global/unimodal_global_div.csv", row.names = FALSE)

#bimodal
master <- data.frame()
for(i in files){
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  data <- read.csv(paste("./results/bimodal/LBGs/Sampled/", i, sep = ""))
  reps <- unique(data$rep)
  
  df <- lapply(reps, function (x){
    n <- x
    dat <- subset(data, rep == n)
    dat <- length(unique(dat$id))
    dat
  })
  
  df <- do.call(cbind, df)
  mean_richness <- mean(df)
  
  quant <- quantile(df, probs = c(0.025, 0.975))
  CI.Lower <- quant[1]
  CI.Upper <- quant[2]
  
  df <- cbind.data.frame(name, mean_richness, CI.Lower, CI.Upper)
  master <- rbind.data.frame(master, df)
}

rownames(master) <- c()
master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/global/bimodal_global_div.csv", row.names = FALSE)

#flat

master <- data.frame()
for(i in files){
  name <- tools::file_path_sans_ext(i)
  name <- gsub("_.*","",name)
  data <- read.csv(paste("./results/flat/LBGs/Sampled/", i, sep = ""))
  reps <- unique(data$rep)
  
  df <- lapply(reps, function (x){
    n <- x
    dat <- subset(data, rep == n)
    dat <- length(unique(dat$id))
    dat
  })
  
  df <- do.call(cbind, df)
  mean_richness <- mean(df)
  
  quant <- quantile(df, probs = c(0.025, 0.975))
  CI.Lower <- quant[1]
  CI.Upper <- quant[2]
  
  df <- cbind.data.frame(name, mean_richness, CI.Lower, CI.Upper)
  master <- rbind.data.frame(master, df)
}

rownames(master) <- c()
master <- plyr::join(master, stages, by = "name", type = "left")
master <- master[order(master$max_age),]

write.csv(master, "./results/global/flat_global_div.csv", row.names = FALSE)


