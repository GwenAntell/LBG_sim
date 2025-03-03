# Lewis A. Jones
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods <- periods[2:7,]
MST <- read.csv("./results/MST/lat_MST.csv")
#MST$MST <- MST$MST + 1
MST$MST[MST$MST == 0] <- NA
source("./R/functions/equations.R")
#---------------------------------

flat_sim <- read.csv("./results/compiled_LBGs/flat_simulated.csv")
flat_samp <- read.csv("./results/compiled_LBGs/flat_sampled.csv")
flat_rare <- read.csv("./results/compiled_LBGs/flat_rarefied.csv")
flat_sim <- flat_sim[order(flat_sim$mid_age),]
flat_samp <- flat_samp[order(flat_samp$mid_age),]
flat_rare<- flat_rare[order(flat_rare$mid_age),]
flat_sim$mean_richness[flat_sim$mean_richness < 1] <- NA
flat_samp$mean_richness[flat_samp$mean_richness < 1] <- NA
flat_rare$mean_richness[flat_rare$mean_richness < 1] <- NA


unimodal_sim <- read.csv("./results/compiled_LBGs/unimodal_simulated.csv")
unimodal_samp <- read.csv("./results/compiled_LBGs/unimodal_sampled.csv")
unimodal_rare <- read.csv("./results/compiled_LBGs/unimodal_rarefied.csv")
unimodal_sim <- unimodal_sim[order(unimodal_sim$mid_age),]
unimodal_samp <- unimodal_samp[order(unimodal_samp$mid_age),]
unimodal_rare<- unimodal_rare[order(unimodal_rare$mid_age),]
unimodal_sim$mean_richness[unimodal_sim$mean_richness < 1] <- NA
unimodal_samp$mean_richness[unimodal_samp$mean_richness < 1] <- NA
unimodal_rare$mean_richness[unimodal_rare$mean_richness < 1] <- NA


bimodal_sim <- read.csv("./results/compiled_LBGs/bimodal_simulated.csv")
bimodal_samp <- read.csv("./results/compiled_LBGs/bimodal_sampled.csv")
bimodal_rare <- read.csv("./results/compiled_LBGs/bimodal_rarefied.csv")
bimodal_sim <- bimodal_sim[order(bimodal_sim$mid_age),]
bimodal_samp <- bimodal_samp[order(bimodal_samp$mid_age),]
bimodal_rare<- bimodal_rare[order(bimodal_rare$mid_age),]
bimodal_sim$mean_richness[bimodal_sim$mean_richness < 1] <- NA
bimodal_samp$mean_richness[bimodal_samp$mean_richness < 1] <- NA
bimodal_rare$mean_richness[bimodal_rare$mean_richness < 1] <- NA


#---------------------------------
#assign period colours
MST$periodcol <- NA
for(i in 1:nrow(periods)){
  for(j in 1:nrow(MST)){
    if(MST$mid_age[j] <= periods$max_age[i] & MST$mid_age[j] >= periods$min_age[i]){
      MST$periodcol[j] <- paste(periods$color[i])}
  }
}
#---------------------------------
#layout
png("./figures/linear_models_MST.png", height = 220, width = 220, unit = "mm", res = 600)

m <- matrix(c(1,2,3,4,5,6,7,8,9,10,10,10),nrow = 4,ncol = 3,byrow = TRUE)
layout(mat = m,heights = c(0.8, 0.8, 0.8, 0.1))
#---------------------------------

plot(log(flat_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("a", adj = 0)
title("Simulated flat-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(flat_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(flat_sim$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(flat_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("b", adj = 0)
title("Sampled flat-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(flat_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(flat_samp$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(flat_samp$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(flat_rare$mean_richness)~log(MST$MST), las = 1,  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("c", adj = 0)
title("Rarefied flat-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(flat_rare$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(flat_rare$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(flat_rare$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(unimodal_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("d", adj = 0)
title("Simulated unimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(unimodal_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(unimodal_sim$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(unimodal_sim$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(unimodal_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("e", adj = 0)
title("Sampled unimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(unimodal_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(unimodal_samp$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(unimodal_samp$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(unimodal_rare$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("f", adj = 0)
title("Rarefied unimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(unimodal_rare$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(unimodal_rare$mean_richness) ~ (log(MST$MST))), col='black', lwd = 2)
Fit <- cor.test(log(unimodal_rare$mean_richness), (log(MST$MST)))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(bimodal_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("g", adj = 0)
title("Simulated bimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(bimodal_sim$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(bimodal_sim$mean_richness) ~ (log(MST$MST))), col='black', lwd = 2)
Fit <- cor.test(log(bimodal_sim$mean_richness), (log(MST$MST)))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(bimodal_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("h", adj = 0)
title("Sampled bimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(bimodal_samp$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(bimodal_samp$mean_richness) ~ (log(MST$MST))), col='black', lwd = 2)
Fit <- cor.test(log(bimodal_samp$mean_richness), (log(MST$MST)))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(bimodal_rare$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("i", adj = 0)
title("Rarefied bimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(bimodal_rare$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(bimodal_rare$mean_richness) ~ (log(MST$MST))), col='black', lwd = 2)
Fit <- cor.test(log(bimodal_rare$mean_richness), (log(MST$MST)))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

par(mar = c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top", inset = 0, bty = "n",
       legend = stringr::str_to_title(periods$name),
       col=paste(periods$color), pch=20, pt.cex = 2.5, cex= 1, x.intersp=1, text.width=c(0.1,0.1,0.1,0.1,0.1, 0.1), horiz = TRUE, xpd = FALSE)
dev.off()