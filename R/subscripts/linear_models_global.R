# Lewis A. Jones
#---------------------------------
library(ggplot2)
library(ggpubr)
col <- c("#1b9e77", "#d95f02", "#7570b3")
stages <- read.csv("./data/raw_data/stages.csv") #load stage bins
periods <- read.csv("./data/raw_data/periods.csv") #load period bins
periods <- periods[2:7,]
MST <- read.csv("./results/MST/global_MST.csv")
MST$MST[MST$MST == 0] <- NA
SSC <- read.csv("./results/SSC/global_SSC.csv")
SSC$ssc[SSC$ssc == 0] <- NA

source("./R/functions/equations.R")
#---------------------------------

flat <- read.csv("./results/global/flat_global_div.csv")
unimodal <- read.csv("./results/global/unimodal_global_div.csv")
bimodal <- read.csv("./results/global/bimodal_global_div.csv")

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
#---------------------------------
#assign period colours
SSC$periodcol <- NA
for(i in 1:nrow(periods)){
  for(j in 1:nrow(SSC)){
    if(SSC$mid_age[j] <= periods$max_age[i] & SSC$mid_age[j] >= periods$min_age[i]){
      SSC$periodcol[j] <- paste(periods$color[i])}
  }
}
#---------------------------------
#layout
png("./figures/linear_models_global.png", height = 140, width = 200, unit = "mm", res = 900)

m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
layout(mat = m,heights = c(0.8, 0.8, 0.1))
#---------------------------------

plot(log(flat$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol), ylab = "ln Species Richness", xlab = "ln Spatial sampling coverage (%)")
title("a", adj = 0)
title("Flat-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(flat$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol))
box()
abline(fit <- lm(log(flat$mean_richness) ~ log(SSC$ssc)), col='black', lwd = 2)
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(unimodal$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol), ylab = "ln Species Richness", xlab = "ln Spatial sampling coverage (%)")
title("b", adj = 0)
title("Unimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(unimodal$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol))
box()
abline(fit <- lm(log(unimodal$mean_richness) ~ log(SSC$ssc)), col='black', lwd = 2)
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(bimodal$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol), ylab = "ln Species Richness", xlab = "ln Spatial sampling coverage (%)")
title("c", adj = 0)
title("Bimodal-type", adj = 0.5)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(bimodal$mean_richness)~log(SSC$ssc),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(SSC$periodcol))
box()
abline(fit <- lm(log(bimodal$mean_richness) ~ log(SSC$ssc)), col='black', lwd = 2)
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(flat$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("d", adj = 0)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(flat$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(flat$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(flat$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------
#---------------------------------

plot(log(unimodal$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("e", adj = 0)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(unimodal$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(unimodal$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(unimodal$mean_richness), log(MST$MST))
pVal <- anova(fit)$'Pr(>F)'[1]
pVal <- round(pVal, digits = 3)
if(pVal == 0){
  pVal <- "<0.001"
}
legend("bottomright",legend = parse(text = equation(fit)), inset=c(0.0,0.98), xpd=TRUE, horiz=TRUE, bty="n")
#---------------------------------

plot(log(bimodal$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol), ylab = "ln Species Richness", xlab = "ln Summed MST length (km)")
title("f", adj = 0)
abline(v=0, col="black", lty = 2)
abline(h=0, col="black", lty = 2)
points(log(bimodal$mean_richness)~log(MST$MST),  las = 1, pch = 21, cex = 1, col = "black", bg = paste(MST$periodcol))
box()
abline(fit <- lm(log(bimodal$mean_richness) ~ log(MST$MST)), col='black', lwd = 2)
Fit <- cor.test(log(bimodal$mean_richness), log(MST$MST))
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