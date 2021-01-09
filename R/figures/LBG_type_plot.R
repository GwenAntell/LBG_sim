source("./R/functions/LBG_type.R")
source("./R/options.R")
library(raster)
library(png)
library(grid)
library(ggplot2)
library(gridExtra)

flat <- LBG_type(type = "flat", sd = 0, res = res)
unimodal <- LBG_type(type = "unimodal", sd = 20, res = res)
bimodal <- LBG_type(type = "bimodal", sd = 10, res = res)

stk <- stack(flat, unimodal, bimodal)
names(stk) <- c("Flat", "Unimodal", "Bimodal")

col <- viridisLite::viridis(100)

png("./figures/flat_type_plot.png", width = 200, height = 100, units = "mm", res = 600)
p <- rasterVis::levelplot(stk[[1]], margin = list(draw = TRUE, FUN = 'mean', scales = list(x = NULL, y = c(0,1)), axis = gpar(col = 'black', fontsize = 10)), main = "Flat-type     ", colorkey = FALSE, layout = c(1,1), col.regions = col)
p$legend$top <- NULL
p$par.settings$layout.heights[
  c( 'bottom.padding',
     'top.padding',
     'key.sub.padding',
     'axis.xlab.padding',
     'key.axis.padding',
     'main.key.padding'
  ) ] <- 1
print(p)
dev.off()

png("./figures/unimodal_type_plot.png", width = 200, height = 100, units = "mm", res = 600)
p <- rasterVis::levelplot(stk[[2]], margin = list(draw = TRUE, FUN = 'mean', x = NULL, axis = gpar(col = 'black', fontsize = 10)), main = "Unimodal-type     ", colorkey = FALSE, layout = c(1,1), col.regions = col)
p$legend$top <- NULL
p$par.settings$layout.heights[
  c( 'bottom.padding',
     'top.padding',
     'key.sub.padding',
     'axis.xlab.padding',
     'key.axis.padding',
     'main.key.padding'
  ) ] <- 1
print(p)
dev.off()

png("./figures/bimodal_type_plot.png", width = 200, height = 100, units = "mm", res = 600)
p <- rasterVis::levelplot(stk[[3]], margin = list(draw = TRUE, FUN = 'mean', x = NULL, axis = gpar(col = 'black', fontsize = 10)), main = "Bimodal-type     ", colorkey = FALSE, layout = c(1,1), col.regions = col)
p$legend$top <- NULL
p$par.settings$layout.heights[
  c( 'bottom.padding',
     'top.padding',
     'key.sub.padding',
     'axis.xlab.padding',
     'key.axis.padding',
     'main.key.padding'
  ) ] <- 1
print(p)
dev.off()

p <- c("./figures/flat_type_plot.png", "./figures/unimodal_type_plot.png", "./figures/bimodal_type_plot.png")

plots <- lapply(p,function(x){
  plot <- as.raster(readPNG(x))
  plot <- rasterGrob(plot, interpolate = FALSE)
  plot
})

g <- ggarrange(plots[[1]], plots[[2]], plots[[3]], labels = "auto", nrow = 3, ncol = 1, font.label = list(size = 16))

ggsave("./figures/LBG_type_plot.png",g,width = 165, height = 250, units = "mm", dpi = 600)
