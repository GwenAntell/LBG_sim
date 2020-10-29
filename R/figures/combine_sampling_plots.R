library(png)
library(grid)
library(ggplot2)
library(gridExtra)

p <- c("./figures/heat_map_ssc.png", "./figures/heat_map_mst.png")

plots <- lapply(p,function(x){
  plot <- as.raster(readPNG(x))
  plot <- rasterGrob(plot, interpolate = FALSE)
  plot
})

g <- ggarrange(plots[[1]], plots[[2]], labels = "auto", nrow = 2, ncol = 1, font.label = list(size = 18))

ggsave("./figures/sampling_plot.png",g,width = 200, height = 280, units = "mm", dpi = 600)

