library(rasterVis)
library(png)
library(sf)
library(RColorBrewer)

dir.create("./results/GIF/")

my.settings <- list(
  par.main.text = list(font = 2, # make it bold
                       just = "left", 
                       x = grid::unit(5, "mm"))
)

collections <- list.files("./data/raw_data/binned_collections/masked/")

col <- rasterVis::GrTheme(region = (brewer.pal(9, 'Greys')))
col$panel.background$col <- "white"


for(i in collections){
  name <- toupper(tools::file_path_sans_ext(i))
  tmp <- read.csv(paste("./data/raw_data/binned_collections/masked/", i, sep = ""))
  tmp <- subset(tmp, !is.na(paleolat))
  min_age <- max(round(unique(tmp$max_ma), digits = 2))
  max_age <- min(round(unique(tmp$min_ma), digits = 2))
  age <- (max_age + min_age)/2
  x <- tmp$paleolng
  y <- tmp$paleolat
  z <- tmp$collection_no
  xyz <- cbind.data.frame(x, y, z)
  xyz <- sp::SpatialPointsDataFrame(coords = cbind.data.frame(xyz$x, xyz$y), data = xyz)
  r <- raster(res = 1)
  ras <- rasterize(xyz, r, 'z', function(x, ...) length(unique(na.omit(x))))
  ras[ras > 0] <- 1
  ras[is.na(ras)] <- 0
  crs(ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  png(paste("./results/GIF/", age, "_", tolower(name), ".png", sep = ""), width = 297, height = 210, units = "mm", res = 900)
  p <- rasterVis::levelplot(ras, main = paste(name,": ", min_age, "\u2012", max_age, " Ma", sep = ""), 
                            scales=list(x=list(cex=1),y=list(cex=1)), ylab = list("Palaeolatitude", fontface = "bold", fontsize = 16),
                            xlab = list("Palaeolongitude", fontface = "bold", fontsize = 16), colorkey = FALSE,
                            margin=list(FUN = 'sum', axis=grid::gpar(col = 'black', fontsize = 12, fontface = "bold")), par.settings = col)
  
  p$par.settings$layout.heights[
    c( 'bottom.padding',
       'top.padding',
       'key.sub.padding',
       'axis.xlab.padding',
       'key.axis.padding',
       'main.key.padding'
    ) ] <- 2
  print(p)
  dev.off()
}


library(gifski)

png_files <- stringr::str_sort(list.files("./results/GIF/"), numeric = TRUE, decreasing = TRUE)
gif_file <- tempfile(tmpdir = "./results/GIF/", fileext = ".gif")
png_files <- paste("./results/GIF/", png_files, sep = "")
gifski(png_files, gif_file, delay = 1, progress = TRUE)
#unlink(png_files)
file.rename(from = gif_file, to = "./results/GIF/spatial_sampling.gif")
