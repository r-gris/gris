## BRAN files
## we need a model for this stuff

library(raster)
library(ncdf)
library(dplyr)
fun <- function(x, grid, vname) {
  on.exit(close.ncdf(nc))
  nc <- open.ncdf(x)
  tlen <- dim.inq.ncdf(nc, 1)$len
  r <- raster(x, varname = vname, stopIfNotEqualSpaced = FALSE, band = 1)/tlen
  #return(r)
  ## calculate entire mean
  for (i in seq(2, tlen)) {
    r <- r + raster(x, varname = vname, stopIfNotEqualSpaced = FALSE, band = i)/tlen 
  }
  lon <- get.var.ncdf(nc, "xt_ocean")
  lat <- get.var.ncdf(nc, "yt_ocean")
  ## we must reverse lat to get the orientation right
  pts <- as.matrix(expand.grid(lon, rev(lat)))
  
  celltab <- cellFromXY(grid, pts)
 
  vals <- data_frame(cell = celltab, val = values(r)) %>% 
    filter(!is.na(cell)) %>% 
    group_by(cell) %>% 
    summarize(mean = mean(val, na.rm = TRUE))
    
  grid[] <- NA_real_
  grid[vals$cell] <- vals$mean
  
  grid
}

dummy <- raster(extent(15, 180, -80, -30), res = 5, crs = "+proj=longlat +ellps=WGS84")
x1 <- fun(fs[1], dummy, "temp")

library(palr)
pal <- sstPal(palette = TRUE)
plot(x1, col = pal$cols, breaks = pal$breaks, legend = FALSE)
  
