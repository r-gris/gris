# library(raster)
#  library(rgl)
#  library(rglgris)
#  library(rgdal)
#  f <- "E:\\DATA\\NASA\\world.topo.bathy.200411.3x21600x21600.D2_10.tif"
# 
#  esource <- readAll(crop(raster("E:\\DATA\\Etopo\\Etopo1Ice\\Etopo1.tif"), extent(90, 180, -90, 0)))
# 
# 
#  r <- readAll(brick(f))
#  extent(r) <- extent(90, 180, -90, 0)
#  smashimate <- function(x, smash) {dim(x) <- dim(x)/smash; x}
# 
#  sm <-smashimate(r, 10)
#  rs <- setValues(sm, extract(r, coordinates(sm), method = "simple"))
#  cols <- brick2col(rs)
# 
#  ro <- bgl(rs, z = esource)
#  ro$vb[1:2,] <- t(project(t(ro$vb[1:2,]), "+proj=laea +lon_0=140 +lat_0=-55"))
#  ro$vb[3,] <- ro$vb[3,] * 30
#  shade3d(ro, col = rep(cols, each = 4), lit = FALSE)
#  library(graticule)
#  lon <- seq(90, 180, by = 10)
#  lat <- seq(-85, 0, by = 15),
#  l <- graticule(lon, lat, proj = "+proj=laea +lon_0=140 +lat_0=-55")
#  for (i in seq(nrow(l))) {
#  xy <- coordinates(as(l[i,], "SpatialPoints"))
#   rgl.lines(cbind(xy, 1000), col = "white")
#   }
