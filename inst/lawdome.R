library(rworldxtra)
data(countriesHigh)
library(rgdal)
library(raadtools)
library(rgl)
library(dismo)
library(gris)

library(raster)
topo0 <- raster("\\\\aad.gov.au\\files\\AADC\\Scientific_Data\\Data\\gridded_new\\data\\hs.pangaea.de\\Maps\\bathy\\IBCSO_v1\\ibcso_v1_is.grd")
projection(topo0) <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
w <- spTransform(subset(countriesHigh, SOVEREIGNT == "Antarctica"), projection(topo0))
ex <- new("Extent"
          , xmin = 2000000
          , xmax = 2800000
          , ymin = -1466414.70001993
          , ymax = -600000
)

topo <- aggregate(crop(topo0, ex, snap = "out"), fact = 8, fun = mean)
plot(topo)
contour(topo, lev = 0, add = TRUE)
plot(w, add = TRUE)


gm0 <- gmap(topo, type = "satellite")

gm <- setValues(brick(gm0, gm0, gm0), t(col2rgb(gm0@legend@colortable[values(gm0) + 1])))
mapfile <- "gmap.png"
writeGDAL(as(gm, "SpatialGridDataFrame"), mapfile, drivername = "PNG")

surf <- bgl(topo, topo)
tcoords <- xyFromCell(setExtent(gm, extent(0, 1, 0, 1)), 
                      cellFromXY(gm, spTransform(SpatialPoints(t(surf$vb[1:2, ]), proj4string = CRS(projection(topo))), projection(gm))))



ice <- readice("2015-02-15", setNA = FALSE)
ice[ice > 100 | ice < 1] <- NA_real_
ice <- crop(ice, projectExtent(topo, projection(ice)))

icesurf <- bgl(ice, ice, na.rm = TRUE)
icesurf$vb[1:2, ] <- t(coordinates( spTransform(SpatialPoints(t(icesurf$vb[1:2,]), proj4string = CRS(projection(ice))), projection(topo))))

scl <- function(x) (x - min(x, na.rm = TRUE))/diff(range(na.omit(x)))

icecol <- grey(seq(0, 1, length = 100))[scl(icesurf$vb[3,]) * 99 + 1]
icecol[icesurf$vb[3,] < 1 | icesurf$vb[3,] > 100] <- NA
icesurf$vb[3,] <- 0

surf$vb[3,is.na(surf$vb[3,])] <- 0

shade3d(surf, col = "white", texcoords = tcoords[surf$ib, ], texture = mapfile)
shade3d(icesurf, col = icecol[icesurf$ib], alpha = 0.7)
aspect3d(1, 1, .4)

writeWebGL()
