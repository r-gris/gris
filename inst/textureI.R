load("MOD021KM.A2015114.0200.006.2015114135358_lon_lat_em.RData")
library(gris)
library(rgdal)
library(RTriangle)
library(rgl)
library(raster)



mlon <- resample(lon, setExtent(em, extent(lon)), method = "bilinear")
mlat <- resample(lat, setExtent(em, extent(lat)), method = "bilinear")

# lon1 <- getValues(mlon, format = "matrix")
# lat1 <- getValues(mlat, format = "matrix")
# em1 <- getValues(em, format = "matrix")
# 
# em2 <- em1
# lat2 <- lat1
# lon2 <- lon1
# for (i in seq(ncol(lat1))) {
#   ord <- order(lat1[,i], decreasing = TRUE)
#   em2[,i] <- em1[, i][ord]
# 
#   lon2[,i] <- lon1[, i][ord]
#   lat2[,i] <- lat1[, i][ord]
# }
# 
# ext <- extent(0, 0.2, 0.8, 1)
# plot(crop(setExtent(em, extent(0, 1, 0, 1)), ext), col = grey(seq(0, 1, length = 256)))
# title("bogus")
# plot(crop(raster(em2), ext), col = grey(seq(0, 1, length = 256)))

library(graticule)
g <- graticule(seq(65, 150, by = 10), seq(-80, -55, by = 5))
pg <- function() {
  plot(spTransform(g, pp), add = TRUE)
  invisible(NULL)
}

library(rworldxtra)
 data(countriesHigh)


pp <- "+proj=omerc +lonc=108 +lat_0=-67 +gamma=10 +alpha=31"
xy <- project(cbind(values(mlon), values(mlat)), pp)
scl <- function(x) (x - na.omit(min(x)))/diff(range(na.omit(x)))
ss <- seq(nrow(xy)) ## sample(nrow(xy),5e5 )
plot(xy[ss, ], pch = ".", col = grey(seq(0, 1, length = 256)[scl(values(em)[ss]) * 255 + 1]), asp =1 )
pg()
w <- spTransform(subset(countriesHigh, SOVEREIGNT == "Antarctica"), pp)
plot(w, add = TRUE, border = "pink")
points(project(cbind(108, -67), pp), col = "red")

p <- pslg(cbind(values(lon), values(lat)))
tr <- RTriangle::triangulate(p)
as(tr, "gris")

im <- setExtent(em, extent(0, 1, 0, 1))
tcoords <- xyFromCell(im, cellFromXY(im, coordinates(setExtent(lon, extent(0, 1, 0, 1)))))

im[] <- scl(values(im)) * 150 + 155
writeGDAL(as(im, "SpatialGridDataFrame"), "im.png", drivername="PNG", type = "Byte", mvFlag = 255)
tri <- tetrahedron3d()
pp <- "+proj=laea +lon_0=90 +lat_0=-68"
tri$vb <- t(cbind(project(tr$P, pp),  0, 1))
tri$it <- t(tr$T)

shade3d(tri, col = "white", texcoords = tcoords[tri$it, ], texture = "imrgb.png")

