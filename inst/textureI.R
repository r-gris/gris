load("C:\\Users\\michae_sum\\Downloads\\MOD021KM.A2015114.0200.006.2015114135358_lon_lat_em.RData")
library(gris)
library(rgdal)
library(RTriangle)
library(rgl)
p <- pslg(cbind(values(lon), values(lat)))
tr <- RTriangle::triangulate(p)

im <- setExtent(em, extent(0, 1, 0, 1))
tcoords <- xyFromCell(im, cellFromXY(im, coordinates(setExtent(lon, extent(0, 1, 0, 1)))))

im[] <- scl(values(im)) * 150 + 155
writeGDAL(as(im, "SpatialGridDataFrame"), "im.png", drivername="PNG", type = "Byte", mvFlag = 255)
tri <- tetrahedron3d()
pp <- "+proj=laea +lon_0=90 +lat_0=-68"
tri$vb <- t(cbind(project(tr$P, pp),  0, 1))
tri$it <- t(tr$T)

shade3d(tri, col = "white", texcoords = tcoords[tri$it, ], texture = "imrgb.png")

