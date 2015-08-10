library(rgdal)
library(RTriangle)
prj <- "+proj=stere +lat_0=-90"
p <- pslg(P = project(geosphere::randomCoordinates(20000), prj)) ## mkpslg(g)
tr <- RTriangle::triangulate(p)
xyz <- llh2xyz(cbind(project(tr$P, prj, inv = TRUE), 0))
o$vb <- t(cbind(xyz, 1))
o$it <- t(tr$T)

shade3d(o, col = "white")
