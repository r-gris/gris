library(gris)
library(RTriangle)

prj <- "+proj=laea +lat_0=-90 +lon_0=90"
mpa <- spTransform(mpa, CRS(prj))
pred_poly <- spTransform(pred_poly, CRS(prj))

pred_poly$ima <- "pred"
mpa$ima <- "pred"
## combined polygon layer
spg <- spRbind(spChFIDs(mpa, as.character(nrow(pred_poly) + 1:3))["ima"], pred_poly["ima"])
g1 <- gris(mpa)
g2 <- gris(pred_poly)
## relation table version
g <- gris(spg)  

## planar straight line graph (for Triangle constrained triangulation )
tri <- RTriangle::triangulate(mkpslg(g))
g1tri <- RTriangle::triangulate(mkpslg(g1))
g2tri <- RTriangle::triangulate(mkpslg(g2))


## centre point of each triangle
pts <- t(apply(tri$T, 1, function(x) apply(tri$P[x, ], 2, mean)))

inpred1 <- !is.na(over(SpatialPoints(pts, proj4string = CRS(prj)), pred_poly)$ima)
inmpa1 <-  !is.na(over(SpatialPoints(pts, proj4string = CRS(prj)), mpa)$ima)

tri2 <- tri
tri2$T <- tri2$T[inpred1 & inmpa1, ]

plot(tri2$P[unique(tri2$T), ])
apply(tri2$T, 1, function(x) lines(tri2$P[x, ]))
