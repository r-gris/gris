## edge  pairs from sequential indexes for polygons
prs1 <- function(x) {
  x1 <- cbind(head(x, -1), tail(x, -1))
  rbind(x1, c(x1[length(x1)], x1[1]))
}
pquads1 <- function(x, texture = NULL, texcoords = NULL, subset = NULL, ...) {
  if (is.null(texcoords)) texcoords <- t(x$vb[1:2,x$ib])
  if (!is.null(subset)) x$ib <- x$ib[,subset]
  rgl.quads(x$vb[1,x$ib], x$vb[2,x$ib], x$vb[3,x$ib], texcoords = texcoords, texture = texture, ...)
}

allcoords <- function(x) {
  coordinates(as(as(x, "SpatialLines"), "SpatialPoints"))
}
library(gris)
library(RTriangle)
library(maptools)
data(wrld_simpl)
o1 <- gris(subset(wrld_simpl, NAME %in% c("Australia", "Indonesia", "United States")))
o <- gris:::normalizeVerts(o1, c("x", "y"))
p <- pslg(P = o$v %>% dplyr::select(x, y) %>% as.matrix(),
          S = do.call(rbind, lapply(split(o$bXv$.vx0, o$bXv$.br0), prs1)))
plot(o1)

tr <- RTriangle::triangulate(p, a = 5e1)
plot(p)
plot(tr, cex = 0.2)


library(rgl)
library(rglgris)
tri <- tetrahedron3d()
tri$vb <- t(cbind(llh2xyz(cbind(tr$P, 0)),1))
tri$it <- t(tr$T)
wimg <- brick("world.topo.bathy.200411.3x5400x2700.png")
tcoords <- xyFromCell(setExtent(wimg, extent(0, 1, 0, 1)), cellFromXY(wimg, tr$P))
shade3d(tri, col = "white", texture = "world.topo.bathy.200411.3x5400x2700.png", texcoords = tcoords[tri$it, ])



