## edge  pairs from sequential indexes for polygons
prs1 <- function(x) {
  x1 <- cbind(head(x, -1), tail(x, -1))
  rbind(x1, c(x1[length(x1)], x1[1]))
}
library(gris)
library(RTriangle)
library(maptools)
data(wrld_simpl)
o <- gris(subset(wrld_simpl, NAME == "Australia"))
##o <- gris(wrld_simpl[sample(nrow(wrld_simpl), 1), ])
##o <- gris(wrld_simpl[sample(nrow(wrld_simpl), 10), ])
p <- pslg(P = o$v %>% select(x, y) %>% as.matrix(), PB = NA, PA = NA,
     S = do.call(rbind, lapply(split(o$bXv$.vx0, o$bXv$.br0), prs1))
, SB = NA, H = NA)
tr <- RTriangle::triangulate(p, a = 1e1)
plot(tr, cex = 0.2)

o$tri <- tr$T

library(rgl)
library(rglgris)
tri <- tetrahedron3d()
tri$vb <- t(cbind(llh2xyz(cbind(tr$P, 0)),1))
tri$it <- t(tr$T)

