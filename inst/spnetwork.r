
## equivalent to spn.Rmd
set.seed(5432)
x = sort(runif(100)) # sort, so x[1] is left, x[100] is right
y = runif(100)
library(RTriangle)
tr <- RTriangle::triangulate(pslg(P = cbind(x, y)))
g <- as.gris(tr)
pts = SpatialPoints(cbind(x, y))

tr <- RTriangle::triangulate(pslg(coordinates(pts)))
g <- gris:::.tri2gris(tr, type = "poly")

ig <- graph(as.vector(t(tr$E)))
ig$x <- tr$P[,1]
ig$y <- tr$P[,2]
ig$n <- table(tr$E)

## once the lengths are correct . . .
ln <- numeric(nrow(tr$E))
for (i in seq_along(ln)) ln[i] <- spDists(tr$P[tr$E[i, ], ], segments = TRUE)

E(ig)$weight <- ln
path = get.shortest.paths(ig, 1, 100, output = "both")
sp = as.vector(path$vpath[[1]])
ids = as_ids(path$epath[[1]])
plot(g, asp = 1)
points(ig$x[sp], ig$y[sp], col = "red", cex = 2)
