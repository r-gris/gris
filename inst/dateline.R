library(gris)
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, col = colorRampPalette(c("yellow", "orange", "brown"))(nrow(wrld_simpl)))
g <- gris(wrld_simpl)
library(RTriangle)
p <- mkpslg(g)

p$P <- rbind(p$P, matrix(c(162, 162, -90, 90), ncol = 2))
p$S <- rbind(p$S, as.integer(nrow(p$P) + c(-1, 0)))


tr <- triangulate(p, a = 5, D = TRUE, q = 1)

#plot(tr)
prj <-  "+proj=robin +lon_0=162 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

library(rgdal)


tr$P <-  project(tr$P, prj)
plot(tr)



g2 <- g
g2$v$x <- xy[,1]
g2$v$y <- xy[,2]
plot(g2)
tr2 <- tr
names(tr2)
dim(tr2$P)
tr2$P <- project(tr2$P, prj)
plot(tr2)
history(Inf)

