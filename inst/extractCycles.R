## example problem taken from here: https://stat.ethz.ch/pipermail/r-sig-geo/2015-November/023667.html
## coordinate "1287248.96712942" should be "1287248.96712943" or rounded somehow
library(sp)
library(rgeos)
p1 <- Polygon(cbind(
      c(1276503.26781119, 1281876.11747031, 1287248.96712942,
        1287248.96712942, 1281876.11747031, 1276503.26781119, 1276503.26781119),
      c(204391.40834643, 207493.42454344, 204391.40834643, 198187.37595242,
        195085.35975541, 198187.37595242, 204391.40834643)))
p2 <- Polygon(cbind(
      c(1287248.96712943, 1292621.81678854, 1297994.66644766,
        1297994.66644766, 1292621.81678854, 1287248.96712943, 1287248.96712943),
      c(204391.40834643, 207493.42454344, 204391.40834643, 198187.37595242,
        195085.35975541, 198187.37595242, 204391.40834643)))
p3 <- Polygon(cbind(
      c(1281876.11747031, 1287248.96712943, 1292621.81678854,
        1292621.81678854, 1287248.96712943, 1281876.11747031, 1281876.11747031),
      c(213697.45693745, 216799.47313446, 213697.45693745, 207493.42454344,
        204391.40834643, 207493.42454344, 213697.45693745)))
spoly <- SpatialPolygons(list(Polygons(list(p1, p2, p3), 's1')))
plot(gUnaryUnion(spoly))
library(gris)

g <- RTriangle::triangulate(mkpslg(gris(SpatialPolygonsDataFrame(spoly, data.frame(x = 1), match.ID = FALSE))))
plot(g, asp = 1)


gr <- gris(SpatialPolygonsDataFrame(spoly, data.frame(x = 1), match.ID = FALSE))
gr2 <-  gris:::bld2(SpatialPolygonsDataFrame(spoly, data.frame(x = 1), match.ID = FALSE), normalize_verts = FALSE)

## truncate brutally
gr2$v <-  gr2$v %>% mutate( x = round(x, digits = 7), y = round(y, digits = 7))

grf <-  gris:::normalizeVerts2(gr2$v , gr2$bXv, c("x", "y"))
gr2$v <- grf$v
gr2$bXv <- grf$bXv


gt <- RTriangle::triangulate(mkpslg(gr2))

## find edges and pull out all segments that trace a border
edges1 <- logical(nrow(gt$S))
m1 <- t(apply(rbind(gt$T[,1:2], gt$T[,2:3], gt$T[,c(1, 3)]) , 1, sort))

for (i in seq(nrow(gt$S))) {edges1[i] <- sum(rowSums(abs(m1 - matrix(sort(gt$S[i,]), ncol = 2, nrow = nrow(gt$T) * 3, byrow = TRUE))) == 0) == 1}

plot(gt, asp = 1)
text(gt$P, lab = seq(nrow(gt$P)), pos = 2)
apply(gt$S[edges1, ], 1, function(x) lines(gt$P[x, ], lwd = 2))



## another example of cycle extraction
oz <- gris(disaggregate(subset(wrld_simpl, NAME == "Australia"))[1, ]);plot(oz)

gt <- RTriangle::triangulate(mkpslg(oz))

## find edges and pull out all segments that trace a border
edges1 <- logical(nrow(gt$S))
m1 <- t(apply(rbind(gt$T[,1:2], gt$T[,2:3], gt$T[,c(1, 3)]) , 1, sort))

for (i in seq(nrow(gt$S))) {edges1[i] <- sum(rowSums(abs(m1 - matrix(sort(gt$S[i,]), ncol = 2, nrow = nrow(gt$T) * 3, byrow = TRUE))) == 0) == 1}

plot(gt, asp = 1)
text(gt$P, lab = seq(nrow(gt$P)), pos = 2)
apply(gt$S[edges1, ], 1, function(x) lines(gt$P[x, ], lwd = 2))


plot(SpatialPolygons(list(Polygons(list(Polygon(gt$P[gt$S[,1], ])), "1"))))
## or
plot(SpatialPolygons(list(Polygons(list(Polygon(gt$P[sort(unique(gt$S)), ])), "1"))))
