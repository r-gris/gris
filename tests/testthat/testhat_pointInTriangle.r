# test_that("point in triangle works", {
#   library(maptools)
#   data(wrld_simpl)
#   g <- gris(wrld_simpl, triangulate = TRUE)
#   xy <- coordinates(wrld_simpl)
#   pit <- pointInTriangle(g, xy)
#   spoints <- SpatialPoints(coordinates(wrld_simpl), proj4string = CRS(proj4string(wrld_simpl)))
#   nover <- which(is.na(over(wrld_simpl, spoints)))
#   ## only Cuba and Haiti don't have their centroid inside a piece(!)
#   expect_that(length(na.omit(pit)), equals(nrow(xy) - 2) )
#   expect_that(levels(g$o$NAME)[g$o$NAME[is.na(pit)]], equals(c("Cuba", "Haiti")))
# })
# 
# op <- par(mfrow = n2mfrow(length(nover)), mar = rep(0, 4))
# for (i in seq_along(nover)) {
#   plot(wrld_simpl[nover[i], ], col = "grey")
#   
#   points(spoints[nover[i], , drop = FALSE], pch = "+", cex  =3)
#   points(xy[nover[i], , drop = FALSE], pch = 1, cex = 2, col = "red")
#   plot(g[nover[i], ], add = TRUE, triangles = TRUE)
#   vv <- (g$oXt %>% filter(.ob0 == nover[i]) %>% inner_join(g$tXv, ".tr0") %>% gris:::v3ToLong())$.vx0 
#   points(g$v %>% filter(.vx0 %in% vv) %>% select(x, y))
#   scan("", 1)
# }
# 
# par(op)
# }