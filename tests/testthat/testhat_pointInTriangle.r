test_that("point in triangle works", {
  library(maptools)
  data(wrld_simpl)
  g <- gris(wrld_simpl, triangulate = TRUE)
  xy <- coordinates(wrld_simpl)
  pit <- pointInTriangle(g, xy)
  spoints <- SpatialPoints(coordinates(wrld_simpl), proj4string = CRS(proj4string(wrld_simpl)))
                              
  opoints <- over(spoints, wrld_simpl)      
  nover <- which(is.na(opoints[,1]))
  ## only Cuba and Haiti don't have their centroid inside a piece (!)
  ## though others identify the wrong piece :)
  expect_that(length(na.omit(pit)), equals(nrow(xy) - 2) )
  expect_that(levels(g$o$NAME)[g$o$NAME[is.na(pit)]], equals(c("Cuba", "Haiti")))
  
  ## six are duplicated as five don't match the parent, and two of these are NA
  expect_that( sum(duplicated(opoints$FIPS)), equals(6L))
  ## two NAs (only Cuba and Haiti have orphaned centroids)
  expect_that(sum(is.na(opoints$FIPS)), equals(2L))
  
})

# op <- par(mfrow = n2mfrow(length(nover)), mar = rep(0, 4))
# #par(mar = rep(0, 4))
# for (i in seq_along(nover)) {
#   plot(wrld_simpl[nover[i], ], col = "grey")
# 
#   abline(v = coordinates(spoints)[nover[i],1], h = coordinates(spoints)[nover[i], 2])
#   points(xy[nover[i], , drop = FALSE], pch = 1, cex = 2, col = "firebrick")
#   plot(g[nover[i], ], add = TRUE, triangles = TRUE)
#   vv <- (g$oXt %>% filter(.ob0 == nover[i]) %>% inner_join(g$tXv, ".tr0") %>% gris:::v3ToLong())$.vx0
#   points(g$v %>% filter(.vx0 %in% vv) %>% select(x, y))
#  
# }
# 
# par(op)
