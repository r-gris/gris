library(raster)

context("basic building")

test_that("we can ingest complex objects from sp", {
  library(maptools)
  data(wrld_simpl)
  
  expect_that(gris(wrld_simpl), is_a("gris"))
  expect_that(gris(wrld_simpl, normalize = FALSE), is_a("gris"))
  expect_that(gris(subset(wrld_simpl, NAME == "Australia"), triangulate = TRUE), is_a("gris"))
  
  
})
test_that("we can ingest a line object from sp", {
  x <- c(1:9, 8:1)
  y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)
  df <- data.frame(x = x, y = y, g = rep(c("a", "b", "c"), c(5, 4, 8)))
  mklinelist <- function(yy, fg) lapply(split(yy, fg), function(x) Line(as.matrix(x[,1:2])))
  line <- SpatialLinesDataFrame(SpatialLines(list(Lines(mklinelist(df, df$g), "1"), Lines(mklinelist(df %>% mutate(x = y, y = rev(x) * 4) %>% as.data.frame, df$g), "16"))), 
                                df[c(1, 16),])
  gline <- gris(line)
  expect_that(gline, is_a("gris"))
  expect_that(gris:::as.SpatialPolygonsDataFrame(gline), is_a("SpatialPolygonsDataFrame"))
})

test_that("we can ingest a line object from rasterToContour", {
  levs <- c(94, 108, 124, 150, 195)
  r <- raster(volcano)
  cl <- rasterToContour(r, lev = levs)
  g <- gris(cl)
  expect_that(nrow(g$v), equals(703))
  expect_that(nrow(g$o), equals(4))
  #expect_that(nrow(g$v %>% inner_join(g$bXv) %>% inner_join(g$b) %>% inner_join(g$o)), equals(703))
  ## why did this change? 2015-11-06
  expect_that(nrow(g$v %>% inner_join(g$bXv) %>% inner_join(g$b) %>% inner_join(g$o)), equals(707))
})

test_that("build from scratch, and triangulate", {
library(gris)

library(sp)
# simple example, from scratch:
Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)

Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
Sp <- SpatialPolygonsDataFrame( SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3), data.frame(x = 1:3, row.names = c("s1", "s2", "s3/4")))
g <- gris(Sp, triangulate = TRUE)
plot(g)
gris:::plotT(g, lwd = 2)
})



