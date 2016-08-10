
##library(rgl)
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

context("test 3d")
x0 <- gris(Sp)
x1 <- triangulate(x0)
test_that("we can triangulate", {
  expect_that(x0, is_a("gris"))
  expect_that(x1, is_a("gris"))
  expect_that(x1$tXv, is_a("tbl"))
  expect_that(x1$oXt, is_a("tbl"))
  
  expect_that(x2 <- triangulate(gris(Sp)), is_a("gris"))
  expect_that(x2$tXv, is_a("tbl"))
  expect_that(x2$oXt, is_a("tbl"))
  
  expect_that(all(c(".vx1", ".vx2", ".vx3", ".tr0") %in% names(x2$tXv)), is_true())
  
  expect_that(all(c("object_", ".tr0") %in% names(x2$oXt)), is_true())
  
})


library(rgl)
test_that("we can plot without error", {
  expect_silent(gris::plot3d(x1, globe = FALSE))
})
  