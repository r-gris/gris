# test_that("we can ingest multipoints", {
#   library(sp)
#   cl1 = cbind(rnorm(3, 10), rnorm(3, 10))
#   cl2 = cbind(rnorm(5, 10), rnorm(5,  0))
#   cl3 = cbind(rnorm(7,  0), rnorm(7, 10))
#   mpdf = SpatialMultiPointsDataFrame(list(cl1, cl2, cl3), data.frame(a = 1:3))
#   gg <- gris(mpdf)
#   expect_that(nrow(gg$v), equals(sum(c(nrow(cl1), nrow(cl2), nrow(cl3)))))
#   expect_that(nrow(gg$b), equals(3L))
# })

