# library(testthat)
# 
# context("geocentric")
# 
# oldfun  <- function(lonlatheight, rad = 6378137.0, exag = 1) {
#   cosLat = cos(lonlatheight[,2] * pi / 180.0)
#   sinLat = sin(lonlatheight[,2] * pi / 180.0)
#   cosLon = cos(lonlatheight[,1] * pi / 180.0)
#   sinLon = sin(lonlatheight[,1] * pi / 180.0)
# 
#   rad <- (exag * lonlatheight[,3] + rad)
#   x = rad * cosLat * cosLon
#   y = rad * cosLat * sinLon
#   z = rad * sinLat
# 
#   cbind(x, y, z)
# }
# 
# geocent_proj <- "+proj=geocent +ellps=sphere +a=6378137"
# x <- geosphere::randomCoordinates(1000)
# y1 <- oldfun(cbind(x, 0))
# 
# y <- proj4::ptransform(x * pi/180, 
#                        src.proj = "+proj=longlat +ellps=sphere",  
#                        dst.proj = geocent_proj)
# 
# test_that("geocent replaces old method", {
#   expect_equal(2 * 2, 4)
# })
