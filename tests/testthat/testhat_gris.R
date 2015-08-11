library(raster)

context("basic building")

## from ?graphics::polygon
x <- c(1:9, 8:1)
y <- c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9)

df <- data.frame(x = x, y = y, g = rep(c("a", "b", "c"), c(5, 4, 8)))
mklinelist <- function(yy, fg) lapply(split(yy, fg), function(x) Line(as.matrix(x[,1:2])))

line <- SpatialLinesDataFrame(SpatialLines(list(Lines(mklinelist(df, df$g), "1"), Lines(mklinelist(df %>% mutate(x = y, y = rev(x) * 4) %>% as.data.frame, df$g), "16"))), 
                              df[c(1, 16),])

gline <- gris(line)
#test_that("objects built from basic building blocks", {
 #
#})


test_that("we can ingest a line object", {
  levs <- c(94, 108, 124, 150, 195)
  r <- raster(volcano)
  cl <- rasterToContour(r, lev = levs)
  g <- gris(cl)
  expect_that(nrow(g$v), equals(703))
  expect_that(nrow(g$o), equals(4))
  expect_that(nrow(g$v %>% inner_join(g$bXv) %>% inner_join(g$b) %>% inner_join(g$o)), equals(703))
})

test_that("we can convert to Spatial", {
  spline <- 
})