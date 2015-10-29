v <- structure(list(x = c(1932.8172823219, 1933.46635883905, 2162.42810026385, 
                          2161.94129287599, 1962.02572559367, 1954.07453825858, 1962.35026385224, 
                          1988.80013192612, 1996.42678100264, 1986.85290237467, 1982.95844327177, 
                          1971.11279683377, 1961.86345646438, 1971.11279683377, 1982.47163588391, 
                          1988.31332453826, 1996.75131926121, 1987.98878627968, 2008.92150395778, 
                          2009.24604221636, 2017.52176781003, 2016.87269129288, 2046.56794195251, 
                          2077.39907651715, 2077.39907651715, 2085.83707124011, 2085.51253298153, 
                          2075.93865435356, 2047.37928759894, 2016.5481530343, 2045.91886543536, 
                          2046.08113456464, 2047.86609498681, 2047.70382585752, 2098.00725593667, 
                          2097.84498680739, 2106.44525065963, 2132.89511873351, 2140.8463060686, 
                          2140.03496042216, 2131.92150395778, 2132.73284960422, 2127.86477572559, 
                          2110.33970976253, 2106.12071240106, 2106.93205804749), y = c(989.230211081794, 
                                                                                       870.286939313984, 861.362137203166, 997.181398416887, 971.380606860158, 
                                                                                       930.001978891821, 886.838390501319, 886.676121372032, 904.201187335092, 
                                                                                       904.038918205805, 896.25, 895.925461741425, 930.326517150396, 
                                                                                       963.591688654354, 963.429419525066, 954.991424802111, 954.666886543536, 
                                                                                       972.191952506596, 971.705145118734, 887.325197889182, 887.000659630607, 
                                                                                       954.991424802111, 887.325197889182, 954.666886543536, 887.48746701847, 
                                                                                       887.48746701847, 972.354221635884, 972.354221635884, 913.12598944591, 
                                                                                       971.867414248021, 972.841029023747, 971.056068601583, 970.893799472296, 
                                                                                       973.003298153034, 971.705145118734, 895.438654353562, 886.838390501319, 
                                                                                       886.676121372032, 895.60092348285, 973.003298153034, 972.354221635884, 
                                                                                       898.846306068602, 895.438654353562, 895.763192612137, 899.982189973615, 
                                                                                       972.191952506596), .vx0 = 1:46), .Names = c("x", "y", ".vx0"), row.names = c(NA, 
                                                                                                                                                                    -46L), class = c("tbl_df", "tbl", "data.frame"))
bXv <- structure(list(.br0 = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 
                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 
                               3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 
                               5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L), .vx0 = c(1L, 2L, 3L, 4L, 
                                                                             1L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 
                                                                             18L, 5L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 
                                                                             30L, 19L, 31L, 32L, 33L, 34L, 31L, 35L, 36L, 37L, 38L, 39L, 40L, 
                                                                             41L, 42L, 43L, 44L, 45L, 46L, 35L)), .Names = c(".br0", ".vx0"
                                                                             ), row.names = c(NA, -51L), class = c("tbl_df", "tbl", "data.frame"
                                                                             ))


b <- structure(list(.br0 = 1:5, .ob0 = 1:5), .Names = c(".br0", ".ob0"
), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"
))

o <- structure(list(ID = 6:10, .ob0 = 1:5), .Names = c("ID", ".ob0"
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                           -5L))

cmu <- list(Vertices = v,  Branches = list(b = b, BxV = bXv), Primitives = NULL, Objects = o)

x <- list(v = v, bXv = bXv, b = b, o = o)
class(x) <- c("gris", "list")

p <- mkpslg(x)
tr <- RTriangle::triangulate(p, a = 100)
centroids <- cbind(.rowMeans(matrix(tr$P[tr$T, 1], nrow = nrow(tr$T)), nrow(tr$T), 3), 
                   .rowMeans(matrix(tr$P[tr$T, 2], nrow = nrow(tr$T)), nrow(tr$T), 3))

oid <- over(SpatialPoints(centroids), gris:::grisToSpatialPolygons((x)))
pt <- list(Vertices = data_frame(x = tr$P[,1], y = tr$P[,2], z = 0, .vx0 = seq(nrow(tr$P))), 
           Primitives = data_frame(.vx0 = as.vector(t(tr$T)), .px0 = rep(seq(nrow(tr$T)), each = ncol(tr$T))), 
           P = data_frame(.ob0 = oid, .px0 = seq(nrow(tr$T))), 
           Objects = x$o)

rgl.triangles((pt$Vertices %>% dplyr::select(x, y, z) %>% as.matrix)[pt$Primitives$.vx0, ], col = "grey")

##              rep((pt$Vertices[pt$Primitives$.vx0, ] %>% inner_join(pt$Primitives) %>% inner_join(pt$P))$.ob0[seq(1, 8395, by = 6)], each = 3))










rep(sample(grey(seq(0, 1, length = max(pt$Primitives$.vx0)))), each = 3))



.Vertices <- function(x) x$Vertices
.vID <- function(x) .Vertices(x)$.vx0

pslgGris <- 
function(x) {
  ## remap vertices
  remap <- seq(nrow(x$Vertices))
  .vx0 <- remap[match(x$Branches$BxV$.vx0, .vID(x))]
  RTriangle::pslg(
    P = x$Vertices %>% dplyr::select(x, y) %>% as.matrix(),
    S = do.call(rbind, lapply(split(
      .vx0, x$Branches$BxV$.br0
    ), gris:::prs1))
  )
  
}


triangulateG <- function(x, maxarea = 100) {
  p <- pslgGris(x)
  # p$PA <- matrix(as.numeric(seq(nrow(p$P))))
  tr <- RTriangle::triangulate(p, a = maxarea)
  ## rebuild the entire object
  Vertices <- data_frame(.vx0 = seq(nrow(tr$P)), x = tr$P[,1], y = tr$P[,2])
  PxV <- data_frame(.vx0 = seq(nrow(tr$P))[as.vector(t(tr$T))], .px0 = rep(seq(nrow(tr$T)), each = ncol(tr$T)))
  P <- data_frame(.px0 = seq(nrow(tr$T)), .ob0 = rep(NA_real_, nrow(tr$T)))
  for (i in seq(nrow(x$Objects))) {
    
  }
}





triangulateGris <- function(x, maxarea = NULL) {
  p <- pslgGris(x)
 # p$PA <- matrix(as.numeric(seq(nrow(p$P))))
  t <- RTriangle::triangulate(p, a = maxarea)
  x$Primitives <- list(PxV = data_frame(.vx0 = as.vector(t(t$T)), .px0 = rep(seq(nrow(t$T)), each = ncol(t$T))), 
                       P = data_frame(.px0 = seq(nrow(t$T))))
  
}
plotP <- function(x) {
  #plot(x$Vertices %>% dplyr::select(x, y), type = "n")
  rgl.triangles((x$Vertices %>% dplyr::select(x, y, z) %>% as.matrix)[x$Primitives$PxV$.vx0, ], col = rep(sample(grey(seq(0, 1, length = max(x$Primitives$PxV$.vx0)))), each = 3))
}
