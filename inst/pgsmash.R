library(gris)  ## devtools::install_github("mdsumner/gris")
library(raster)
## biggish shapefile
#sh <- shapefile("D:\\data\\AAD\\AntarcticCoast\\cst00_polygon.shp")
sh <- shapefile("D:\\data\\AAD\\1mill\\cst01_polygon.shp")

## convert to gris format
gh <- gris(sh)

## for triangulation
library(RTriangle)

## for area calcs / intersections
library(rgeos)

## triangulation - from this we will create simple polygons
ps <- mkpslg(gh)

tr <- RTriangle::triangulate(ps)

rawpolys <- lapply(split(t(tr$T)[], rep(seq(nrow(tr$T)), each = 3)), function(ind) Polygon(tr$P[ind, ]))
#polys <- SpatialPolygons(Polygons(rawpolys, "1"), proj4string = CRS(projection(sh)))
polys <- spPolygons(rawpolys[1:1000], crs = projection(sh))

## big-a grid

r0 <- raster(extent(sh), crs = projection(sh))
 res(r0) <- 5e3
# x <- over(SpatialPoints(coordinates(r0), proj4string = CRS(projection(sh))), sh)
# values(r0) <- is.na(x$gid) * 1
b <- bgl(r0)
sh0 <- as(sh, "SpatialPolygons")
ids <- 579015
areas <- numeric(length(ids))
p0 <- SpatialPolygons(list(Polygons(list(Polygon(t(b$vb[1:2, b$ib[,i]]))), "1")), proj4string = CRS(projection(r0)))
ind <- c(1, 2, 3, 4, 1)
vb <- t(b$vb[1:2, ])
ib <- t(b$ib)

for (i in seq(ncell(r0))) {
  p0@polygons[[1]]@Polygons[[1]]@coords <- vb[ib[i, ], ][ind, ]
  x <- unlist(gBinarySTRtreeQuery(p0, sh0))
  #p0 <- SpatialPolygons(list(Polygons(list(Polygon(t(b$vb[1:2, b$ib[,i]]))), "1")), proj4string = CRS(projection(r0)))
 # if (!is.na(over(p0, sh0))) {
  if (any(!is.null(x))) {
    result <- try(gArea(raster::intersect(p0, sh0)))
    if (!inherits(result, "try-error")) areas[i] <- result
  }
  if (i %% 1e4 == 0) print(i)
  
}

library(gris)
library(raster)
g <- bgl(raster(volcano))



p0 <- SpatialPolygons(list(Polygons(list(Polygon(t(g$vb[1:2,g$ib[,1]]))), "1")))




library(rbenchmark)
library(maptools)
benchmark(create = SpatialPolygons(list(Polygons(list(Polygon(t(g$vb[1:2,g$ib[,1]]))), "1"))), 
          elide = elide(p0, shift = c(5, 5)), 
          shift =  local({p0@polygons[[1]]@Polygons[[1]]@coords[,1] <- p0@polygons[[1]]@Polygons[[1]]@coords[,1] + 5; p0@polygons[[1]]@Polygons[[1]]@coords[,2] <- p0@polygons[[1]]@Polygons[[1]]@coords[,2] + 5; p0})
          , 
          replications = 1000
          
)


test replications elapsed relative user.self sys.self user.child sys.child
1 create         1000    0.65    7.222      0.61        0         NA        NA
2  elide         1000    0.71    7.889      0.70        0         NA        NA
3  shift         1000    0.09    1.000      0.09        0         NA        NA
> 
  
