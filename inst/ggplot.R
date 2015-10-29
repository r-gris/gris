library(rworldxtra)
library(raster)
data(countriesHigh)

w <- disaggregate(subset(countriesHigh, SOVEREIGNT == "Australia"))
ind <-c(3L, 8L, 10L, 13L, 16L, 28L, 34L, 35L, 40L, 43L, 45L, 71L)
#plot(w[ind, ])
#plot(w)
#w <- disaggregate(subset(wrld_simpl, NAME == "Australia"))
#w$id <- seq(nrow(w))
#ind <- c(2L, 8L, 10L, 12L, 14L, 22L, 25L, 32L, 35L, 36L, 40L, 63L, 74L, 95L)
#plot(w[ind, ])
#w$id[ind %in% c(8, 12, 32)] <- 8

w <- w[ind, ]
w$id <- seq(nrow(w));
w$id[w$id %in% c(2, 4, 9)] <- 2
w0 <- gUnionCascaded(w, byid = TRUE)
w <- SpatialPolygonsDataFrame(w0, w@data %>% distinct(id), match.ID = FALSE)
library(rgeos)
p0 <- SpatialPolygons(list(Polygons(list(Polygon(structure(c(146.295375451873, 147.220165888371, 145.84454011408, 
                                                        146.295375451873, -41.5171056556833, -42.3652475999849, -42.3998656385278, 
                                                        -41.5171056556833), .Dim = c(4L, 2L)))), "1")))
projection(p0) <- projection(w)

w <- gDifference(w, p0, byid = TRUE)
w <- SpatialPolygonsDataFrame(w, data.frame(dummy = seq(length(w))), match.ID = FALSE)


library(ggplot2)
gg <- fortify(w2)

library(dplyr)
gg %>% ggplot(aes(long, lat, group = id, fill = id)) + geom_polygon()
## in ggplot speak, "group" is a polygon (branch), "piece" is the number of each group within an object, 
## "order" is the order of vertices in the whole object, and "id" is the object

## so for gris
w$.ob0 <- row.names(w)

#w$gid <- row.names(w)

library(gris)
o <- as_data_frame(as.data.frame(w2)) %>% as_data_frame
b <- gg %>% distinct(group) %>% mutate(.ob0 = id, .br0 = group) %>% select(.ob0, .br0, hole) %>% as_data_frame
v <- gg  %>% mutate(.vx0 = seq(n()), .br0 = group) %>% select(.vx0, .br0, long, lat) %>% as_data_frame
bXv <- v %>% select(-long, lat) %>% as_data_frame
v <- v %>% select(long, lat, .vx0) %>% as_data_frame

gr <- gris:::gris.full(o = o, b = b, bXv = bXv, v)
obj0 <- gris:::normalizeVerts2(gr$v, gr$bXv , c("long", "lat"))
gr$v <- obj0$v %>% mutate(x = long, y = lat)
gr$bXv <- obj0$bXv
plot(gr)
srtm1 <-  getData("SRTM", lon = mean(gr$v$x), lat = mean(gr$v$y))
srtm2 <-  getData("SRTM", lon = 144, lat = mean(gr$v$y))

srtm <- merge(srtm1, srtm2)

p <- mkpslg(gr)
tr <- RTriangle::triangulate(p, a = prod(res(srtm)))
library(rgdal)
#tr$P <- project(tr$P, "+proj=laea +lon_0=147 +lat_0=-42 +ellps=WGS84")
library(rgl)
ob <- tetrahedron3d()
ob$vb <- t(cbind(project(tr$P,"+proj=laea +lon_0=147 +lat_0=-42 +ellps=WGS84") , extract(srtm, tr$P), 1))
ob$vb[is.na(ob$vb)] <- 0
ob$it <- t(tr$T)


gm <- gmap(extent(w2), type = "roadmap", scale = 2)
im <- setValues(brick(gm, gm, gm), t(col2rgb(gm@legend@colortable))[values(gm) + 1, ])
imfile <- writeGDAL(as(im, "SpatialGridDataFrame"), "pngfile.png", drivername = "PNG")
tcoords <- xyFromCell(setExtent(gm, extent(0, 1, 0, 1)), cellFromXY(gm, project(tr$P, projection(gm))))
shade3d(ob, col = "white", texcoords = tcoords[ob$it, ], texture = imfile)



