library(dplyr)
source("R/gris.R")
v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), bid__00 = 1, oid__00 = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), bid__00 = 2, oid__00 = 1)

v3 <- v1 %>% mutate(x = x + 2, bid__00 = 4, oid__00 = 2)
v4 <- v2 %>% mutate(x = x + 2, bid__00 = 5, oid__00 = 2)
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), bid__00 = 3, oid__00 = 1)

v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(id = seq(n()))


verts <- v %>% select(id, x, y)
branch <- v %>% distinct(bid__00) %>% transmute(id = bid__00)
##ts <- pl(v, debug = TRUE)

pl(v, col = c("grey", "aliceblue"))

pl(v, col = c("black", "red"), type = "l", lwd = c(3, 5))

library(rworldmap)
data(countriesLow)

load("inst/extdata/brokeCountries.Rdata")

## multi-branch polygons with holes
## point in polygon test is over the top
## triang.list is really inefficient
dpc <- dv(brokeCountries)
pl(dpc)


library(geometry)
cols <- rainbow(255, alpha = 0.5)
for (jj in unique(dpc$oid__00)) {
  ab <- filter(dpc, oid__00 == jj)  ##%>% filter(bid__00 == 1)
  ##pl(ab, col = "red")
  del <- delaunayn(cbind(ab$x, ab$y))
  ##apply(del, 1, function(x) polypath(cbind(ab$x[x], ab$y[x])))

   centr <- data_frame(x = ab$x[t(del)], y = ab$y[t(del)], t = rep(seq(nrow(del)), each = 3)) %>% group_by(t) %>% summarize(x = mean(x), y = mean(y))
   x1 <- ab %>% mutate(mg = bid__00) %>%  group_by(mg) %>% do(rbind(., NA_real_))
   inside <- which(point.in.polygon(centr$x, centr$y, x1$x, x1$y) == 1)
   col <- sample(cols, 1)
   apply(del[inside, ], 1, function(x) polypath(cbind(ab$x[x], ab$y[x]), col = col, border = NA))

#   for (i in seq_along(tril)) if (inside[i]) polypath(tril[[i]]$x, tril[[i]]$y)
}








## deldir
for (jj in unique(dpc$oid__00)) {
  ab <- filter(dpc, oid__00 == jj)  ##%>% filter(bid__00 == 1)
  ##pl(ab, col = "red")
  del <- deldir(ab$x, ab$y)
  tril <- triang.list(del)
  centr <- matrix(0, length(tril), ncol = 2)
  for (i in seq_along(tril)) {centr[i, 1] <- mean(tril[[i]]$x); centr[i, 2] <- mean(tril[[i]]$y)}
  x1 <- ab %>% mutate(mg = bid__00) %>%  group_by(mg) %>% do(rbind(., NA_real_))
  inside <- point.in.polygon(centr[,1], centr[,2], x1$x, x1$y)
  for (i in seq_along(tril)) if (inside[i]) polypath(tril[[i]]$x, tril[[i]]$y)
}


library(deldir)

system.time({
  prj <- "+proj=laea +lat_0=-90"
  xy <- project(as.matrix(dpc[,1:2]), prj)
  dpc$x <- xy[,1]
  dpc$y <- xy[,2]
  pl(dpc, asp = 1)
})

system.time(plot(spTransform(brokeCountries, CRS(prj)), col = grey(seq(0, 1, length = nrow(brokeCountries)))))

dd <- d %>% arrange(tag, date) %>% mutate(x = long, y = lat, oid = c(unclass(factor(tag))), bid = c(unclass(factor(tag))))
library(rgdal)
xy <- project(as.matrix(dd[,c("x", "y")]), "+proj=laea +lat_0=-90")
dd[, c("x", "y")] <- xy


library(trip)
load("inst/extdata/tr_2012.Rdata")
tr <- as_data_frame(as.list(as.data.frame(tr_))) %>% arrange(ptt, gmt) %>% mutate(x = longitude, y = latitude, oid = c(unclass(factor(ptt))), bid = c(unclass(factor(ptt))))

library(rgdal)
xy <- project(as.matrix(tr[,c("x", "y")]), "+proj=laea +lat_0=-90")
tr[, c("x", "y")] <- xy
pl(tr, type = "l")

data(volcano)

library(raster)
rexample <- raster(volcano)
r <- data_frame(cell = 1:length(volcano), val = as.vector(t(volcano)))
## transform
rt <- list(ext = extent(rexample), nr = nrow(rexample), nc = ncol(rexample))
plot(setValues(rexample * 0, r$val))


library(raadtools)
sst <- readsst(sort(sample(sstfiles()$date, 10)), xylim = extent(100, 150, -50, -30))
r3d <- list(data = data_frame(cell = seq(prod(dim(sst))), val = as.vector(values(sst))),
            transform =  list(ext = nextent(sst), dim = dim(sst)))
##r3d$data <- r3d$data %>% filter(!is.na(val))
pr <- function(x, layer, ...) {
  if (missing(layer)) layer <- x$transform$dim[3]
  op <- par(mfrow = n2mfrow(length(layer)))
  rtemp <- setValues(raster(extent(c(x$transform$ext[,1:2])),
                  nrow = x$transform$dim[1],
                  ncol = x$transform$dim[2]), NA_real_)

  for (i in seq_along(layer)) {
    rtemp0 <- rtemp
    cells <- seq(prod(x$transform$dim[1:2])) + (layer - 1) * prod(x$transform$dim[1:2])
    cells <- intersect(cells, x$data$cell)
    rtemp0[cells - (layer - 1) * prod(x$transform$dim[1:2])] <- x$data$val[match(cells, x$data$cell)]
    plot(rtemp0, ...)
  }
  par(op)
  NULL
}

for (i in 1:10) {pr(r3d, i, col = sst.pal(100), zlim = c(-1.8, 32));Sys.sleep(0.2)}
