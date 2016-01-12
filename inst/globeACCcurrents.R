blues <- local( {
  f <- colorRampPalette(rev(c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b')))
 
  function(x) f(x)
})


   
library(gris)
library(rgl)


 a <- subset(currentsfiles(), date > as.POSIXct("2015-01-10"))

x <- readcurr(a$date[1], xylim = extent(-180, 180, -90, 0))
v <- function(x) sqrt(x[[1]] ^2 + x[[2]]^2)
b <- bgl(v(x), v(x))
vs <- sqrt(b$vb[3,])
b$vb[1:3, ] <- t(llh2xyz(cbind(t(b$vb[1:2,]), 0)))

scl <- function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
#vs[is.na(vs)] <- 0
shade3d(b, col = blues(100)[scl(vs[b$ib]) * 99 + 1])


p <- projectExtent(x, "+proj=ortho +ellps=WGS84 +lat_0=-90 +lon_0=70")
cell <- cellFromXY(x, project(coordinates(p), projection(p), inv = TRUE))
p1 <- setValues(p, extract(sqrt(x[[1]]^2 + x[[2]]^2), cell))


