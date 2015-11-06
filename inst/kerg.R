# f <- "C:\\data\\kerg\\kerg_dem_100m.grd"
# r0 <- raster(f)
# r0 <- writeRaster(r0, file.path(dirname(filename(r0)), "kerg.grd"))

library(raster)
f <- "C:\\data\\kerg\\kerg.grd"
r0 <- raster(f)
ex <- extent(73, 74, -54, -53) 

#projectExtent(raster(extent(100, 110, -68, -62), 
#                         crs = "+proj=longlat +ellps=WGS84"), 
#                  projection(r0))


# r2 <- raster(r0); res(r2) <- res(r0) * 15
# r2 <- setValues(r2, extract(r0, coordinates(r2)))

r <- crop(r0, ex, snap = "out")
#contour(r)

library(gris)
library(rgl)
m <- bgl(r, r)

blues <- colorRampPalette(c("darkblue",  "aliceblue"))

cmap <- c(blues(28), grey(seq(1, 0, length = 28)))
# brks <- c(seq(min(m$vb[3,], na.rm = TRUE), 0, length = 28), 
#           

brks <- c( quantile(m$vb[3,m$vb[3,] < 0], seq(0, 1, length = 28), na.rm = T), 
           seq(0, max(m$vb[3,], na.rm = TRUE), length = 28))

z <- m$vb[3,]
m$vb[1:3, ] <- t(llh2xyz(t(m$vb[1:3, ]), exag = 4))
rgl.close()
shade3d(m, col = cmap[findInterval(z[m$ib], brks)])
#aspect3d(1, 1, 0.5)
## aspect3d(1, 1, 0.001)


