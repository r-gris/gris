
library(gris)
library(rgl)

library(raster)
r0 <- raster("C:\\data\\ibcso\\ibcso.grd")

lat <- -70

lonstep <- seq(180, 0, by = -10)

lon <- 103
lat <- -65
 ex <- extent(lon - 5, lon + 5, lat - 2, lat + 2) 
  r <- crop(r0, ex, snap = "out")
  m <- bgl(r, r)
  
  blues <- colorRampPalette(c("darkblue",  "aliceblue"))
  
  cmap <- c(blues(28), grey(seq(1, 0, length = 28)))
  # brks <- c(seq(min(m$vb[3,], na.rm = TRUE), 0, length = 28), 
  #           
  
  brks <- c( quantile(m$vb[3,m$vb[3,] < 0], seq(0, 1, length = 28), na.rm = T), 
             seq(0, max(m$vb[3,], na.rm = TRUE), length = 28))
  
  z <- m$vb[3,]
#  z[is.na(z)] <- 0
  m$vb[1:3, ] <- t(llh2xyz(t(m$vb[1:3, ]), exag = 20))
  
  # m$vb[3, is.nan(m$vb[3,])] <- 0
  # m$vb[2,is.nan(m$vb[2,])] <- min(na.omit(m$vb[2, ]))
  # m$vb[1,is.nan(m$vb[1,])] <- min(na.omit(m$vb[1, ]))
  # 
  # 
  shade3d(m, col = cmap[findInterval(z[m$ib], brks)])

