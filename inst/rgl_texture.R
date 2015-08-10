Hello, I've been using the texture mapping in rgl quite a lot with great results, thank you for your previous help with shade3d. 

I'd like to ask about the possibility of being able to pass in a texture image as an object rather than a file
- this would open up a lot more flexibility for passing in imagery, especially via the raster and rgdal packages. Presumably 
the efficiencies available for texture mapping are somewhat tied to the PNG format read in rgl, but I wonder if it's a good
idea to try to extend that? Ideally I would like to pass in a matrix of colours, without the need for a file.  I deal with a lot of 
imagery in either palette formats or dynamically created colour mappings within R, from actual data - the ability to use texture 
mapping is extremely powerful for transforming imagery between coordinates systems without destructuve resampling, but currently 
I have to create an actual PNG file, and often I have to create a full RGB data set from arbitrary numeric data. This is fine but 
obviously the file management becomes and issue when we start to use this much more. 


Below I have a full example that downloads elevation data and satellite imagery for the Grand Canyon, 


## quad index template
p4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}
## offset pairs from a vector
prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}
## pixel corners from a raster
edgesXY <- function(x) {
  coordinates(shift(
    extend(x, 
           extent(xmin(x), xmax(x) + res(x)[1], ymin(x), ymax(x) + res(x)[2])), 
    x = -res(x)[1]/2, y = -res(x)[2]/2))
}
## build a quad mesh from a raster
bgl <- function(x, z = NULL, na.rm = FALSE) {
  x <- x[[1]]  ## just the oneth raster for now
  ##exy <- as.matrix(expand.grid(edges(x), edges(x, "y")))
  exy <- edgesXY(x)
  ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) + 
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
 ## need to consider normalizing vertices here
  if (na.rm) {
    ind1 <- matrix(ind0, nrow = 4)
    ind0 <- ind1[,!is.na(values(x))]
  }
  ## dummy object from rgl
  ob <- rgl::oh3d()
  if (!is.null(z)) z <- extract(z, exy, method = "bilinear") else z <- 0
  ob$vb <- t(cbind(exy, z, 1))
  ob$ib <- matrix(ind0, nrow = 4)
  ob
}



library(raster)
library(dismo)
library(rgdal)
library(rgl)

ll <- c(-112.1, 36.1)

## 41 Mb
## download SRTM elevation data (something's wrong with raster::getData)
##srtm <- getData("SRTM", lon =  ll[1], lat = ll[2])
f <- "ftp://xftp.jrc.it/pub/srtmV4/tiff/srtm_14_05.zip"
tif <- gsub("zip$", "tif", basename(f))
if (!file.exists(basename(tif))) {
  if (!file.exists(basename(f))) download.file(f, basename(f), mode = "wb")
  unzip(basename(f))
}
srtm <- raster("srtm_14_05.tif")
srtm <- crop(srtm, extent(ll[1] + c(-1, 1) * 0.5, ll[2] + c(-1, 1) * 0.7))

## build mesh3d object 
## we are plotting in long/lat so rescale heights 
ro <- bgl(srtm, z = srtm/30000)

# 0.7Mb
## download a google satellite image with dismo
gm <- gmap(x = srtm, type = "satellite", scale = 2)

# we need RGB expanded (gmap gives a palette)
rgb1 <- col2rgb(gm@legend@colortable)
img <- brick(gm, gm, gm)
cells <- values(gm) + 1
img <- setValues(img, cbind(rgb1[1, cells], rgb1[2, cells], rgb1[3, cells]))

## finally, create RGB PNG image to act as a texture image
writeGDAL(as(img, "SpatialGridDataFrame"), "gm.png", drivername = "PNG", type = "Byte", mvFlag = 255)

## project our mesh to the image and get it in [0,1,0,1] of texture space
tcoords <- xyFromCell(setExtent(gm, extent(0, 1, 0, 1)), cellFromXY(gm, project(t(ro$vb[1:2, ]), projection(gm))))

shade3d(ro, col = "white", texture = "gm.png", texcoords = tcoords[ro$ib, ])
