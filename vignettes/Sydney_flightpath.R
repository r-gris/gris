library(gris) ## devtools::install_github("mdsumner/gris")
    

## does this function already exist?
coltable2RGB <- function(x) {
  rgb1 <- col2rgb(x@legend@colortable)
  img <- brick(x, x, x)
  cells <- values(x) + 1
  setValues(img, cbind(rgb1[1, cells], rgb1[2, cells], rgb1[3, cells]))
}

mercproj <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
llproj <- "+proj=longlat +ellps=WGS84"
##mproj <- "+proj=laea +lon_0=150 +lat_0=-32"

library(raster)
library(ncdf)
library(dismo)
library(rgdal)
e1 <- "C:\\data\\Etopo1\\ETOPO1_Ice_c_gdal.grd"
etopo <- raster(e1)
ext <- extent(16742481, 16954921, -4026452, -3818439) + 5e5
extll <- extent(projectExtent(raster(ext, crs = mercproj), llproj))

etopo <- crop(etopo, extll)
plot(etopo)

gtopo <- bgl(etopo, etopo * 5)
demxy <- t(gtopo$vb[1:2,])
##gtopo$vb[1:3,] <- t(cbind(project(demxy, mproj), gtopo$vb[3,] * 5))
gtopo$vb[1:3,] <- t(llh2xyz(t(gtopo$vb[1:3, ])))
## raster version
rimg <- gmap(extll, type = "satellite", scale = 2)

img <- coltable2RGB(rimg)

teximage <- "animage.png"
writeGDAL(as(img, "SpatialGridDataFrame"), teximage, drivername = "PNG")

## project our mesh to the image and get it in [0,1,0,1] of texture space
tcoords <- xyFromCell(setExtent(rimg, extent(0, 1, 0, 1)), 
                      cellFromXY(rimg, project(demxy, projection(rimg))))


## get locations
syd <- geocode("1 George Street, Sydney")[1,]
pm <- geocode("1 Hunter Street, Newcastle, Australia")
path1 <- as.matrix(rbind(syd, pm)[, c("longitude", "latitude")])
library(geosphere)
gcp <- gcIntermediate(path1[1,], path1[2,], n = 200)
height <- 35000
sclsin <- function(x) {
  sin(x/(max(x) / (pi/2))) * max(x)
}

gcp <- cbind(gcp, sclsin(approxfun(c(1, nrow(gcp)/2, nrow(gcp)), c(0, height, 0))(seq(nrow(gcp)))))
## turn this into segments
ind <- as.vector(t(head(matrix(seq(nrow(gcp)), ncol = 2, nrow = nrow(gcp) + 1), -2)))
##path2 <- cbind(project(gcp[ind, 1:2], mproj), gcp[ind, 3])
path2 <- llh2xyz(gcp[ind, ])
shade3d(gtopo, col = "white", texcoords = tcoords[gtopo$ib, ], texture = teximage, lit = FALSE)
rgl.lines(path2, col = "white")
rgl.bg(color = "black")
