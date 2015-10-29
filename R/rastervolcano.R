#library(raster)

#r <- raster(volcano)

#library(dismo)
#g <- gmap(rep(rev(c(-36.877, 174.764)), each  = 2)  + c(-1, 1) * 0.004, type = "satellite")

#r <- raster("D:\\data\\topography\\NZ\\kx-tile-00000001tif-GTiff\\00000001.tif")

#rvolcano <- crop(r, extent(projectExtent(g, projection(r))))

#library(gris)
#vol <- bgl(rvolcano, rvolcano)
#library(rgdal)
#tcoords <- xyFromCell(setExtent(g, extent(0, 1, 0, 1)), cellFromXY(g, 
#                                                                   project(		project(t(vol$vb[1:2, ]), projection(rvolcano), inv = TRUE), projection(g))))



#cols <- t(col2rgb( g@legend@colortable))

#imgname <- writeGDAL(as(setValues(brick(g, g, g), t(cols)[values(g) + 1, ]), "SpatialGridDataFrame"), "tex.png", drivername = "PNG")

#library(rgl)
#shade3d(vol, col = "white", texcoords = tcoords[vol$ib, ], texture = imgname)

