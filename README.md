# gris

A dplyr-based geometry model for R. This is in early stages and will change a lot.

=======
There's not much to see except for this out of date readme with a companion package: 

https://github.com/mdsumner/cgalgris/blob/master/README.md

What we have 
 
* basic S3 methods to build gris objects from sp and plot them. 
* examples of various workflows


What we need

* flesh out the RTriangle support and relation to rgl texture mapping
* consider edge model as default rather than branches
* coercion from gris to Spatial*
* convert raster to quad mesh
* apply system of naming for vertices . . .
* ~~triangulation with CGAL, constrained Delaunay~~
* ~~speed up the normalization of vertices~~

Problems

* speed up of normalization leaves a problem with gris(countriesLow) but gris(wrld_simpl) is fine, I think it comes down to duplicated versus numeric equality on the sort
* ~~problem with countriesLow: e.g. gris(countriesLow) gives a triangulation that is convex on all polygons, while wrld_simpl is fine~~
* ~~general slowness of gris normalization~~


```{r}
pp <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))), 
        Polygon(cbind(c(0, 1, 0.5, 0), c(1, 1, 1.5, 1)))), "1"))), data.frame(x = 1))
        

pp <- SpatialPolygonsDataFrame(SpatialPolygons(list(
Polygons(list(Polygon(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))), 
        Polygon(cbind(c(0, 1, 0.5, 0), c(1, 1, 1.5, 1))), 
        Polygon(cbind(c(0.2, 0.6, 0.6, 0.2, 0.2), c(0.2, 0.2, 0.6, 0.6, 0.2)))), "1"))
        ), data.frame(x = 1))
        
        
        
```



