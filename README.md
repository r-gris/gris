[![Travis-CI Build Status](https://travis-ci.org/mdsumner/gris.svg?branch=master)](https://travis-ci.org/mdsumner/gris)

# gris

A dplyr-based geometry model for R. This is in early stages and will change a lot.

=======
There's not much to see except for this out of date readme with a companion package: 

https://github.com/mdsumner/cgalgris/blob/master/README.md

What we have 
 
* basic S3 methods to build gris objects from sp and plot them. 
* examples of various workflows
* basic proj4::ptransform with dplyr piping

What we need

* need to fix how gris objects are created, since g[i, ] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
* flesh out the RTriangle support and relation to rgl texture mapping
* ensure standard triangulation is done in a local projection, no matter what the source is in
* add ability to explode and recombine objects and their component polygons arbitrarily
* coercion from gris to Spatial*
* apply system of naming for vertices - this is begun in gris.default, needs to be made only for simple case of points
* ~convert raster to quad mesh~
* ~~triangulation with CGAL, constrained Delaunay~~
* ~~speed up the normalization of vertices~~

Problems

* speed up of normalization leaves a problem with gris(countriesLow) but gris(wrld_simpl) is fine, I think it comes down to duplicated versus numeric equality on the sort

```{r}
pp <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))), 
        Polygon(cbind(c(0, 1, 0.5, 0), c(1, 1, 1.5, 1)))), "1"))), data.frame(x = 1))
        

pp <- SpatialPolygonsDataFrame(SpatialPolygons(list(
Polygons(list(Polygon(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))), 
        Polygon(cbind(c(0, 1, 0.5, 0), c(1, 1, 1.5, 1))), 
        Polygon(cbind(c(0.2, 0.6, 0.6, 0.2, 0.2), c(0.2, 0.2, 0.6, 0.6, 0.2)))), "1"))
        ), data.frame(x = 1))
        
        
        
```



