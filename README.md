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

* integration of primitive model for topology
* need to fix how gris objects are created, since g[i, ] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
* flesh out the RTriangle support and relation to rgl texture mapping
* ensure standard triangulation is done in a local projection, no matter what the source is in
* add ability to explode and recombine objects and their component polygons arbitrarily
* coercion from gris to Spatial*
* apply system of naming for vertices - this is begun in gris.default, needs to be made only for simple case of points
* ~convert raster to quad mesh~
* ~~triangulation with CGAL, constrained Delaunay~~
* ~~speed up the normalization of vertices~~

Topology

Currently in gris, traditional GIS-like objects are decomposed into a set of relational tables. These are Vertices, Branches, and Objects. Storing these data in tables is similar to the ggplot2 model where a GIS object is converted to a single table with *fortify*, but the problem with fortify is that it simply duplicates object and branch information by copying them onto every vertex. This is straightforward in on sense, but wasteful and does not provide an obvious logical structure for GIS objects. In *gris* the Objects table is analogous to the "attributes" table in a GIS, each row stores data on an object which might be a polygon, a line, or a point (and each may be multi-versions thereof). The Branches table allows for multiple vertices on a a single point, multiple connected sets of line segments (linear strings) for a single line, and multiple "rings" for a complex polygon that may contain "holes" or include multiple separate "islands". 

This model is fine for storing basic traditional geometry including multipoints and Z and/or Time (and more) attributes on vertices, but is insufficient for tetrahedral surfaces and higher dimensional topologies. There are three steps in converting a traditional polygon into a surface: 1) decompose the polygon into a set of vertices with a two-element index specifying every boundary segment, a planar-straight-line-graph (the vertices must also be "normalized" to remove any duplicates) 2) triangulate with Triangle's constrained Delaunay algorithm (optionally with a maximum triangle area constraint) 3) calculate centroids of every triangle and filter those that compose a "hole" in the original complex polygon. 

Gris currenty provides a function to convert the Branches topology to a planar-straight-line-graph (function mkpslg), but it would be preferable to add this topology model to the object with a "triangulate" method. This could be a replacement of the Branches structure with a Topology structure that could hold one or more of Branches, Primitives and possibly other models. The Triangle library provides all the required algorithms for constrained triangulation with area constraints as well, but we lack a simple conversion from and to the Branches model for visualization and conversion back to traditional forms. 

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



