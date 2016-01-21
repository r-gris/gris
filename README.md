[![Travis-CI Build Status](https://travis-ci.org/mdsumner/gris.svg?branch=master)](https://travis-ci.org/mdsumner/gris)

# gris

A dplyr-based geometry model for R. This is in early stages and will change a lot.


=======
There's not much to see except for this blog post on building rgl visualizations via gris: 

http://mdsumner.github.io/2015/12/28/gis3d.html

Also this out of date readme with a companion package: 

https://github.com/mdsumner/cgalgris/blob/master/README.md


# Story plan

- ingestion of Spatial structures
- ability to feed to rgl
- need for RTriangle
- comparative limitations of sp, difficulty in "normalizing vertices", no XYZ possible, 
- parallel Topology model, Primitives and Branches

Related Manifold resources: 

http://www.georeference.org/forum/t87852.23  Inflection point index generator

http://www.georeference.org/forum/t87791.23 Creating a tapered buffer

http://www.georeference.org/forum/t128563.6  Removing duplicates

http://www.georeference.org/forum/t99723.2 Deconstructing areas with multiple branches

http://www.georeference.org/forum/t122710.18

http://www.georeference.org/forum/t108653.30

http://www.georeference.org/forum/t94378r94829

http://www.georeference.org/forum/t70597.61#91661

http://staff.acecrc.org.au/~mdsumner/Eonfusion/Eonfusion_V2.0_Rasters.mp4

# What we have 
 
* basic S3 methods to build gris objects from sp and plot them. 
* examples of various workflows
* triangulated Primitives model (triGris and plotT internal functions)
* coercion from gris to Spatial (need more on cycle-extraction for triangles)
* on hold: ~basic proj4::ptransform with dplyr piping~

What we need

* integration of primitive model for topology - this is working, but we don't record the branch each triangle belongs too - this is possible but could become meaningless if we start manipulating the mesh of triangles
* need to fix how gris objects are created, since g[i, ] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
* flesh out the RTriangle support and relation to rgl texture mapping
* add option where standard triangulation is done in a local projection, no matter what the source is in
* add ability to explode and recombine objects and their component polygons arbitrarily
* apply system of naming for vertices - this is begun in gris.default, needs to be made only for simple case of points
* ~convert raster to quad mesh~
* ~~triangulation with CGAL, constrained Delaunay~~
* ~~speed up the normalization of vertices~~

Motivation

http://gis.stackexchange.com/questions/168584/representation-of-spatial-lines-with-altitude/168618#168618


Topology

Currently in gris, traditional GIS-like objects are decomposed into a set of relational tables. These are Vertices, Branches, and Objects. Storing these data in tables is similar to the ggplot2 model where a GIS object is converted to a single table with *fortify*, but the problem with fortify is that it simply duplicates object and branch information by copying them onto every vertex. This is straightforward in one sense, but is wasteful and does not provide an obvious logical structure for GIS objects. In *gris* the Objects table is analogous to the "attributes" table in a GIS, each row stores data on an object which might be a polygon, a line, or a point (and each may be multi-versions thereof). The Branches table allows for multiple vertices on a a single point, multiple connected sets of line segments (linear strings) for a single line, and multiple "rings" for a complex polygon that may contain "holes" or include multiple separate "islands". 

This model is fine for storing basic traditional geometry including multipoints and Z and/or Time (and more) attributes on vertices, but is insufficient for tetrahedral surfaces and higher dimensional topologies. There are three steps in converting a traditional polygon into a surface: 1) decompose the polygon into a set of vertices with a two-element index specifying every boundary segment, a planar-straight-line-graph (the vertices must also be "normalized" to remove any duplicates) 2) triangulate with Triangle's** constrained Delaunay algorithm (optionally with a maximum triangle area constraint) 3) calculate centroids of every triangle and filter those that compose a "hole" in the original complex polygon. 


Gris currenty provides a function to convert the Branches topology to a planar-straight-line-graph (function mkpslg), but it would be preferable to add this topology model to the object with a "triangulate" method. This could be a replacement of the Branches structure with a Topology structure that could hold one or more of Branches, Primitives and possibly other models. The Triangle library provides all the required algorithms for constrained triangulation with area constraints as well, but we lack a simple conversion from and to the Branches model for visualization and conversion back to traditional forms. 

As far as I know, the only way to keep the relation between objects and primitives is to perform the triangulation on an object-by-object basis. This complicates the approach since the Triangle vertex index is *structural* rather than relational, but it might work with the following: 

1) assume that the addition of Steiner points to the triangulation is always appended to the input vertices
2) always maintain the entire set of input vertices, but filter the PSLG segments for each triangulation (in fact this might mean doing the entire triangulation upfront, then iterating again through every object in turn)

**Triangle currently provides the only easily accessible constrained Delaunay triangulation algorithm, as far as I know. CGAL does provide boundary constraints, but is pretty hard to use and maybe?? doesn't easily allow maximum triangle area. Manifold does boundary constraints, but not maximum area and is not free software (it's affordable, but not open). Spatstat, deldir, GEOS/rgeos and geometry packages all have Delaunay but not with constraints. GDAL has developing Delaunay work but it's not finished. PostGIS has GEOS capability, maybe something more? Eonfusion does constraint but not maximum area. There are others . . .

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


# Related

https://htmlpreview.github.io/?https://github.com/mstrimas/rgeos-scale/blob/master/rgeos-scale.html

http://www.datasciencecentral.com/forum/topics/3-d-visualizations-for-small-and-big-data


