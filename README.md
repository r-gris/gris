[![Travis-CI Build Status](https://travis-ci.org/mdsumner/gris.svg?branch=master)](https://travis-ci.org/mdsumner/gris)

# gris

A dplyr-based geometry model for R. This is in early stages and will change a lot.


=======
There's not much to see except for this blog post on building rgl visualizations via gris: 

http://mdsumner.github.io/2015/12/28/gis3d.html

I also plan to make connections with Manifold GIS: 

https://github.com/mdsumner/manifoldr/blob/master/README.md

# What we have 
 
* basic methods to build relational table structures to represent objects from vector GIS and convert to / from network models and visualization modesl
* Branches and or/Primitives model to link vertices and objects
* conversion methods for OpenGL visualization


What we need

* more integration of primitive model for topology - this is working, but we don't record the branch each triangle belongs too - this is possible but could become meaningless if we start manipulating the mesh of triangles
* need to fix how gris objects are created, since g[i, ] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
* control over coordinate system (crs) for triangulation independent of input crs
* ability to explode and union objects s arbitrarily
* system of naming for vertices in order to generalize x/y 



Topology

Currently in gris, traditional GIS-like objects are decomposed into a set of relational tables. These are Vertices, Vertex-Link-Branches, Branches, and Objects. Storing these data in tables is similar to the `ggplot2` model where a GIS object is converted to a single table with *fortify*, but the problem with fortify is that it simply duplicates object and branch information by copying them onto every vertex. This is straightforward in one sense, but is wasteful and does not provide a logical structure. In *gris* the Objects table is analogous to the "attributes" table in a GIS, each row stores data on an object which may be topologically a (multi) polygon,  line, or  point. The Branches table allows for multiple vertices on a a single point, multiple connected sets of line segments (linear strings) for a single line, and multiple "rings" for a complex polygon that may contain "holes" or include multiple separate "islands". 

Vertex-Link-Branches is necessary so that the vertex table can be normalized, i.e. remove duplicates in x/y (or more generally any combination of attributes). It may be worth having a Branches-Link-Objects table (and triangles-link-objects)  for further normalization of complex layers, but I ignore it for now. 

. There are three steps in converting a traditional polygon into a surface: 1) decompose the polygon into a set of vertices with a two-element index specifying every boundary segment, a planar-straight-line-graph (the vertices must also be "normalized" to remove any duplicates) 2) triangulate with Triangle's** constrained Delaunay algorithm (optionally with a maximum triangle area constraint) 3) calculate centroids of every triangle and filter those that compose a "hole" in the original complex polygon. 


Gris converts to the Branches topology by default, but can also use Shewchuk's Triangle to decompose each object to Constrained Delaunay triangular primitives and simply insert tables to link the objects and vertices by this alternative. 

There is a wrinkle in keeping the relation between objects and primitives in that it may be necessary to perform the triangulation on an object-by-object basis. This complicates the approach since the Triangle vertex index is *structural* rather than relational, but it might work with the following: 

1) assume that the addition of Steiner points to the triangulation is always appended to the input vertices
2) always maintain the entire set of input vertices, but filter the PSLG segments for each triangulation (in fact this might mean doing the entire triangulation upfront, then iterating again through every object in turn)

**Triangle currently provides the only easily accessible constrained Delaunay triangulation algorithm. CGAL does provide boundary constraints, but is pretty hard to use and maybe?? doesn't easily allow maximum triangle area. Manifold does boundary constraints, but not maximum area and is affordable, but not open. Spatstat, deldir, GEOS/rgeos and geometry packages all have Delaunay but not with constraints. GDAL (i.e. Even Roualt) is developing Delaunay algorithms due in early 2016. PostGIS has GEOS capability, maybe something more? Eonfusion had constrained triangulations but not further subdivision for maximum area of triangles, and the decomposition to primitives was always done upfront in the native crs which was not necessarily sensible. 


# Related

https://htmlpreview.github.io/?https://github.com/mstrimas/rgeos-scale/blob/master/rgeos-scale.html

http://www.datasciencecentral.com/forum/topics/3-d-visualizations-for-small-and-big-data

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

Motivation

http://gis.stackexchange.com/questions/168584/representation-of-spatial-lines-with-altitude/168618#168618
