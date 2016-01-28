
# gris

A database table model for storing geometry in R. 

## What we have 
 
*  build relational table structures representing vector GIS and conversion to network models and visualization models
* `Branches` and `Primitives` model to link vertices and objects, including coexistence of both models on one structure
* simplistic raster conversion to OpenGL structures 

## What we need

* Primitives model is currently only triangles, this needs generalization for line segments, for representation and plotting, and some resolution of whether "wide" (v1, v2, v3) or "long" (v0, p0) format is preferable (the former is like the structural form in OpenGL, the latter more relational)
* invalidation, i.e. if we manipulate the mesh of triangles either the branches get removed or re-calculated
* need to fix how gris objects are created, with a proper set of constructor methods, not that  g[i, ] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
* control over coordinate system (crs) for triangulation independent of input crs
* ability to explode and union objectss arbitrarily, and general access to lower level tools (tools in sp/rgeos should be decoupled from the Spatial classes, for example)
* system of naming somehow for vertices in order to generalize away from x/y 
* analogue to the vector case for raster data, to provide n-dimensional curvilinear grids with rectilinear and affine-regular grids as special cases

# Topology

Currently in gris, traditional GIS-like objects are decomposed into a set of relational tables. These are Vertices and Objects, and these two tables may be linked via the *Branches model* with intermediate tables `Vertex-Link-Branches` (bXv) and `Branches` (b), or via the *Primitives model* with intermediate tables  `Vertex-Link-Triangles` (tXv) and `Object-Link-Triangles` (oXt). 

Storing these data in tables is similar to the *ggplot2 model* where a GIS object is converted to a single table with `fortify`, but this duplicates object and branch information by copying them onto every vertex. This is straightforward, but wasteful and does not provide a logical structure for entitities, these are implicit and must be derived by uniqueness tests and so on. In *gris* the `Objects` table is analogous to the *attributes* table in a GIS, each row stores data on an object which may be topologically a (multi) polygon,  line, or  point. The Branches table allows for multiple vertices on a a single point, multiple connected sets of line segments (linear strings) for a single line, and multiple "rings" for a complex polygon that may contain "holes" or include multiple separate "islands". 

Vertex-Link-Branches is necessary so that the vertex table can be normalized, i.e. remove duplicates in x/y (or more generally any combination of attributes). It may be worth having a Branches-Link-Objects table (and triangles-link-objects)  for further normalization of complex layers, but I ignore it for now. 

There are three steps in converting a traditional polygon into a surface: 1) decompose the polygon into a set of vertices with a two-element index specifying every boundary segment, a planar-straight-line-graph (the vertices must also be "normalized" to remove any duplicates) 2) triangulate with Triangle's** constrained Delaunay algorithm (optionally with a maximum triangle area constraint) 3) calculate centroids of every triangle and filter those that compose a "hole" in the original complex polygon. 

Gris converts to the Branches topology by default, but can also use Shewchuk's Triangle to decompose each object to Constrained Delaunay triangular primitives and simply insert tables to link the objects and vertices by this alternative. It would be helpful to include other triangulation methods, like the unconstrained algorithms in  *deldir*, *geometry* and *spatstat*, and the ear-clipping (random or deterministic) algorithms in *rgl*. I'll build in a connection to Manifold's constrained algorithm via the *manifoldr* package. 

Other important comparisons must be done with topojson and D3. 

There is a wrinkle in keeping the relation between objects and primitives in that it may be necessary to perform the triangulation on an object-by-object basis. This complicates the approach since the Triangle vertex index is *structural* rather than relational, but it's working and these notes may need revision: 

1) assume that the addition of Steiner points to the triangulation is always appended to the input vertices
2) always maintain the entire set of input vertices, but filter the PSLG segments for each triangulation (in fact this might mean doing the entire triangulation upfront, then iterating again through every object in turn)

**Triangle currently provides the only easily accessible constrained Delaunay triangulation algorithm. CGAL does provide boundary constraints, but is pretty hard to use and maybe?? doesn't easily allow maximum triangle area. Manifold does boundary constraints, but not maximum area and is affordable, but not open. Spatstat, deldir, GEOS/rgeos and geometry packages all have Delaunay but not with constraints. GDAL (i.e. Even Roualt) is developing Delaunay algorithms due in early 2016. PostGIS has GEOS capability, maybe something more? Eonfusion had constrained triangulations but not further subdivision for maximum area of triangles, and the decomposition to primitives was always done upfront in the native crs which was not necessarily sensible. 


# Related

https://htmlpreview.github.io/?https://github.com/mstrimas/rgeos-scale/blob/master/rgeos-scale.html

http://www.datasciencecentral.com/forum/topics/3-d-visualizations-for-small-and-big-data

http://portolan.leaffan.net/category/gdalogr/

# Story plan

- ingestion of Spatial structures
- ability to feed to rgl
- need for RTriangle
- comparative limitations of sp, difficulty in "normalizing vertices", no XYZ possible, 
- parallel Topology model, Primitives and Branches


# Manifold 

I also plan to make connections with Manifold GIS, and turn that into a dplyr back end interface: 

https://github.com/mdsumner/manifoldr

Related Manifold resources: 

http://www.georeference.org/forum/t87852.23  Inflection point index generator

http://www.georeference.org/forum/t87791.23 Creating a tapered buffer

http://www.georeference.org/forum/t128563.6  Removing duplicates

http://www.georeference.org/forum/t99723.2 Deconstructing areas with multiple branches

http://www.georeference.org/forum/t122710.18

http://www.georeference.org/forum/t108653.30

http://www.georeference.org/forum/t94378r94829

http://www.georeference.org/forum/t70597.61#91661

# Other stuff and motivation

http://mdsumner.github.io/2015/12/28/gis3d.html

http://staff.acecrc.org.au/~mdsumner/Eonfusion/Eonfusion_V2.0_Rasters.mp4

http://portolan.leaffan.net/category/gdalogr/

http://gis.stackexchange.com/questions/168584/representation-of-spatial-lines-with-altitude/168618#168618

https://github.com/domlysz/BlenderGIS/wiki/Make-terrain-mesh-with-Delaunay-triangulation
