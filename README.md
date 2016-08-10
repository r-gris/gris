
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Linux Build Status](https://travis-ci.org/mdsumner/gris.svg?branch=master)](https://travis-ci.org/mdsumner/gris) [![Windows Build status](https://ci.appveyor.com/api/projects/status/github/mdsumner/gris?svg=true)](https://ci.appveyor.com/project/mdsumner/gris) [![](http://www.r-pkg.org/badges/version/gris)](http://cran.rstudio.com/web/packages/gris/index.html) [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/gris)](http://cran.r-project.org/web/packages/gris/index.html) [![Coverage Status](https://img.shields.io/codecov/c/github/mdsumner/gris/master.svg)](https://codecov.io/github/mdsumner/gris?branch=master)

gris
====

A database table model for storing geometry in R.

The gris package provides a relational geometry/topology model for spatial data in R. This is inspired by data models used in the commercial products Manifold System GIS and Eonfusion. The main aspirations are

1.  remove the X/Y limitation on vertex attributes for points, lines, surfaces (and polygons)
2.  allow multiple topology types in individual layers
3.  provide a flexible basis for conversion between other formats.
4.  (a similar generalization for raster data)

Installation
------------

``` r
devtools::install_github("mdsumner/gris")
```

Quick example
-------------

Convert `SpatialPolygonsDataFrame` to `gris`. This shows the basics, that we can store Spatial-structures without loss of information.

``` r
library(gris)
library(maptools);data(wrld_simpl)
#> Loading required package: sp
#> Checking rgeos availability: FALSE
#>      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
#>      which has a restricted licence. It is disabled by default;
#>      to enable gpclib, type gpclibPermit()
gg <- gris(wrld_simpl)
plot(gg)
```

![](figures/README-unnamed-chunk-3-1.png)

``` r
str(gg)
#> List of 5
#>  $ o     :Classes 'tbl_df', 'tbl' and 'data.frame':  246 obs. of  12 variables:
#>   ..$ FIPS     : Factor w/ 244 levels "","AC","AE","AF",..: 2 5 6 7 8 10 11 12 13 17 ...
#>   ..$ ISO2     : Factor w/ 246 levels "AD","AE","AF",..: 4 61 17 6 7 9 12 11 14 24 ...
#>   ..$ ISO3     : Factor w/ 246 levels "ABW","AFG","AGO",..: 15 64 18 6 11 3 12 10 16 25 ...
#>   ..$ UN       : int [1:246] 28 12 31 8 51 24 16 32 36 48 ...
#>   ..$ NAME     : Factor w/ 246 levels "Aaland Islands",..: 10 4 16 3 12 7 5 11 14 18 ...
#>   ..$ AREA     : int [1:246] 44 238174 8260 2740 2820 124670 20 273669 768230 71 ...
#>   ..$ POP2005  : num [1:246] 83039 32854159 8352021 3153731 3017661 ...
#>   ..$ REGION   : int [1:246] 19 2 142 150 142 2 9 19 9 142 ...
#>   ..$ SUBREGION: int [1:246] 29 15 145 39 145 17 61 5 53 145 ...
#>   ..$ LON      : num [1:246] -61.78 2.63 47.4 20.07 44.56 ...
#>   ..$ LAT      : num [1:246] 17.1 28.2 40.4 41.1 40.5 ...
#>   ..$ .ob0     : int [1:246] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ b     :Classes 'tbl_df', 'tbl' and 'data.frame':  3768 obs. of  3 variables:
#>   ..$ .br0: int [1:3768] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ .ob0: int [1:3768] 1 1 2 3 3 3 3 3 4 5 ...
#>   ..$ .h0 : int [1:3768] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ bXv   :Classes 'tbl_df', 'tbl' and 'data.frame':  22497 obs. of  3 variables:
#>   ..$ .vx0     : int [1:22497] 5589 5620 5605 5596 5611 5613 16101 17581 18103 18790 ...
#>   ..$ .br0     : int [1:22497] 1 1 1 2 2 2 3 3 3 3 ...
#>   ..$ .br_order: int [1:22497] 1 2 3 1 2 3 1 2 3 4 ...
#>  $ v     :Classes 'tbl_df', 'tbl' and 'data.frame':  21165 obs. of  3 variables:
#>   ..$ x   : num [1:21165] -61.7 -61.9 -61.8 -61.7 -61.9 ...
#>   ..$ y   : num [1:21165] 17 17.1 17.2 17.6 17.6 ...
#>   ..$ .vx0: int [1:21165] 5589 5620 5605 5596 5611 5613 16101 17581 18103 18790 ...
#>  $ georef:List of 1
#>   ..$ proj4: chr " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#>   ..- attr(*, "class")= chr [1:2] "georef" "list"
#>  - attr(*, "class")= chr [1:2] "gris" "list"
```

Convert `gris` to rgl and plot in XYZ.

``` r
gt <- triangulate(gg)
#> Joining, by = ".br0"
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
str(gt)
#> List of 7
#>  $ o     :Classes 'tbl_df', 'tbl' and 'data.frame':  246 obs. of  12 variables:
#>   ..$ FIPS     : Factor w/ 244 levels "","AC","AE","AF",..: 2 5 6 7 8 10 11 12 13 17 ...
#>   ..$ ISO2     : Factor w/ 246 levels "AD","AE","AF",..: 4 61 17 6 7 9 12 11 14 24 ...
#>   ..$ ISO3     : Factor w/ 246 levels "ABW","AFG","AGO",..: 15 64 18 6 11 3 12 10 16 25 ...
#>   ..$ UN       : int [1:246] 28 12 31 8 51 24 16 32 36 48 ...
#>   ..$ NAME     : Factor w/ 246 levels "Aaland Islands",..: 10 4 16 3 12 7 5 11 14 18 ...
#>   ..$ AREA     : int [1:246] 44 238174 8260 2740 2820 124670 20 273669 768230 71 ...
#>   ..$ POP2005  : num [1:246] 83039 32854159 8352021 3153731 3017661 ...
#>   ..$ REGION   : int [1:246] 19 2 142 150 142 2 9 19 9 142 ...
#>   ..$ SUBREGION: int [1:246] 29 15 145 39 145 17 61 5 53 145 ...
#>   ..$ LON      : num [1:246] -61.78 2.63 47.4 20.07 44.56 ...
#>   ..$ LAT      : num [1:246] 17.1 28.2 40.4 41.1 40.5 ...
#>   ..$ .ob0     : int [1:246] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ b     :Classes 'tbl_df', 'tbl' and 'data.frame':  3768 obs. of  3 variables:
#>   ..$ .br0: int [1:3768] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ .ob0: int [1:3768] 1 1 2 3 3 3 3 3 4 5 ...
#>   ..$ .h0 : int [1:3768] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ bXv   :Classes 'tbl_df', 'tbl' and 'data.frame':  22497 obs. of  3 variables:
#>   ..$ .vx0     : int [1:22497] 5589 5620 5605 5596 5611 5613 16101 17581 18103 18790 ...
#>   ..$ .br0     : int [1:22497] 1 1 1 2 2 2 3 3 3 3 ...
#>   ..$ .br_order: int [1:22497] 1 2 3 1 2 3 1 2 3 4 ...
#>  $ v     :Classes 'tbl_df', 'tbl' and 'data.frame':  21165 obs. of  3 variables:
#>   ..$ x   : num [1:21165] -61.7 -61.9 -61.8 -61.7 -61.9 ...
#>   ..$ y   : num [1:21165] 17 17.1 17.2 17.6 17.6 ...
#>   ..$ .vx0: int [1:21165] 5589 5620 5605 5596 5611 5613 16101 17581 18103 18790 ...
#>  $ georef:List of 1
#>   ..$ proj4: chr " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#>   ..- attr(*, "class")= chr [1:2] "georef" "list"
#>  $ tXv   :Classes 'tbl_df', 'tbl' and 'data.frame':  14966 obs. of  5 variables:
#>   ..$ .vx1: int [1:14966] 5589 5596 17800 17781 17791 17802 17778 17787 11068 2608 ...
#>   ..$ .vx2: int [1:14966] 5620 5611 17801 17778 17790 17801 17781 17790 11062 2610 ...
#>   ..$ .vx3: int [1:14966] 5605 5613 17802 17775 17787 17800 17775 17791 11077 2613 ...
#>   ..$ .tr0: int [1:14966] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ .br0: int [1:14966] 1 2 6 7 8 11 12 13 16 17 ...
#>  $ oXt   :Classes 'tbl_df', 'tbl' and 'data.frame':  14966 obs. of  2 variables:
#>   ..$ .tr0: int [1:14966] 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ .ob0: int [1:14966] 1 1 3 3 3 5 5 5 6 7 ...
#>  - attr(*, "class")= chr [1:2] "gris" "list"
gris::plot3d(gt)
rgl::rgl.snapshot("figures/xyz.png")
```

![xyz](figures/xyz.png)

What we have
------------

-   build relational table structures representing vector GIS and conversion to network models and visualization models
-   `Branches` and `Primitives` model to link vertices and objects, including coexistence of both models on one structure
-   simplistic raster conversion to OpenGL structures

What we need
------------

-   Primitives model is currently only triangles, this needs generalization for line segments, for representation and plotting, and some resolution of whether "wide" (v1, v2, v3) or "long" (v0, p0) format is preferable (the former is like the structural form in OpenGL, the latter more relational)
-   need point-in-poly tests independent of sp for determining holes and filtering out triangles (currently using geometry::tsearch)
-   invalidation, i.e. if we manipulate the mesh of triangles either the branches get removed or re-calculated
-   need to fix how gris objects are created, with a proper set of constructor methods, not that g\[i, \] duplicates .ob0 for repeated i values - but maybe it's nonsense to allow repeats anyway?
-   control over coordinate system (crs) metadata (PROJ.4 is not enough, need to be flexible to change, proj4 rather than rgdal etc.)
-   ability to explode and union objects arbitrarily, and general access to lower level tools (tools in sp/rgeos should be decoupled from the Spatial classes, for example)
-   system of naming for vertices in order to generalize away from x/y
-   analogue to the vector case for raster data, to provide n-dimensional curvilinear grids with rectilinear and affine dimensions as special cases

### Storage of triangles and branches together

This is a valuable feature to have, but it's not yet obvious what is the best way to go. Complicating factors are:

-   storage of end coordinate for polygons (gris doesn't do this, though the method used keeps them at first so this needs review)
-   object triangulation for minimum area or angle - this needs vertices inserted so updating IDs per object is more complicated
-   normalization of vertices by branch is possible by triangulating twice, first the holes and then the rest, or by matching original IDs as the vertices are re-collated
-   it may make sense to build the triangular mesh, with holes removed, then find cycles and rebuild the branches - but this has to happen on a per object basis, unless we somehow carry the line segment indexes through

### Links to source data

Triangle meshes can be combined and re-triangulated fairly simply, this means we can work with the union of two or more layers (the Identity intersection etc.). We need a system of recording these links to inputs.

Topology
========

Currently in gris, traditional GIS-like objects are decomposed into a set of relational tables. These are Vertices and Objects, and these two tables may be linked via the *Branches model* with intermediate tables `Vertex-Link-Branches` (bXv) and `Branches` (b), or via the *Primitives model* with intermediate tables `Vertex-Link-Triangles` (tXv) and `Object-Link-Triangles` (oXt).

Storing these data in tables is similar to the *ggplot2 model* where a GIS object is converted to a single table with `fortify`, but this duplicates object and branch information by copying them onto every vertex. This is straightforward, but wasteful and does not provide a logical structure for entitities, these are implicit and must be derived by uniqueness tests and so on. In *gris* the `Objects` table is analogous to the *attributes* table in a GIS, each row stores data on an object which may be topologically a (multi) polygon, line, or point. The Branches table allows for multiple vertices on a a single point, multiple connected sets of line segments (linear strings) for a single line, and multiple "rings" for a complex polygon that may contain "holes" or include multiple separate "islands".

Vertex-Link-Branches is necessary so that the vertex table can be normalized, i.e. remove duplicates in x/y (or more generally any combination of attributes). It may be worth having a Branches-Link-Objects table (and triangles-link-objects) for further normalization of complex layers, but I ignore it for now.

There are three steps in converting a traditional polygon into a surface: 1) decompose the polygon into a set of vertices with a two-element index specifying every boundary segment, a planar-straight-line-graph (the vertices must also be "normalized" to remove any duplicates) 2) triangulate with Triangle's\*\* constrained Delaunay algorithm (optionally with a maximum triangle area constraint) 3) calculate centroids of every triangle and filter those that compose a "hole" in the original complex polygon.

Gris converts to the Branches topology by default, but can also use Shewchuk's Triangle to decompose each object to Constrained Delaunay triangular primitives and simply insert tables to link the objects and vertices by this alternative. It would be helpful to include other triangulation methods, like the unconstrained algorithms in *deldir*, *geometry* and *spatstat*, and the ear-clipping (random or deterministic) algorithms in *rgl*. I'll build in a connection to Manifold's constrained algorithm via the *manifoldr* package.

Other important comparisons must be done with topojson and D3.

There is a wrinkle in keeping the relation between objects and primitives in that it may be necessary to perform the triangulation on an object-by-object basis. This complicates the approach since the Triangle vertex index is *structural* rather than relational, but it's working and these notes may need revision:

1.  assume that the addition of Steiner points to the triangulation is always appended to the input vertices
2.  always maintain the entire set of input vertices, but filter the PSLG segments for each triangulation (in fact this might mean doing the entire triangulation upfront, then iterating again through every object in turn)

\*\*Triangle currently provides the only easily accessible constrained Delaunay triangulation algorithm. CGAL does provide boundary constraints, but is pretty hard to use and maybe?? doesn't easily allow maximum triangle area. Manifold does boundary constraints, but not maximum area and is affordable, but not open. Spatstat, deldir, GEOS/rgeos and geometry packages all have Delaunay but not with constraints. GDAL (i.e. Even Roualt) is developing Delaunay algorithms due in early 2016. PostGIS has GEOS capability, maybe something more? Eonfusion had constrained triangulations but not further subdivision for maximum area of triangles, and the decomposition to primitives was always done upfront in the native crs which was not necessarily sensible.

Related
=======

[Notes](notes_links.md)
