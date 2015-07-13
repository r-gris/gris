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
* speed up the normalization of vertices
* consider edge model as default rather than branches
* apply system of naming for vertices . . .
* ~~triangulation with CGAL, constrained Delaunay~~


Problems

* general slowness of gris normalization
* problem with countriesLow: 

e.g. gris(countriesLow) gives a triangulation that is convex on all polygons, while wrld_simpl is fine ?







