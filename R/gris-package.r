#' gris: A flexible geometry data model for R.
#'
#' The gris package provides a relational table model for \bold{spatial vector} data in R. \bold{\emph{Spatial vector data}} are topology-based discrete spatial units such as areas (including polygons), lines and points.  
#'
#' @section Gris features: 
#' 
#' \itemize{
#'  \item provide a clear decoupling of topology from geometry (see section "Gris topology" below)
#'  \item transcend the X/Y-only limitation for vertex attributes
#'  \item allow multiple topological types in individual layers (i.e. Manifold or MapInfo drawings)
#'  \item provide a flexible basis for conversion between other formats and domain-specific structures
#' }
#' 
#' @section I. Creation: 
#' \tabular{ll}{
#'  \code{\link{gris.SpatialPolygonsDataFrame}} coerce to SpatialPolygonsDataFrame, SpatialLinesDataFrame, SpatialPointsDataFrame, SpatialMultiPointsDataFrame
#'  \code{\link{gris.full}} create from raw object, branch, and vertex tables
#'  \code{\link{grisFromRasterGeom}} create from \code{\link[raster]{geom}} format
#'  \code{\link{grisFromFortify}} create from \code{\link[ggplot2]{fortify}} format
#'  \code{\link{as.gris.triangulation}} create from \code{\link[rgl]{triangulation}}
#'  }
#' 
#' @section II. Plotting: 
#' \tabular{ll}{
#'  \code{\link{plot.gris}} plot a gris object as polypath, lines, points, or triangles
#'  \code{\link{plot3d.gris}} plot a gris object in 3d
#' }
#' 
#' @section III. Manipulation: 
#' \tabular{ll}{
#'  \code{\link{[.gris}} extract a subset of a gris object
#'  \code{\link{pointInTriangle}} identify which triangle points fall in
#' }
#' @section IV. Conversion to other types: 
#' \tabular{ll}{
#'  \code{\link{as.SpatialPolygonsDataFrame.gris}} coerce to SpatialPolygonsDataFrame, SpatialLinesDataFrame, SpatialPointsDataFrame, SpatialMultiPointsDataFrame
#'  \code{\link{triangulate.gris}} add triangle primitives to a gris object
#'  }
#'  
#' @section V. Miscellaneous: 
#' \tabular{ll}{
#'  \code{\link{quadmeshFromRaster}} create a \code{\link[rgl]{quads3d}} object from a \code{\link[raster]{RasterLayer}}
#'  \code{\link{brick2col}} create hex value colours from an RGB \code{\link[raster]{RasterBrick}}
#'  \code{\link{llh2xyz}} calculate XYZ coordinates of the Earth from longitude,latitude,height
#'  \code{\link{mkpslg}} create a Planar Straight Line Graph \code{\link[RTriangle]{pslg}} object from gris
#'  
#'  }
#'  
#' @section Gris topology: 
#' 
#' Topology defines the way that space is filled by 0D (points), 1D (lines), 2D (areas - or polygons) objects. 
#'  (Higher dimensional topologies are possible but not explored yet.)
#' 
#' Geometry defines an actual instantiation of an object in a given coordinate system. 
#' 
#' Examples of the difference are
#' \itemize{
#'  \item polygons defining continental outlines on the Earth, these shapes are 2D topologically, but geometrically can be plotted in the plane (in long-lat, or projected coordinates) or "draped" around a curve, such as the Earth as a globe or across a topographic landscape
#'  \item lines defining animal tracks in space time, these 1D shapes can have **geometric** vertex attributes such as X, Y, Z, time, temperature, etc. 
#' }
#' Topology dimension may be 0D, 1D, 2D, or higher and geometry dimension is not necessarily coupled to this, or restricted to 2D. 
#' 
#' Note that certain degeneracies are possible and may be important. 
#' A time series of point estimates in space may not have any sensible geometric values at all, but the topological description provided by that time series is a real structure. 
#'
#'
#' @name gris-package
#' @docType package
NULL



#' Template quad 3d object
#' 
#' 
#' @format see rgl oh3d
"q3d"

#' Template triangle 3d object
#' 
#' 
#' @format see rgl tetrahedron3d
"t3d"
