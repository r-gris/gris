#' gris: A flexible geometry data model for R.
#'
#' The gris package provides a relational table model for \bold{spatial vector} data in R. \bold{\emph{Spatial vector data}}
#' are topology-based discrete spatial units such as areas (including polygons), lines and points.
#'
#' These kinds of data are defined in many different ways in various R packages, but the \code{sp} package has provided the most comprehensive and standard model to match the traditional GIS layer forms.
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
#'  \code{\link{gris.Spatial}} \tab create from Spatial*DataFrame  \cr
#'  \code{\link{gris.full}} \tab create from raw object, branch, and vertex tables \cr
#'  \code{\link{grisFromRasterGeom}} \tab create from \code{\link[raster]{geom}} format  \cr
#'  \code{\link{grisFromFortify}} \tab create from \code{\link[ggplot2]{fortify}} format \cr
#'  \code{\link{as.gris.triangulation}} \tab  create from \code{\link[rgl]{triangulation}} \cr
#'  }
#' 
#' @section II. Plotting: 
#' \tabular{ll}{
#'  \code{\link{plot.gris}} \tab plot a gris object as polypath, lines, points, or triangles \cr
#'  \code{\link{plot3d.gris}} \tab plot a gris object in 3d \cr
#' }
#' 
#' @section III. Manipulation: 
#' \tabular{ll}{
#'  \code{\link{[.gris}} \tab extract a subset of a gris object \cr
#'  \code{\link{pointInTriangle}} \tab identify which triangle points fall in \cr
#' }
#' @section IV. Conversion to other types: 
#' \tabular{ll}{
#'  \code{\link{as.SpatialPolygonsDataFrame.gris}} \tab coerce to SpatialPolygonsDataFrame, SpatialLinesDataFrame, SpatialPointsDataFrame, SpatialMultiPointsDataFrame \cr
#'  \code{\link{triangulate.gris}} \tab add triangle primitives to a gris object \cr
#'  }
#'  
#' @section V. Miscellaneous: 
#' \tabular{ll}{
#'  \code{\link{quadmeshFromRaster}} \tab create a \code{\link[rgl]{quads3d}} object from a \code{\link[raster]{RasterLayer}} \cr
#'  \code{\link{brick2col}} \tab create hex value colours from an RGB \code{\link[raster]{RasterBrick}} \cr
#'  \code{\link{llh2xyz}} \tab calculate XYZ coordinates of the Earth from longitude,latitude,height \cr
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
#'  \item lines defining animal tracks in space time, these 1D shapes can have \bold{geometric} vertex attributes such as X, Y, Z, time, temperature, etc. 
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
