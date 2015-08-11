

vertsToPoly <- function(v) {
  Polygon(as.matrix(v[, dfn(1:2)]))
}
vertsToLine <- function(v) {
  Line(as.matrix(v[, dfn(1:2)]))
}
vertsToPoint <- function(v) {
  as.matrix(v[, dfn(1:2)])
}
#' Return a list of Polygon, Line or matrix
grisToSPbase <- function(x, type = "pp") {
  conv <- switch(type, pp = vertsToPoly, l = vertsToLine, p = vertsToPoint)
    branches <- x$v %>% 
    inner_join(x$bXv, by = c(".vx0" = ".vx0")) %>% 
    inner_join(x$b, by = c(".br0" = ".br0"))
  lapply(split(branches, branches$.br0), conv)
}

grisToSpatialPolygons <- function(x) 
  SpatialPolygons(lapply(x$o$.ob0, function(obid) Polygons(gris:::grisToSPbase(x[x$o$.ob0 == obid, ]), obid)))



#for (i in seq(nrow(gline$o))) gris:::grisToSPbase(gline[i, ])
# grisToSPlist <- function(x) {
#   if (inherits(x[[1L]], "Polygon")) wrap <- Polygons
#   if (inherits(x[[1L]], "Line")) wrap <- Lines
#   if (inherits(x[[1L]], "matrix")) wrap <- identity
#   wrap(x)
# }



