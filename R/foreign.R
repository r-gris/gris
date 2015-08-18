sp2map <- function(x) {
  g <- gris:::bld2(x, normalize_verts = FALSE)
  
  v <- g$o %>% inner_join(g$b) %>% inner_join(g$bXv) %>% inner_join(g$v) %>% 
 # v <- x$v %>% inner_join(x$bXv) %>% inner_join(x$b) %>% inner_join(x$o) %>% 
    mutate(obj = paste(.ob0, .br0, sep = "_")) %>% group_by(obj) %>% 
    do(rbind(., NA_real_))
  v <- v[-nrow(v), ]
  m <- list(x = v$x, y = v$y, range = c(range(v$x, na.rm = TRUE), range(v$y, na.rm = TRUE)), 
            names = unique(v$objs))

  class(m) <- "map"
  m
}




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

#' @export
#' @rdname foreign
as.SpatialPolygonsDataFrame <- function(x, ...) {
  UseMethod("as.SpatialPolygonsDataFrame")
}


#' Convert to Spatial
#'
#' @param x gris object
#' @param ... ignored
#'
#' @export
#' @rdname foreign
as.SpatialPolygonsDataFrame.gris <- function(x, ...) {
  geom <- grisToSpatialPolygons(x)
  dat <- x$o
  rownames(dat) <- dat$.ob0
  dat$.ob0 <- NULL
  SpatialPolygonsDataFrame(geom, as.data.frame(dat))
}


#for (i in seq(nrow(gline$o))) gris:::grisToSPbase(gline[i, ])
# grisToSPlist <- function(x) {
#   if (inherits(x[[1L]], "Polygon")) wrap <- Polygons
#   if (inherits(x[[1L]], "Line")) wrap <- Lines
#   if (inherits(x[[1L]], "matrix")) wrap <- identity
#   wrap(x)
# }



