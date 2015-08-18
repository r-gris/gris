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

map2gris <- function(x, orphans = TRUE) {
  na <- is.na(x$x)
  segs <- c(0, cumsum(abs(diff(is.na(x$x)))))
  nms <- x$names
  
  y <- x$y
  x <- x$x

    ##clist <- split(data.frame(x$x, x$y)[!na, ], segs[!na])
  v <- data_frame(x = x, y = y, .br0 = segs) %>% filter(!is.na(x)) %>% mutate(.vx0 = seq(n()))
  bXv <- v %>% select(.br0, .vx0)
  v <- v %>% select(x, y, .vx0)
  o <- normalizeVerts2(v, bXv, c("x", "y"))
  o$b <- bXv %>% distinct(.br0) %>% select(.br0) 
  dif <- length(nms) - nrow(o$b)
  if (dif < 0) {
    nms <- c(nms, paste("orphan", seq(-dif)))
  }
  o$o <- data_frame(names = nms, .ob0 = seq(length(nms)))
  o$b$.ob0 <- o$o$.ob0
  class(o) <- c("gris", "list")
  o
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



