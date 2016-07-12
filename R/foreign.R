# # these won't work yet (changes to names x_/y_ and auto normalize by bld3) 
#sp2map <- function(x) {
#   g <- bld3(x, normalize_verts = FALSE)
#   
#   v <- g$o %>% inner_join(g$b) %>% inner_join(g$bXv) %>% inner_join(g$v) %>% 
#  # v <- x$v %>% inner_join(x$bXv) %>% inner_join(x$b) %>% inner_join(x$o) %>% 
#     mutate(obj = paste(object_, branch_, sep = "_")) %>% group_by(obj) %>% 
#     do(rbind(., NA_real_))
#   v <- v[-nrow(v), ]
#   m <- list(x = v$x, y = v$y, range = c(range(v$x, na.rm = TRUE), range(v$y, na.rm = TRUE)), 
#             names = unique(v$objs))
# 
#   class(m) <- "map"
#   m
# }
# 
# map2gris <- function(x, orphans = TRUE) {
#   na <- is.na(x$x)
#   segs <- c(0, cumsum(abs(diff(is.na(x$x)))))
#   nms <- x$names
#   
#   y <- x$y
#   x <- x$x
# 
#     ##clist <- split(data.frame(x$x, x$y)[!na, ], segs[!na])
#   v <- data_frame(x = x, y = y, branch_ = segs) %>% filter(!is.na(x)) %>% mutate(vertex_ = seq(n()))
#   bXv <- v %>% select(branch_, vertex_)
#   v <- v %>% select(x, y, vertex_)
#   o <- normalizeVerts2(v, bXv, c("x", "y"))
#   # #may need keep_all here
#   o$b <- bXv %>% dplyr::distinct(branch_) %>% select(branch_) 
#   dif <- length(nms) - nrow(o$b)
#   if (dif < 0) {
#     nms <- c(nms, paste("orphan", seq(-dif)))
#   }
#   o$o <- data_frame(names = nms, object_ = seq(length(nms)))
#   o$b$object_ <- o$o$object_
#   class(o) <- c("gris", "list")
#   o
# }


#' @importFrom sp Polygon Line Polygons SpatialPolygons Lines SpatialLines
vertsToPoly <- function(v) {
  Polygon(as.matrix(v[, dfn(1:2)]))
}
vertsToLine <- function(v) {
  Line(as.matrix(v[, dfn(1:2)]))
}
vertsToPoint <- function(v) {
  as.matrix(v[, dfn(1:2)])
}



grisToSPbase <- function(x, type = "pp") {
  conv <- switch(type, pp = vertsToPoly, l = vertsToLine, p = vertsToPoint)
    branches <- x$v %>% 
    inner_join(x$bXv, by = c("vertex_" = "vertex_")) %>% 
    inner_join(x$b, by = c("branch_" = "branch_"))
  lapply(split(branches, branches$branch_), conv)
}

grisToSpatialPolygons <- function(x) 
  SpatialPolygons(lapply(x$o$object_, function(obid) Polygons(grisToSPbase(x[which(x$o$object_ == obid), ]), obid)))

#' Return a list of Polygon, Line or matrix from gris
#'
#' @export
#' @param x gris object
#' @param type return type:  pp-oly, l-ine, p-oints
#' @rdname foreign
as.SpatialPolygonsDataFrame <- function(x, ...) {
  UseMethod("as.SpatialPolygonsDataFrame")
}


#' Convert gris to Spatial
#'
#' Spatial classes are defined in the \link{sp} package. 
#' @param x gris object
#' @param ... ignored
#'
#' @export
#' @rdname foreign
#' @importFrom sp Polygons SpatialPolygons SpatialPolygonsDataFrame
as.SpatialPolygonsDataFrame.gris <- function(x, ...) {
  geom <- grisToSpatialPolygons(x)
  dat <- x$o
  rownames(dat) <- dat$object_
  dat$object_ <- NULL
  SpatialPolygonsDataFrame(geom, as.data.frame(dat))
}


#for (i in seq(nrow(gline$o))) gris:::grisToSPbase(gline[i, ])
# grisToSPlist <- function(x) {
#   if (inherits(x[[1L]], "Polygon")) wrap <- Polygons
#   if (inherits(x[[1L]], "Line")) wrap <- Lines
#   if (inherits(x[[1L]], "matrix")) wrap <- identity
#   wrap(x)
# }



