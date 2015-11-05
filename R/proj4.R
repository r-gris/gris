
#' Map projection for gris
#'
#' @param x gris object
#' @param target proj4 string
#' @param ... passed to methods
#'
#' @return gris object
#' @export
#' @importFrom proj4 ptransform
#' @examples 
#' library(maptools)
#' data(wrld_simpl)
#' g <- gris(wrld_simpl[10, ])
#' ptransform(g, "+proj=laea +ellps=WGS84")
ptransform <- function(x, ...) {
  UseMethod("ptransform")
}

#' @rdname ptransform
#' @export
ptransform.gris <- function(x, target, ...) {
  m <- x$v %>% select(x, y) %>% mutate(x = x * pi/180, y = y * pi/180)
  m <- proj4::ptransform(m, src.proj = x$georef$proj4, dst.proj = target, ...)
  x$v$x <- m[[1]]
  x$v$y <- m[[2]]
  x
}
ptransform.default <- function(x, ...) {
  proj4::ptransform(data = x, ...)
}


#' @export
#' @rdname gris-summary
xmin <- function(x) UseMethod("xmin")

#' @export
#' @rdname gris-summary
xmax <- function(x) UseMethod("xmax")

#' @export
#' @rdname gris-summary
ymin <- function(x) UseMethod("ymin")

#' @export
#' @rdname gris-summary
ymax <- function(x) UseMethod("ymax")


#' @export
#' @rdname gris-summary
xmin.default <- function(x) raster::xmin(x)

#' @export
#' @rdname gris-summary
xmax.default <- function(x) raster::xmax(x)

#' @export
#' @rdname gris-summary
ymin.default <- function(x) raster::ymin(x)

#' @export
#' @rdname gris-summary
ymax.default <- function(x) raster::ymax(x)

#' min/max for gris
#'
#' @param x gris object
#'
#' @return numeric
#' @export
#' @rdname gris-summary
xmin.gris <- function(x) {
  min(x$v$x)
}

#' @export
#' @rdname gris-summary
xmax.gris <- function(x) max(x$v$x)

#' @export
#' @rdname gris-summary
ymin.gris <- function(x) min(x$v$y)

#' @rdname gris-summary
ymax.gris <- function(x) max(x$v$y)


#' @export
#' @rdname gris-summary
xrange <- function(x, ...) UseMethod("xrange")

#' @export
#' @rdname gris-summary
yrange <- function(x, ...) UseMethod("yrange")

#' @export
#' @rdname gris-summary
xrange.default <- function(x) c(raster::xmin(x), raster::xmax(x))

#' @export
#' @rdname gris-summary
yrange.default <- function(x) c(raster::ymin(x), raster::ymax(x))

#' @param family projection family
#' @export
#' @rdname gris-summary
local_triangulate <- function(x, family = "laea") {
  xmid <- mean(xrange(x))
  ymid <- mean(yrange(x))
  proj4 <- sprintf("+%s +lon_0=%f +lat_0=%f +ellps=WGS84", family, xmid, ymid)
  proj4
}
