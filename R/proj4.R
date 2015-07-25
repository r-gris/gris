
#' Map projection for gris
#'
#' @param x gris object
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


