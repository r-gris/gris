
#' Map projection for gris
#'
#' @param x gris object
#' @param ... passed to methods
#'
#' @return gris object
#' @export
#' @importFrom proj4 ptransform
ptransform <- function(x, ...) {
  UseMethod("ptransform")
}

ptransform.default <- function(x, ...) {
  proj4::ptransform(x, ...)
}