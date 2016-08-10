#' @export
plot3d <- function(x, ...) UseMethod("plot3d")
#' @rawNamespace 
#' if ( requireNamespace("rgl", quietly = TRUE)) {
#' importFrom("rgl",  plot3d)
#' }
#' @export
plot3d.gris <- function(x, globe = TRUE, verts = c("x", "y"), objname = NULL, ...) {
  if (requireNamespace("rgl", quietly = TRUE)) {
    gx <- grisTri2rgl(x, globe = globe, verts = verts, 
                      objid = if(is.null(objname)) NULL else x$o[[objname]])
    rgl::plot3d(gx, ...)
  } else {
    ## persp somesuch
    stop("cannot plot in 3d")
  }
}

# pquads
#
# pquads
#
# @param x mesh object
# @param texture path to PNG file
# @param subset option index (on ib)
# @param ... arguments pass to \code{\link[rgl]{rgl.quads}}
# @param texcoords texture coordinates
# @export
pquads <- function(x, texture = NULL, texcoords = NULL, subset = NULL, ...) {
  
 
  
  if (is.null(texcoords)) texcoords <- t(x$vb[1:2,x$ib])
  if (!is.null(subset)) x$ib <- x$ib[,subset]
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("rgl needed for this function to work. Please install it.",
         call. = FALSE)
  
     
  } else {
    rgl::rgl.quads(x$vb[1,x$ib], x$vb[2,x$ib], x$vb[3,x$ib], texcoords = texcoords, texture = texture, ...)
  }
  invisible(x)
}

## OR, so we get this in raster-native order
#' @importFrom sp coordinates
#' @importFrom raster extend res shift xmin xmax ymin ymax
edgesXY <- function(x) {
  ## report to Hijmans 2015-11-06
  #extract(r, expand.grid(c(xmin(r), xmax(r)), c(ymin(r), ymax(r))), method = "bilinear")
  #[1]   NA   NA 99.5   NA
   ## remove this eps fudge once bilinear works
  eps <- sqrt(.Machine$double.eps)
  as.matrix(expand.grid(seq(xmin(x), xmax(x) -eps, length = ncol(x) + 1),
                        seq(ymax(x), ymin(x) + eps, length = nrow(x) + 1)
                        ))
}

prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}

p4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}



# Raster to gris object
#
# @param x RasterLayer
# @param z RasterLayer to extract Z value from
#
# @return gris
# @export
ras2gris <- function(x, z = NULL) {
  ##exy <- as.matrix(expand.grid(edges(x), edges(x, "y")))
  exy <- edgesXY(x)
  ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) + 
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  
 
  if (!is.null(z)) z <- extract(z, exy, method = "bilinear") else z <- 0
  v <- data_frame(x = exy[,1], y = exy[,2], z = z, .vx0 = seq(nrow(exy)))
  bXv <- data_frame(.br0 = rep(seq(length(ind0)/4), each = 4), .vx0 = ind0)
  b <- bXv %>% dplyr::select(.br0) %>% dplyr::distinct(.br0, .keep_all = TRUE)
  oXb <- b %>% dplyr::mutate(.ob0 = .br0)
  o <- oXb %>% dplyr::select(.ob0)
  gris.full(o,  b, bXv, v)
#   ob$vb <- t(cbind(exy, z, 1))
#   ob$ib <- matrix(ind0, nrow = 4)
#   ob
}



rasterPal2RGB <- function(x) {
  setValues(brick(x, x, x), t(col2rgb(x@legend@colortable))[values(x) + 1, ])
}


#' @importFrom raster ncell ncol nrow
brick2rgl <- function(x) {
  as.vector(matrix(seq(ncell(x)), ncol(x))[, nrow(x):1])
}

#' Title
#'
#' @param x RasterBrick
#'
#' @return hex colour character vector
#' @export
#' @importFrom raster values
brick2col <- function(x) {
  ## count left to right from bottom to top
  ## (raster is left to right top to bottom)
  v <- values(x)  ##[ord, ]
  
  rgb(v[,1], v[,2], v[,3], maxColorValue = 255)
}