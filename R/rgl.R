# edges <- function(x, side = "x") {
#   e <- switch(side, 
#          x = c(xmin(x), xFromCol(x) + res(x)[1]/2), 
#          y = c(ymin(x), rev(yFromRow(x)) + res(x)[2]/2))
#   
#   e
# }

#' pquads
#'
#' pquads
#'
#' @param x mesh object
#' @param texture path to PNG file
#' @param subset option index (on ib)
#' @param ... arguments pass to \code{\link[rgl]{rgl.quads}}
#' @param texcoords texture coordinates
#'
#' @export
pquads <- function(x, texture = NULL, texcoords = NULL, subset = NULL, ...) {
  if (is.null(texcoords)) texcoords <- t(x$vb[1:2,x$ib])
  if (!is.null(subset)) x$ib <- x$ib[,subset]
  rgl::rgl.quads(x$vb[1,x$ib], x$vb[2,x$ib], x$vb[3,x$ib], texcoords = texcoords, texture = texture, ...)
}

## OR, so we get this in raster-native order
#' @importFrom sp coordinates
#' @importFrom raster extend res shift xmin xmax ymin ymax
edgesXY <- function(x) {
  coordinates(shift(
    extend(x, 
           extent(xmin(x), xmax(x) + res(x)[1], ymin(x), ymax(x) + res(x)[2])), 
    x = -res(x)[1]/2, y = -res(x)[2]/2))
}


prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}

p4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}


#' Title
#'
#' @param x raster object for mesh structure
#' @param z raster object for height values
#' @param na.rm remove quads where missing values?
#'
#' @return mesh3d
#' @export
#'
#' @importFrom rgl oh3d
#' @importFrom raster extract extent
#' @importFrom dplyr  bind_rows  distinct  group_by  inner_join  mutate row_number transmute
#' @examples 
#' \dontrun{
#' library(raster)
#'  library(rgl)
#'  library(rglgris)
#'  library(rgdal)
#'  f <- "E:\\DATA\\NASA\\world.topo.bathy.200411.3x21600x21600.D2_10.tif"
#'  
#'  esource <- readAll(crop(raster("E:\\DATA\\Etopo\\Etopo1Ice\\Etopo1.tif"), extent(90, 180, -90, 0)))
#'  
#'  
#'  r <- readAll(brick(f))
#'  extent(r) <- extent(90, 180, -90, 0) 
#'  smashimate <- function(x, smash) {dim(x) <- dim(x)/smash; x}
#'  
#'  sm <-smashimate(r, 10)
#'  rs <- setValues(sm, extract(r, coordinates(sm), method = "simple"))
#'  cols <- brick2col(rs)
#'  
#'  ro <- bgl(rs, z = esource)
#'  ro$vb[1:2,] <- t(project(t(ro$vb[1:2,]), "+proj=laea +lon_0=140 +lat_0=-55"))
#'  ro$vb[3,] <- ro$vb[3,] * 30
#'  shade3d(ro, col = rep(cols, each = 4), lit = FALSE)
#'  library(graticule)
#'  lon <- seq(90, 180, by = 10)
#'  lat <- seq(-85, 0, by = 15),
#'  l <- graticule(lon, lat, proj = "+proj=laea +lon_0=140 +lat_0=-55")
#'  for (i in seq(nrow(l))) {
#'  xy <- coordinates(as(l[i,], "SpatialPoints"))
#'   rgl.lines(cbind(xy, 1000), col = "white")
#'   }
#' }
bgl <- function(x, z = NULL, na.rm = FALSE) {
  x <- x[[1]]  ## just the oneth raster for now
  ##exy <- as.matrix(expand.grid(edges(x), edges(x, "y")))
  exy <- edgesXY(x)
  ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
  
 
  ## all face indexes
  ind0 <- as.vector(ind) + 
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
 
  
  ## need to consider normalizing vertices here
  if (na.rm) {
    ind1 <- matrix(ind0, nrow = 4)
    ind0 <- ind1[,!is.na(values(x))]
  }
  ob <- rgl::oh3d()
  
  if (!is.null(z)) z <- extract(z, exy, method = "bilinear") else z <- 0
  ob$vb <- t(cbind(exy, z, 1))
  ob$ib <- matrix(ind0, nrow = 4)
  ob
}

#' Raster to gris object
#'
#' @param x RasterLayer
#' @param z RasterLayer to extract Z value from
#'
#' @return gris
#' @export
ras2gris <- function(x, z = NULL) {
  ##exy <- as.matrix(expand.grid(edges(x), edges(x, "y")))
  exy <- edgesXY(x)
  ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
  ## all face indexes
  ind0 <- as.vector(ind) + 
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
  
  ##ob <- rgl::oh3d()
  
  if (!is.null(z)) z <- extract(z, exy, method = "bilinear") else z <- 0
  v <- data_frame(x = exy[,1], y = exy[,2], z = z, .vx0 = seq(nrow(exy)))
  bXv <- data_frame(.br0 = rep(seq(length(ind0)/4), each = 4), .vx0 = ind0)
  b <- bXv %>% dplyr::select(.br0) %>% dplyr::distinct(.br0)
  oXb <- b %>% dplyr::mutate(.ob0 = .br0)
  o <- oXb %>% dplyr::select(.ob0)
  gris.full(o,  b, bXv, v)
#   ob$vb <- t(cbind(exy, z, 1))
#   ob$ib <- matrix(ind0, nrow = 4)
#   ob
}


#' Title
#'
#' @param lonlatheight matrix or data.frame of lon,lat,height values
#' @param rad radius of sphere
#' @param exag exaggeration to apply to height values (added to radius)
#'
#' @return matrix
#' @export
#'

llh2xyz <- function(lonlatheight, rad = 6378137.0, exag = 1) {
  cosLat = cos(lonlatheight[,2] * pi / 180.0)
  sinLat = sin(lonlatheight[,2] * pi / 180.0)
  cosLon = cos(lonlatheight[,1] * pi / 180.0)
  sinLon = sin(lonlatheight[,1] * pi / 180.0)
  
  rad <- (exag * lonlatheight[,3] + rad)
  x = rad * cosLat * cosLon
  y = rad * cosLat * sinLon
  z = rad * sinLat
  
  cbind(x, y, z)
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
#'

#' @importFrom raster values
brick2col <- function(x) {
  ## count left to right from bottom to top
  ## (raster is left to right top to bottom)
  ##ord  <- brick2rgl(x)
  v <- values(x)  ##[ord, ]
  
  rgb(v[,1], v[,2], v[,3], maxColorValue = 255)
}