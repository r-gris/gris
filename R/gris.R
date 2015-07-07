
##' x is a tbl_df
mpolypath <- function(x, g = 1, ...) {
  x1 <- x %>% mutate(mg = g) %>%  group_by(mg) %>% do(rbind(., NA_real_))
  polypath(x1[-nrow(x1), ], ...)
}
mlinepath <- function(x, g = 1, ..., col = "black") {

  x1 <- split(x, g)
  col <- rep(col, length(x1))
  er <- lapply(seq_along(x1), function(x) lines(x1[[x]], col = col[x],  ...))
  invisible(er)
}


#' Simple plot for vertices by branch and object
#'
#' Input is a data_frame of x, y, coordinates and .br0 and .ob0 the indices of individual branches and objects.
#' @param x vertex table
#' @param col optional colours for each object
#' @param asp aspect ratio for plot
#' @param ... arguments based to graphics::plot
#' @param type p or l for poly or line
#' @return \code{NULL}
#' @export
pl <- function(x, col = NULL, debug = FALSE, asp = NULL,  ..., type = "p") {
  plot(dplyr::select(x, x, y), type = "n", asp = asp, ...)
  uoid <- unique(x$.ob0)
  if (is.null(col)) col <- sample(grey(seq_along(uoid)/(length(uoid)+1)))
  col <- rep(col, length(uoid))
  for (i in seq(length(uoid))) {
    asub <- x %>% dplyr::filter(.ob0 == uoid[i]) %>% dplyr::select(x, y, .ob0, .br0)
    if (type == "p") mpolypath(asub, g = asub$.br0, col = col[i], rule = "evenodd", ...)
    if (type == "l") mlinepath(asub, g = asub$.br0, col = col[i])
  }
  invisible(NULL)
}

#' Subset for a vertex/branch/object object
#'
#' @param x list of \code{v} vertex, \code{b} branch and \code{o} object tables
#' @param subset a subset of the \code{o} object
#' @param ... ignored
#' @return list of \code{v} vertex, \code{b} branch and \code{o} object tables
#' @export
sbs <- function(x, subset, ...) {
  o <- subset
  b <- x$b %>% semi_join(o, by = c(".ob0" = "id"))
  v <- x$v %>%  semi_join(b, by = c(".br0" = ".br0"))
  list(o = o, b = b, v = v)
}


#' Generate a vertex/branch/object table from Spatial polygons
#'
#' @param x SpatialPolygonsDataFrame
#' @param ... ignored
#' @return list of \code{v} vertex, \code{b} branch and \code{o} object tables
#' @export
#' @importFrom sp geometry
bld <- function(x, ...) {
  g <- sp::geometry(x)
  d <- as.data.frame(x)
  x <- vector("list", nrow(d))
  d <- d %>% dplyr::mutate(nbranches = 0)
  for (i in seq(nrow(d))) {

    rawcoords <- lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      data_frame(x = m[,1], y = m[,2], .br0 = xi)
    })
    d$nbranches[i] <- length(rawcoords)
    l <- do.call(bind_rows, rawcoords)
    if (i > 1) l$.br0 <- l$.br0 + max(x[[i-1]]$.br0)
    l$.ob0 <- i
    x[[i]] <- l
  }
  x <- do.call(bind_rows, x)
  x$id <- seq(nrow(x))
  b <- x %>% dplyr::distinct(.br0) %>% dplyr::select(.br0, .ob0)
  ## watch out for bad levels https://github.com/hadley/dplyr/issues/859
  d <-  as_data_frame(lapply(d, function(x) {if(isTRUE(all.equal(attr(x, 'levels'), character(0)))) {attr(x, 'levels') <- NULL}; x}))
  d <- d %>% dplyr::mutate(id = row_number())
  list(v = x, b = b, o = d)
}

#   getCoords <- function(xi) {
#     m <- g@polygons[[i]]@Polygons[[xi]]@coords
#     data_frame(x = m[,1], y = m[,2])
#   }
#   getBranchs <- function(xi) {
#     obj <- g@polygons[[i]]@Polygons[[xi]]
#     data_frame(labptx = obj@labpt[1], labpty = obj@labpt[2], area = obj@area, hole = obj@hole, ringDir = obj@ringDir)
#   }


#' gris
#'
#' @param x
#' @param ...
#'
#' @return gris
#' @export
#'
gris <- function(x, ...) {
  UseMethod("gris")
}

#' Convert Spatial* objects to gris
#'
#' Description of it
#'
#' Details are
#' @param x Spatial* object
#' @param ... not used
#'
#' @return gris
#' @export
#' @rdname sp2gris
gris.SpatialPolygonsDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}

#' @rdname sp2gris
gris.SpatialLinesDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}

#' @rdname sp2gris
gris.SpatialPointsDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}

topotype <- function(x) {
  "basic gris"
}


#' print
#'
#' @param x
#'
#' @param ...
#' @param n
#' @param width
#'
#' @export
print.gris <- function(x, ..., n = NULL, width = NULL) {
  cat("gris object", "\n", sep = "")
  print(x$o)
  cat("\n")
  cat("gris topology ", topotype(x), "\n", sep = "")
  cat("\n")
  cat("gris vertices", "\n", sep = "")
  print(x$v)
  cat("\n")
  invisible(x)
}


bld2 <- function(x, ...) {
  x0 <- x  ## need for test lower down, must fix
  g <- sp::geometry(x)
 o <- as_data_frame(as.data.frame(x))
 o <- o %>% mutate(.ob0 = row_number())
 if (inherits(x0, "SpatialPoints")) mcoords <- coordinates(g)
  x <- vector("list", nrow(o))
  for (i in seq_along(x)) {


    if (inherits(x0, "SpatialPolygons")) rawcoords <- lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
    })

    if (inherits(x0, "SpatialLines")) rawcoords <- lapply(seq_along(g@lines[[i]]@Lines), function(xi) {
      m <- g@lines[[i]]@Lines[[xi]]@coords
      dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
    })
    ## obviously this could be much faster without the loop
    if (inherits(x0, "SpatialPoints")) rawcoords <- list(dplyr::data_frame(x = mcoords[i,1], mcoords[i,2], .br0 = i))

   ## d$nbranches[i] <- length(rawcoords)
    l <- do.call(bind_rows, rawcoords)
    if (i > 1) l$.br0 <- l$.br0 + max(x[[i-1]]$.br0)
    l <- l %>% dplyr::mutate(.ob0 = i)
    x[[i]] <- l
  }
  v <- do.call(bind_rows, x) %>% mutate(.vx0 = row_number())
  b <- v  %>% distinct(.br0)  %>% transmute(.br0 = .br0, .ob0 = .ob0)
  bXv <- b %>% dplyr::inner_join(v) %>% dplyr::select(.br0, .vx0)
  oXb <- o %>% dplyr::inner_join(b) %>% dplyr::select(.ob0, .br0)
  ## clean up
  b <- b %>% dplyr::select(.br0)
  v <- v %>% dplyr::select(-.br0, -.ob0)

  ## no normalize vertices yet
  ##
  ##v <-  v  %>% distinct(x, y)
  ##bXv <- bXv  %>% semi_join(v)
  ## watch out for bad levels https://github.com/hadley/dplyr/issues/859
  o <-  as_data_frame(lapply(o, function(x) {if(isTRUE(all.equal(attr(x, 'levels'), character(0)))) {attr(x, 'levels') <- NULL}; x}))
  list(v = v,
       bXv = bXv,
       b = b,
       oXb = oXb,
       o = o)
}


#' Generate a vertex/branch/object table from Spatial polygons
#'
#' @param x SpatialPolygonsDataFrame
#' @param ... ignored
#' @return vertex/branch/object table suitable for \code{pl}
dv <- function(x, ...) {
  g <- geometry(x)
  d <- as.data.frame(x)
  x <- NULL
  for (i in seq(nrow(d))) {
    l <- do.call(bind_rows, lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      data_frame(x = m[,1], y = m[,2], .br0 = xi)
    }))
    l$.ob0 <- i
    x <- bind_rows(x, l)
  }
  x$id <- seq(nrow(x))
  x
}
