## edge  pairs from sequential indexes for polygons
prs1 <- function(x) {
  x1 <- cbind(head(x,-1), tail(x,-1))
  rbind(x1, c(x1[length(x1)], x1[1]))
}

#' Make a Planar Straight Line Graph from gris.
#'
#'
#' @param x  gris object
#'
#' @return pslg object from RTriangle
#' @export
#' @importFrom RTriangle pslg
#' @importFrom dplyr select %>% 
mkpslg <- function(x) {
  ## remap vertices
  x$v$remap <- seq(nrow(x$v))
  x$bXv$.vx0 <- x$v$remap[match(x$bXv$.vx0, x$v$.vx0)]
  pslg(
    P = x$v %>% dplyr::select(x, y) %>% as.matrix(),
    S = do.call(rbind, lapply(split(
      x$bXv$.vx0, x$bXv$.br0
    ), prs1))
  )
  
}


#' gris
#'
#' @param x Spatial* object
#' @param ... unused
#'
#' @return gris
#' @export
#' @section Methods:
#'
#' \code{gris} implements these base methods:
#'
#' \describe{
#' \item{plot}{Plots the object according to its topology type, or by a chosen \code{type}. This
#' method follows base plotting with types "p" points, "l" lines, and adds "pp" for polygons}
#' \item{print}{Prints the object and vertices tables with a summary of the topology linkages}
#' \item{\code{[}}{Subset as an object table on attributes, propagates down through the geometry
#' links to return a subsetted gris object}
#' }
gris <- function(x, ...) {
  UseMethod("gris")
}

gris.full <- function(o,  b, bXv, v, georef = NULL) {
  x <- list(
    o = o,  b = b, bXv = bXv, v = v, 
    georef = georef
  )
  class(x) <- c("gris", "list")
  x
}

## vectors of (possibly named) arguments in ... or in a list
#' @export
#' @importFrom dplyr as_data_frame
gris.default <- function(..., topotype = "p") {
  x <- list(...)
  as_data_frame(setNames(x, buildnames(x)))
}

# build names as required, but preserve any that are input
# generated names may clobber later ones that are specified, cest la vie
buildnames <- function(x) {
  nams <- names(x)
  if (is.null(nams)) {
    nams <-  dfn(x)
  }
  nonames <- nchar(nams) < 1
  if (any(nonames)) {
    nams[nonames] <- dfn(x)[nonames]
  }
  make.unique(nams, sep = "")
}


# generate x, y, z, t and then as many more as required appending integers to the alphabet
dfn <- function(x) {
  ind <- c(24, 25, 26, 20)
  first <-
    rep_len(c(letters[ind], letters[-ind], letters), length.out = length(x))
  make.unique(first, sep = "")
}

#' @rdname gris
#' @param i indices for extract
#' @param j indices for extract
#' @param drop ignored
#' @export
#' @importFrom dplyr semi_join
`[.gris` <- function (x, i, j, drop = FALSE) {
  o <- x$o[i,j,drop = drop]
  ## TODO: what if i is repeated? we need to make unique
  ## o$.ob0 <- update dupes somehow
 # oXb <- x$oXb %>% semi_join(o, by = ".ob0")
  b <- x$b %>% semi_join(o, by = ".ob0")
  bXv <- x$bXv %>% semi_join(b, by = ".br0")
  v <- x$v %>% semi_join(bXv, by = ".vx0")
  #gris.full(o, oXb, b, bXv, v)
  gris.full(o,  b, bXv, v)
}

#' @rdname gris
#' @param width width
#' @param n n
#' @param y y
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

#' @rdname gris
#' @export
plot.gris <- function(x, y, ...) {
  ## forget y
  largs <- list(x = x$v %>% dplyr::select(x, y),   ...)
  if (is.null(largs$type))
    largs$type <- "pp"  ## default to polygon for now
  rule <- "evenodd"
  if (!is.null(largs$rule)) {
    if (largs$type != "pp") warning("argument 'rule' ignored for non polygon plot")
    rule <- largs$rule 
    largs$rule <- NULL
  }
  
  if (is.null(largs$add) || !largs$add) {
    otype <- largs$type
    largs$type <- "n"
    largs$add <- NULL
    
    do.call(plot, largs)
    largs$type <- otype
  }
  if (largs$type == "pp") largs$rule <- rule
 uoid <- unique(x$o$.ob0)
  if (is.null(largs$col))
    largs$col <- sample(grey(seq_along(uoid) / (length(uoid) + 1)))
  col <- rep(largs$col, length(uoid))
  type <- largs$type
  largs$type <- NULL
  
  for (i in seq(length(uoid))) {
    ##asub <- x %>% dplyr::filter(.ob0 == uoid[i]) %>% dplyr::select(x, y, .ob0, .br0)
    asub <-
      x$o %>% dplyr::select(.ob0) %>%  filter(.ob0 == uoid[i]) %>%
      ##inner_join(x$oXb, by = c(".ob0" = ".ob0")) %>%
      inner_join(x$b, by = c(".ob0" = ".ob0")) %>%
      inner_join(x$bXv, by = c(".br0" = ".br0")) %>%
      inner_join(x$v, by = c(".vx0" = ".vx0")) %>%
      dplyr::select(x, y, .br0)
    
    largs$col <- col[i]
    
    
    x1 <-
      asub %>% mutate(mg = .br0) %>%  group_by(mg) %>% do(rbind(., NA_real_))
    largs$x <- x1[-nrow(x1),]
    largs$y <- NULL
    
    
    if (type == "pp") {
      
      do.call(polypath, largs)
    }
    
    if (type == "l") {
      do.call(lines, largs)
    }
    if (type == "p")
      do.call(points, largs)
  }
  invisible(x)
}


#' @export
#' @rdname gris
as.gris <- function(x, ...)
  UseMethod("as.gris")

#' @export
as.gris.gris <- function(x, ...)
  x

#' @param x triangulation (package RTriangle)
#' @param type mesh, triangle polygons, or triangle edges
#' @export
#' @rdname gris
#' @importFrom dplyr data_frame
as.gris.triangulation <- function(x, type = c("mesh", "poly", "line")) {
  type <- match.arg(type)
  .tri2gris(x, type = type)
}


.tri2gris <- function(xx, type) {
  o <-
    list(v = data_frame(
      x = xx$P[,1], y = xx$P[,2], .vx0 = seq(nrow(xx$P))
    ))
  
  multi <- !type == "mesh"
  if (type == "line") {
    prims <- xx$E
    primNVerts <- 2
    #stop("gris line creation not yet implemented")
  } else {
    prims <- xx$T
    primNVerts <- 3
  }
  o$b <- data_frame(.br0 = seq(nrow(prims)))
  

  if (multi) {
    #prims <- xx$T
    o$b$.ob0 <- seq(nrow(o$b))
    o$o <- data_frame(.ob0 = seq(nrow(o$b)))
  } else {
    o$b$.ob0 <- rep(1, nrow(o$b))
    o$o <- data_frame(.ob0 = 1)
  }
  
  o$bXv <-
    data_frame(.vx0 = as.vector(t(prims)), .br0 = rep(seq(nrow(prims)), each = primNVerts))
 # o$oXb <-
 #   data_frame(.ob0 = rep(1, nrow(xx$T)), .br0 = seq(nrow(xx$T)))
  
  class(o) <- c("gris", "list")
  o
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
#' @export
gris.SpatialLinesDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}

#' @rdname sp2gris
#' @export
gris.SpatialPointsDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}






topotype <- function(x) {
  "basic gris"
}




#' @importFrom sp proj4string
bld2 <- function(x, normalize_verts = TRUE, ...) {
  x0 <- x  ## need for test lower down, must fix
  g <- sp::geometry(x)
  proj <- proj4string(x)
  o <- as_data_frame(as.data.frame(x))
  o <- o %>% mutate(.ob0 = row_number())
  if (inherits(x0, "SpatialPoints"))
    mcoords <- coordinates(g)
  x <- vector("list", nrow(o))
  for (i in seq_along(x)) {
    if (inherits(x0, "SpatialPolygons"))
      rawcoords <-
        lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
          m <- head(g@polygons[[i]]@Polygons[[xi]]@coords,-1)
          dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
        })
    
    if (inherits(x0, "SpatialLines"))
      rawcoords <- lapply(seq_along(g@lines[[i]]@Lines), function(xi) {
        m <- g@lines[[i]]@Lines[[xi]]@coords
        dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
      })
    ## obviously this could be much faster without the loop
    if (inherits(x0, "SpatialPoints"))
      rawcoords <-
        list(dplyr::data_frame(x = mcoords[i,1], y = mcoords[i,2], .br0 = i))
    
    ## d$nbranches[i] <- length(rawcoords)
    l <- do.call(bind_rows, rawcoords)
    if (i > 1)
      l$.br0 <- l$.br0 + max(x[[i - 1]]$.br0)
    l <- l %>% dplyr::mutate(.ob0 = i)
    x[[i]] <- l
  }
  v <- do.call(bind_rows, x) %>% mutate(.vx0 = row_number())
  b <-
    v  %>% distinct(.br0)  %>% transmute(.br0 = .br0, .ob0 = .ob0)
  bXv <-
    b %>% dplyr::inner_join(v, by = c(".br0", ".ob0")) %>% dplyr::select(.br0, .vx0, .ob0)
#  oXb <-
#    o %>% dplyr::inner_join(b, by = ".ob0") %>% dplyr::select(.ob0, .br0)
  ## clean up
  b <- b %>% dplyr::select(.br0, .ob0)
  v <- v %>% dplyr::select(-.br0,-.ob0)
  
  ##v <-  v  %>% distinct(x, y)
  ##bXv <- bXv  %>% semi_join(v)
  ## watch out for bad levels https://github.com/hadley/dplyr/issues/859
  o <-
    as_data_frame(lapply(o, function(x) {
      if (isTRUE(all.equal(attr(x, 'levels'), character(0)))) {
        attr(x, 'levels') <- NULL
      }; x
    }))
  obj <- gris.full(v = v, bXv = bXv, b = b, o = o, georef = .georeference(proj4 = proj))

  print(nrow(obj$v))
  if (normalize_verts) {
    obj0 <- normalizeVerts2(obj$v, obj$bXv , c("x", "y"))
    obj$v <- obj0$v
    obj$bXv <- obj0$bXv
  }
  obj <- triGris(obj)
  # print(nrow(obj$v))
  # print(range(obj$bXv$.vx0))
  obj
}

.georeference <- function(proj4 = "NA_character_", ...) {
  gg <- list(proj4 = proj4)
  class(gg) <- c("georef", "list")
  gg
}
#' @importFrom dplyr arrange
normalizeVerts2 <- function(v, bXv, nam) {
  bXv$original <- v$original <- seq(nrow(v))
  ord <- do.call(order, v[nam])
  v <- v[ord, ]
  bXv <- bXv[ord, ]
  dupes <- duplicated(v[, nam])
  v$.vx0 <- bXv$.vx0 <- cumsum(!dupes)
  v <- v[!dupes, ]
  x <- list()
  x$v <- v %>% arrange(original) %>% select(-original)
  x$bXv <- bXv %>% arrange(original) %>% select(-original)
  x
}





# normalizeVerts <- function(v, bXv, nam) {
#   #v <- x$v
#   #bXv <- x$bXv
#   v$badge <- as.character(v$.vx0)
#   vx0 <- v$.vx0
#   EPS <- sqrt(.Machine$double.eps)
#   
#   dupes <- duplicated(v[, nam], fromLast = TRUE)
#   #index <- which(dupes)[1]
#   #dupes <- abs(rep(v[[nam[1L]]][index], nn) - v[[nam[1L]]])  < EPS &
#   #  abs(rep(v[[nam[2L]]][index], nn) - v[[nam[2L]]])  < EPS
#   
#   nn <- nrow(v)
#   while (any(dupes)) {
#     dupeindex <- which(dupes)
#     index <- dupeindex[1L]
#     bad <-
#       abs(rep(v[[nam[1L]]][index], nn) - v[[nam[1L]]])  < EPS &
#       abs(rep(v[[nam[2L]]][index], nn) - v[[nam[2L]]])  < EPS
#     sb <- sum(bad)
#     vx0[bad] <- rep(index, sb)
#     dupes[bad] <- rep(FALSE, sb)
#     
#   }
#   
#   v$.vx0 <- vx0
#   bXv$badge <- v$badge[vx0]
#   v <- v %>% distinct(.vx0)
#   v$.vx0 <- seq(nrow(v))
#   bXv$.vx0 <- v$.vx0[match(bXv$badge, v$badge)]
#   v <- v %>% dplyr::select(-badge)
#   bXv <- bXv %>% dplyr::select(-badge)
#   x <- list()
#   x$v <- v
#   x$bXv <- bXv
#   x
# }

# library(dplyr)
# v <- data_frame(x = rep(1, 10), y = c(1, 2, 3, 2, 5, 6, 2, 8, 9, 2), .vx0 = 1:10)
# nam <- c("x", "y")
# bXv <- data_frame(.vx0 = seq(nrow(v)))
#
# normalizeVerts(v, bXv, nam)


# #' Subset for a vertex/branch/object object
# #'
# #' @param x list of \code{v} vertex, \code{b} branch and \code{o} object tables
# #' @param subset a subset of the \code{o} object
# #' @param ... ignored
# #' @return list of \code{v} vertex, \code{b} branch and \code{o} object tables
# #' @export
# sbs <- function(x, subset, ...) {
#   o <- subset
#   b <- x$b %>% semi_join(o, by = c(".ob0" = "id"))
#   v <- x$v %>%  semi_join(b, by = c(".br0" = ".br0"))
#   list(o = o, b = b, v = v)
# }

#
# #' Generate a vertex/branch/object table from Spatial polygons
# #'
# #' @param x SpatialPolygonsDataFrame
# #' @param ... ignored
# #' @return vertex/branch/object table suitable for \code{pl}
# dv <- function(x, ...) {
#   g <- geometry(x)
#   d <- as.data.frame(x)
#   x <- NULL
#   for (i in seq(nrow(d))) {
#     l <- do.call(bind_rows, lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
#       m <- g@polygons[[i]]@Polygons[[xi]]@coords
#       data_frame(x = m[,1], y = m[,2], .br0 = xi)
#     }))
#     l$.ob0 <- i
#     x <- bind_rows(x, l)
#   }
#   x$id <- seq(nrow(x))
#   x
# }
#
# #' Generate a vertex/branch/object table from Spatial polygons
# #'
# #' @param x SpatialPolygonsDataFrame
# #' @param ... ignored
# #' @return list of \code{v} vertex, \code{b} branch and \code{o} object tables
# #' @export
# #' @importFrom sp geometry
# bld <- function(x, ...) {
#   g <- sp::geometry(x)
#   d <- as.data.frame(x)
#   x <- vector("list", nrow(d))
#   d <- d %>% dplyr::mutate(nbranches = 0)
#   for (i in seq(nrow(d))) {
#
#     rawcoords <- lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
#       m <- g@polygons[[i]]@Polygons[[xi]]@coords
#       data_frame(x = m[,1], y = m[,2], .br0 = xi)
#     })
#     d$nbranches[i] <- length(rawcoords)
#     l <- do.call(bind_rows, rawcoords)
#     if (i > 1) l$.br0 <- l$.br0 + max(x[[i-1]]$.br0)
#     l$.ob0 <- i
#     x[[i]] <- l
#   }
#   x <- do.call(bind_rows, x)
#   x$id <- seq(nrow(x))
#   b <- x %>% dplyr::distinct(.br0) %>% dplyr::select(.br0, .ob0)
#   ## watch out for bad levels https://github.com/hadley/dplyr/issues/859
#   d <-  as_data_frame(lapply(d, function(x) {if(isTRUE(all.equal(attr(x, 'levels'), character(0)))) {attr(x, 'levels') <- NULL}; x}))
#   d <- d %>% dplyr::mutate(id = row_number())
#   list(v = x, b = b, o = d)
# }

#   getCoords <- function(xi) {
#     m <- g@polygons[[i]]@Polygons[[xi]]@coords
#     data_frame(x = m[,1], y = m[,2])
#   }
#   getBranchs <- function(xi) {
#     obj <- g@polygons[[i]]@Polygons[[xi]]
#     data_frame(labptx = obj@labpt[1], labpty = obj@labpt[2], area = obj@area, hole = obj@hole, ringDir = obj@ringDir)
#   }
