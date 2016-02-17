## edge  pairs from sequential indexes for polygons
prs1 <- function(x) {
  x1 <- cbind(head(x,-1), tail(x,-1))
  rbind(x1, c(x1[length(x1)], x1[1]))
}


# Planar Straight Line Graph
# 
# Constrained triangulation requires a Planar Straight Line Graph consisting of
# unique vertices and an index matrix for each line segment edge. This function builds that object.
# @param x  gris object
# 
# @return \code{\link[RTriangle]{pslg}} Planar Straight Line Graph object
# @importFrom RTriangle pslg
# @importFrom dplyr select %>%
# @export
mkpslg <- function(x) {
  ## remap vertices
  x$v$remap <- seq(nrow(x$v))
  x$bXv$.vx0 <- x$v$remap[match(x$bXv$.vx0, x$v$.vx0)]
  pslg.default(
    x = x$v %>% dplyr::select(x, y) %>% as.matrix(),
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
  v$.vx0 <- as.integer(v$.vx0)
  bXv$.vx0 <- as.integer(bXv$.vx0)
  bXv$.br_order <- as.integer(bXv$.br_order)
  bXv$.br0 <- as.integer(bXv$.br0)
  b$.br0 <- as.integer(b$.br0)
  if (".h0" %in% names(b)) b$.h0 <- as.integer(b$.h0)
  b$.ob0 <- as.integer(b$.ob0)
  o$.ob0 <- as.integer(o$.ob0)
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
  do_tri <- "tXv" %in% names(x)  
  o <- x$o[i,j,drop = drop]
  ## TODO: what if i is repeated? we need to make unique
  ## o$.ob0 <- update dupes somehow
  # oXb <- x$oXb %>% semi_join(o, by = ".ob0")
  b <- x$b %>% semi_join(o, by = ".ob0")
  bXv <- x$bXv %>% semi_join(b, by = ".br0")
  v <- x$v %>% semi_join(bXv, by = ".vx0")
  if (do_tri) {
    oXt <- x$oXt %>% semi_join(o, by = ".ob0")
    tXv <- x$tXv %>% semi_join(oXt, by = ".tr0")
  #  v <- bind_rows(x$v %>% semi_join(tXv, c(".vx0" = ".vx1")),
  #                  x$v %>% semi_join(tXv, c(".vx0" = ".vx2")),
  #                  x$v %>% semi_join(tXv, c(".vx0" = ".vx3"))) %>%
  #    distinct_(".vx0")

  } else {
    v <- x$v %>% semi_join(bXv, by = ".vx0")
  }
  #gris.full(o, oXb, b, bXv, v)
  x <- gris.full(o,  b, bXv, v)
  if (do_tri) {
    x$tXv <- tXv 
   x$oXt <- oXt
  }
  x
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
#' @importFrom dplyr do ungroup
plot.gris <- function(x, y,  triangles  = FALSE, ...) {
  xrange <- range(x$v$x, na.rm = TRUE)
  yrange <- range(x$v$y, na.rm = TRUE)
  largs <- list(...)
  if (is.null(largs$type)) {
    type <- "pp"  ## default to polygon for now
  }
  rule <- "evenodd"
  if (!is.null(largs$rule)) {
    if (largs$type != "pp") warning("argument 'rule' ignored for non polygon plot")
    rule <- largs$rule 
  }
  # type, rule
  if (is.null(largs$add) || !largs$add) {
    #otype <- largs$type
    largs$type <- "n"
    largs$add <- NULL
    largs$x <- xrange
    largs$y <- yrange
    do.call(plot, largs)
    largs$type <- NULL
  }
  if(triangles) {
    plotT(x ,border = "black")
    return(invisible(x))
  }
  if (type == "pp") largs$rule <- rule
  uoid <- unique(x$o$.ob0)
  if (is.null(largs$col))
    largs$col <- sample(grey(seq_along(uoid) / (length(uoid) + 1)))
  col <- rep(largs$col, length(uoid))
  dNA <- data_frame(x = NA_real_, y = NA_real_)

  largs$y <- NULL
  
  ## do all of this upfront, add the NAs in loop
  a1 <- 
    x$o %>% select(.ob0) %>% inner_join(x$b, '.ob0') %>% inner_join(x$bXv, '.br0') %>% inner_join(x$v, ".vx0") %>% 
    #group_by(.br0) %>% 
    select(x, y, .br0, .ob0)  
  #%>% ungroup ## ungroup to speed up filter
  
  for (i in seq(length(uoid))) {
    largs$col <- col[i]
    a <- a1 %>% filter(.ob0 == uoid[i])
  if (length(unique(a$.br0)) > 1) {
      a <- do.call(bind_rows, lapply(split(a[, c("x", "y")], a$.br0), function(x) bind_rows(x, dNA)))
     # a <- a[-nrow(a), ]
      largs$x <- head(a$x, -1)
      largs$y <- head(a$y, -1)
    } else {
    largs$x <- a$x
    largs$y <- a$y
    }
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

#' as.gris
#' @param x triangulation (package RTriangle)
#' @param type mesh, triangle polygons, or triangle edges
#' @export
#' @importFrom dplyr data_frame
as.gris.triangulation <- function(x, ...) {
  largs <- list(...)
  types = c("mesh", "poly", "line")
  type <- grep(largs$type, types, value = TRUE)
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
#    data_frame(.vx0 = as.vector(t(prims)), .br0 = rep(seq(nrow(prims)), each = primNVerts))
  data_frame(.vx0 = as.vector(t(xx$T)), .br0 = rep(seq(nrow(xx$T)), each = 3))
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
#' @rdname sp2gris
#' @export
gris.SpatialMultiPointsDataFrame <- function(x, ...) {
  x <- bld2(x, ...)
  class(x) <- c("gris", "list")
  x
}

#' Not yet implemented
#'
#' @param x 
#'
#' @return gris
#' @export
#'
#' @examples
#' \dontrun{
#'  data(wrld_simpl)
#'  g <- gris(wrld_simpl)
#'  sp <- as.SpatialPolygonsDataFrame(g)
#'  gm <- raster::geom(sp)
#' }
grisFromFortify <- function(x) {
  stop("not yet implemented")
}

#' Not yet implemented
#'
#' @param x 
#'
#' @return gris
#' @export
#'
#' @examples
grisFromRasterGeom <- function(x) {
  stop("not yet implemented")
}

topotype <- function(x) {
  "basic gris"
}



#' @importFrom dplyr %>%  group_size summarize group_by_
#' @importFrom sp proj4string
bld2 <- function(x, normalize_verts = TRUE, triangulate = FALSE, ...) {
 proj <- proj4string(x)
  o <- as_data_frame(x@data)  ## as.data.frame doesn't work for multi-points
  o <- o %>% mutate(.ob0 = row_number())
  
  ## original gris method
  ## need to find out why this had more vertices?
  ## it's because raster is not removing the closing vertex on polygons, but I was
  #fhttps://github.com/mdsumner/gris/issues/15
  # v <- exall(x)
  
  ## a <- t(raster::geom(x))
##  setNames(split(t(a),seq(ncol(a))), colnames(a))
  ## leverage raster::geom
  if (inherits(x, "SpatialPolygons")) {
    rg <- .polysGeom(x)
    v <- data_frame(x = rg[,"x"], y = rg[,"y"], 
                    .br0 = rg[,"cump"], 
                    .ob0 = rg[,"object"], 
                    .h0 = rg[,"hole"],  
                    .vx0 = seq(nrow(rg)))
    v$redundant <- c(diff(v$.br0), 0)
    #print(v)
    v <- v %>% dplyr::filter_("redundant < 1") 
    v$redundant <- NULL
    #print(v)
  }
  if (inherits(x, "SpatialLines")) {
    rg <- .linesGeom(x)
    v <- data_frame(x = rg[,"x"], y = rg[,"y"], 
                    .br0 = rg[,"cump"], 
                    .ob0 = rg[,"object"], 
                    .vx0 = seq(nrow(rg)))
  }
 if (inherits(x, "SpatialPoints") | inherits(x, "SpatialMultiPoints")) {
    rg <- .pointsGeom(x)
    v <- data_frame(x = rg[,"x"], y = rg[,"y"],  
                    .br0 = rg[,"cump"], 
                    .ob0 = rg[,"object"], 
                    .vx0 = seq(nrow(rg)))
 }
  
  ## original vertex order within a branch
  v$.br_order <- unlist(lapply(dplyr::group_size(group_by_(v, ".br0")), seq))
  
  
  b <- v  %>% distinct(.br0) 
  bXv <- b %>% dplyr::select_(".br0", ".ob0") %>% 
    dplyr::inner_join(v, by = c(".br0", ".ob0")) %>% 
    dplyr::select(.br0, .vx0, .ob0, .br_order)
  
  ## clean up
  b <- b %>% dplyr::select(-x, -y, -.vx0, -.br_order)
  #v <- v %>% dplyr::select(-.ob0)
  
  ## watch out for bad levels https://github.com/hadley/dplyr/issues/859
 o <-
    as_data_frame(lapply(o, function(x) {
      if (isTRUE(all.equal(attr(x, 'levels'), character(0)))) {
        attr(x, 'levels') <- NULL
      }; x
    }))
obj <- gris.full(v = v, bXv = bXv, b = b, o = o, georef = .georeference(proj4 = proj))
  
 # print(nrow(obj$v))
 if (normalize_verts) {
    obj0 <- normVerts(v, c("x", "y"))
    v <- obj0$v 
    bXv <- obj0$bXv
  } else {
    bXv <- v %>% select(.vx0, .br0, .br_order)
  }
#bXv <- bXv %>% select(-.br_order)
  v <- v %>% select(-.br0, -.ob0, -.br_order)
  v$.h0 <- NULL
  obj <- gris.full(v = v, bXv = bXv, b = b, o = o, georef = .georeference(proj4 = proj))
  
  if (triangulate) {
    obj <- triangulate.gris(obj)
  }
  obj
}

normVerts <- function(v, nam) {
  v$.vx0 <- as.integer(factor(do.call("paste", c(v[,nam], sep = "\r"))))
  bXv <- v %>% select(.vx0, .br0, .br_order)
  v <- v %>% distinct(.vx0) 
  list(v = v, bXv = bXv)
}



.georeference <- function(proj4 = "NA_character_", ...) {
  gg <- list(proj4 = proj4)
  class(gg) <- c("georef", "list")
  gg
}


#' #' Title
#' #'
#' #' @param v vertices *with* branch ID .br0
#' #' @param nam 
#' normVerts <- function(v, nam){
#'   text1 <- do.call("paste", c(v[, nam], sep = "\r"))
#' #ord <- order(text1)
#'   
#'   dupes <- duplicated(text1)
#'   these <- which(dupes)
#'   vx0 <- v$.vx0
#'   cnt <- 0
#'   # while(any(dupes)) {
#'   #  # cnt <- cnt + 1
#'   #   this <- these[1L]
#'   #   
#'   #   ind <-  which(text1 == text1[this]) 
#'   #   vx0[ind] <- vx0[this]
#'   #   dupes[ind] <- FALSE
#'   #   these <- which(dupes)
#'   # }
#'  while(any(dupes)) {
#'     cnt <- cnt + 1
#'     this <- these[cnt]
#' 
#'     ind <-  which(text1 == text1[this])
#'     vx0[ind] <- vx0[this]
#'     dupes[ind] <- FALSE
#'   }
#'   v$.vx0 <- vx0
#'   bXv <- v %>% select(.vx0, .br0, .br_order)
#'   v <- v %>% distinct(.vx0)  
#'   list(v = v, bXv = bXv)
#' }

# 
# 
# library(dplyr)
# ## one object, two branches
# v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), .br0 = 1, .ob0 = 1)
# v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), .br0 = 2, .ob0 = 1)
# 
# ## another object two branches
# v3 <- v1 %>% mutate(x = x + 2, .br0 = 4, .ob0 = 2)
# v4 <- v2 %>% mutate(x = x + 2, .br0 = 5, .ob0 = 2)
# ## modify one to have concavity
# v4 <- bind_rows(v4[1,], data_frame(x = 2.9, y = 0.6, .br0 = 5, .ob0 = 1), v4[2:3, ])
# ## third branch in first  object
# v0 <- data_frame(x = c(0.1,  0.4, 0.5, 0.3), y = c(0.05,  0.05,  0.12, 0.2), .br0 = 3, .ob0 = 1)
# v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(.vx0 = seq(n())) 
# v$.br_order <- unlist(lapply(dplyr::group_size(group_by(v, .br0)), seq))
# 
# plot(v %>% select(x, y))
# lapply(split(v, v$.br0), function(x) polypath(x$x, x$y, col = "grey"))
#' 
#' #' @importFrom dplyr arrange
#' normalizeVerts2 <- function(v, bXv, nam) {
#'   bXv$original <- v$original <- seq(nrow(v))
#'   ord <- do.call(order, v[nam])
#'   v <- v[ord, ]
#'   bXv <- bXv[ord, ]
#'   dupes <- duplicated(v[, nam])
#'   v$.vx0 <- bXv$.vx0 <- cumsum(!dupes)
#'   v <- v[!dupes, ]
#'   x <- list()
#'   x$v <- v %>% arrange(original) %>% select(-original)
#'   x$bXv <- bXv %>% arrange(original) %>% select(-original)
#'   x
#' }


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
