## edge  pairs from sequential indexes for polygons
prs1 <- function(x) {
  x1 <- cbind(head(x,-1), tail(x,-1))
  rbind(x1, c(x1[length(x1)], x1[1]))
}


## simpler version of mkpslg, to only make edges from branches

mkedges <- function(x) {
  v <- x$v
  bXv <- x$bXv
  v$remap <- seq(nrow(v))
  bXv$vertex_ <- v$remap[match(bXv$vertex_, v$vertex_)]
  E <- do.call(rbind, lapply(split(
    bXv$vertex_, bXv$branch_
  ), prs1))
  ## this is structural for now, needs to store the vertex index
  x$e <- data_frame(s0 = E[,1], s1 = E[,2])
  x
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
  x$bXv$vertex_ <- x$v$remap[match(x$bXv$vertex_, x$v$vertex_)]
  RTriangle::pslg(
    P = x$v %>% dplyr::select_(.dots = c("x_", "y_")) %>% as.matrix(),
    S = do.call(rbind, lapply(split(
      x$bXv$vertex_, x$bXv$branch_
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
  v$vertex_ <- as.integer(v$vertex_)
  bXv$vertex_ <- as.integer(bXv$vertex_)
  bXv$order_ <- as.integer(bXv$order_)
  bXv$branch_ <- as.integer(bXv$branch_)
  b$branch_ <- as.integer(b$branch_)
  if ("island_" %in% names(b)) b$island_ <- as.integer(b$island_)
  b$object_ <- as.integer(b$object_)
  o$object_ <- as.integer(o$object_)
  x <- list(
    o = o,  b = b, bXv = bXv, v = v, 
    georef = georef
  )
  class(x) <- c("gris", "list")
  x
}

## vectors of (possibly named) arguments in ... or in a list
#' @export
#' @importFrom tibble as_tibble
gris.default <- function(..., topotype = "p") {
  x <- list(...)
  as_tibble(setNames(x, buildnames(x)))
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
  make.unique(sprintf("%s_", first, sep = ""))
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

  b <- x$b %>% semi_join(o, by = "object_")
  bXv <- x$bXv %>% semi_join(b, by = "branch_")
  v <- x$v %>% semi_join(bXv, by = "vertex_")
  if (do_tri) {
    oXt <- x$oXt %>% semi_join(o, by = "object_")
    tXv <- x$tXv %>% semi_join(oXt, by = ".tr0")
  #  v <- bind_rows(x$v %>% semi_join(tXv, c("vertex_" = ".vx1")),
  #                  x$v %>% semi_join(tXv, c("vertex_" = ".vx2")),
  #                  x$v %>% semi_join(tXv, c("vertex_" = ".vx3"))) %>%
  #    distinct_("vertex_")

  } else {
    v <- x$v %>% semi_join(bXv, by = "vertex_")
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
#' @importFrom dplyr do ungroup select
plot.gris <- function(x, y,  triangles  = FALSE, ...) {
  xrange <- range(x$v$x_, na.rm = TRUE)
  yrange <- range(x$v$y_, na.rm = TRUE)
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
  uoid <- unique(x$o$object_)
  if (is.null(largs$col))
    largs$col <- sample(grey(seq_along(uoid) / (length(uoid) + 1)))
  col <- rep(largs$col, length(uoid))
  dNA <- data_frame(x_ = NA_real_, y_ = NA_real_)

  largs$y <- NULL
  
  ## do all of this upfront, add the NAs in loop
  a1 <- 
    x$o %>% dplyr::select(object_) %>% 
    inner_join(x$b, 'object_') %>% 
    inner_join(x$bXv, 'branch_') %>% 
    inner_join(x$v, "vertex_") %>% 
    select(x_, y_, branch_, object_)  
  #%>% ungroup ## ungroup to speed up filter
  
  for (i in seq(length(uoid))) {
    largs$col <- col[i]
    a <- a1 %>% filter(object_ == uoid[i])
  if (length(unique(a$branch_)) > 1) {
      a <- do.call(bind_rows, lapply(split(a[, c("x_", "y_")], a$branch_), function(x) bind_rows(x, dNA)))
     # a <- a[-nrow(a), ]
      largs$x <- head(a$x_, -1)
      largs$y <- head(a$y_, -1)
    } else {
    largs$x <- a$x_
    largs$y <- a$y_
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
      x = xx$P[,1], y = xx$P[,2], vertex_ = seq(nrow(xx$P))
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
  o$b <- data_frame(branch_ = seq(nrow(prims)))
  
  
  if (multi) {
    #prims <- xx$T
    o$b$object_ <- seq(nrow(o$b))
    o$o <- data_frame(object_ = seq(nrow(o$b)))
  } else {
    o$b$object_ <- rep(1, nrow(o$b))
    o$o <- data_frame(object_ = 1)
  }
  
  o$bXv <-
#    data_frame(vertex_ = as.vector(t(prims)), branch_ = rep(seq(nrow(prims)), each = primNVerts))
  tibble(vertex_ = as.vector(t(xx$T)), branch_ = rep(seq(nrow(xx$T)), each = 3))

 class(o) <- c("gris", "list")
  o
}



#' Convert Spatial*DataFrame objects to gris
#'
#' All availables Spatial*DataFrame types are supported, and are interpreted via the Branch model. 
#' The gris function can ingest \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}}, \code{\link[sp]{SpatialPointsDataFrame}}, and \code{\link[sp]{SpatialMultiPointsDataFrame}} objects. 
#' 
#' See  \code{vignette("branch-vs-primitives")}
#' @param x Spatial* object
#' @param ... not used
#' @aliases gris.Spatial
#' @return gris
#' @export
#' @rdname sp2gris
gris.Spatial <- function(x, ...) {
  ## one method for all sp (need to dummify if doesn't have a dataframe)
  x <- bld3(x, ...)
  class(x) <- c("gris", "list")
  x
}

gris.SpatialPolygonsDataFrame <- gris.Spatial
gris.SpatialLinesDataFrame <- gris.Spatial
gris.SpatialMultiPointsDataFrame <- gris.Spatial
gris.SpatialPointsDataFrame <- gris.Spatial



#' Not yet implemented
#'
#' @param x 
#'
#' @return gris
#' @export
#'
#' @examples
#' \dontrun{
#'  data(cmu)
#'  
#'  
#'  sp <- as.SpatialPolygonsDataFrame(cmu)
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


#' @importFrom tibble as_tibble
bld3 <- function(x, ...) {
  tabmap <- spbabel::sptable(x)
  tabdat <- tibble::as_tibble(x)
  tabdat$object_ <- seq(nrow(tabdat))
  grasp(tabdat, tabmap)
}

#' convert from spbabel sptable to gris
#' @importFrom dplyr %>% bind_rows distinct_ mutate select select_
#' @importFrom tibble tibble
#' @importFrom spbabel sptable
grasp <- function(dat1, map1) {
  ## we expect that these attributes, taken together are "unique vertices" potentially shared by neighbours
  v_atts <- c("x_", "y_")
  o_atts <- setdiff(names(map1), v_atts)
  b_atts <- setdiff(o_atts, c("order_", "vertex_"))
  bxv_atts <- c(setdiff(names(map1), c("object_", "island_", v_atts)), "vertex_")
  ## classify unique vertices by unique index
  map1 <- map1 %>%
    mutate(vertex_  = as.integer(factor(do.call(paste, select_(map1, .dots = v_atts)))))
  ## branches, owner object and island status
 b <- map1 %>% distinct_(.dots = b_atts) 
  #b <- map1 %>% dplyr::select(-object_, -island_)
  
  ## four tables (dat1, map2, map4, map5)
  bXv <- map1 %>% dplyr::select_(.dots = bxv_atts)
  v <- map1 %>% distinct_(.dots = c(v_atts, "vertex_"))

  list(o = dat1, b = b, bXv = bXv, v = v)
  
}




.georeference <- function(proj4 = "NA_character_", ...) {
  gg <- list(proj4 = proj4)
  class(gg) <- c("georef", "list")
  gg
}
