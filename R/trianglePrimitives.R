
## boundaryEdges and cycles
## functions not used, but may be
## to regenerate branches from new triangulations
boundaryEdges <- function(x) {
  x$E[x$EB > 0, ]
}
cycles <- function(aa) {
  ii <- 1
  set0 <- ii
  visited <- logical(nrow(aa))
  while(!all(visited)) {
    i0 <- ii
    repeat {
      ii <- which(aa[,1] == aa[ii, 2])
      if (ii == i0) {
        set0 <- c(set0, NA_integer_)
        break; 
      }
      set0 <- c(set0, ii)
    }
    visited <- seq(nrow(aa)) %in% na.omit(set0)
    ii <- which(!visited)[1L]
    if (!is.na(ii)) set0 <- c(set0, ii)
  }
  set0
}



#' Point in triangle test.
#'
#' @param x gris object with triangulation
#' @param pt matrix of xy coordinates
#'
#' @return
#' for PointInTriangle, the index of triangle for each point, in vertex_ form
#' for PointBary, the table of barycentric coordinates and vertex_ index
#' @importFrom geometry tsearch
#' @export
pointInTriangle <- function(x, pt) {
  a <- gris2Structural(x)
  triangle <- a$triangle %>% select(.tr1, .tr2, .tr3) %>% as.matrix
  a$triangle$.tr0[geometry::tsearch(a$vert$x, a$vert$y, triangle, pt[,1], pt[,2], bary = FALSE)]
}


#' @rdname pointInTriangle
pointBary <- function(x, pt) {
  a <- gris2Structural(x)
  triangle <- a$triangle %>% select(.tr1, .tr2, .tr3) %>% as.matrix
  r <- geometry::tsearch(a$vert$x, a$vert$y, triangle, pt[,1], pt[,2], bary = TRUE)
  data_frame(vertex_ = a$vert$vertex_[r$idx], .bary1 = r$p[,1], .bary2 = r$p[,2], .bary3 = r$p[,3])
}

gris2Structural <- function(x) {
  rg <- grisTri2rgl(x)
  v <- t(rg$vb[1:2, ])
  t <- t(rg$it)
  vertex_ <- x$v$vertex_
  list(vert = data_frame(x = v[,1], y = v[,2], vertex_ = vertex_),
       triangle = data_frame(.tr0 = x$tXv$.tr0, .tr1 = t[,1], .tr2 = t[,2], .tr3 = t[,3]))
}

#' Add triangle primitives
#'
#' @param x gris object
#' @param ... passed on to \code{\link[Rtriangle]{triangulate}}, as arguments after the input \code{\link[Rtriangle]{pslg}}
#' @details only \code{a} has been extensively explored, this is the minimum triangle area in the units of the coordinates
#' @aliases triangulate.gris
#' @examples
#' \donttest{
#' \dontrun{
#' library(gris)
#' library(maptools)
#' data(wrld_simpl)
#' gall <- gris(wrld_simpl)
#' g <- gall[gall$o$NAME %in% c("Australia", "New Zealand"), ]
#' g$v <- g$v %>% mutate(x = ifelse(x < 0, x + 360, x))
#' 
#' gT <- gris::triangulate(g)
#' plot(g)
#' plot(gT, triangles = TRUE, add = TRUE)
#' }}
#' 
#' @export
triangulate <- function(x, ...) UseMethod("triangulate")
#' @export
#' @importFrom dplyr filter
triangulate.gris <- function(x, ...) {
  trigris(x)
}






branchAsObject <- function(x, branchID) {
  b <- x$b %>% filter(branch_ == branchID)
  o <- x$o %>% filter(object_ == b$object_)
  bXv <- x$bXv %>% filter(branch_ == branchID)
  v <- x$v %>% filter(vertex_ %in% bXv$vertex_)
  x$v <- v
  x$bXv <- bXv
  x$o <- o
  x$b <- b
  x
}


#' @importFrom dplyr %>% group_by summarize transmute
trigris <- function(x) {
  b <- x$b
  bXv <- x$bXv
  ## we don't touch the objects
  ## first scan for tri-branches and flag as done
  vertsPerBranch <- b %>% inner_join(bXv) %>% group_by(branch_) %>% dplyr::summarize(nverts = n()) 
  branchTri <- vertsPerBranch %>% filter(nverts == 3)
  branch <- vertsPerBranch %>% filter(nverts > 3)
  ## then visit remaining branches and triangulate 
  tXvList <- vector('list', nrow(vertsPerBranch)-1)
  tXvNoTrineed <- bXv %>% filter(branch_ %in% branchTri$branch_) %>% transmute(vertex_, .tr0 = branch_, branch_ = branch_)
  structTri <- matrix(tXvNoTrineed$vertex_ , ncol = 3, byrow = TRUE)
  maxtr <- nrow(tXvNoTrineed)
  tXv0 <- NULL  ## might be 0-rows in this case
  if (maxtr > 0) {
  tXv0 <- data_frame(.vx1 = structTri[,1], .vx2 = structTri[,2], .vx3 = structTri[,3], .tr0 = seq(nrow(structTri)), 
                     branch_ = tXvNoTrineed$branch_[seq(1, nrow(tXvNoTrineed), by = 3)])
                     }
  
  for (ibranch in seq(1, nrow(branch))) {
    obj0 <- branchAsObject(x, branch$branch_[ibranch])
    tri <- RTriangle::triangulate(mkpslg(obj0))
    tXvList[[ibranch]] <- data_frame(.vx1 = obj0$v$vertex_[tri$T[,1]], 
                       .vx2 = obj0$v$vertex_[tri$T[,2]], 
                       .vx3 = obj0$v$vertex_[tri$T[,3]], 
                       .tr0 = maxtr + seq(nrow(tri$T)), 
                       branch_ = rep(branch$branch_[ibranch], nrow(tri$T))
                       )
    maxtr <- maxtr + nrow(tri$T)
  }
  tXv <- bind_rows(tXv0, do.call(bind_rows, tXvList))
  oXt <- tXv %>% 
    dplyr::select(branch_, .tr0) %>% 
    dplyr::inner_join(b %>% dplyr::select(branch_, object_), "branch_") %>% 
    dplyr::select(.tr0, object_) %>% #transmute(.tr0 = .tr0, object_) %>% 
    dplyr::distinct(.tr0, .keep_all = TRUE)
  x$tXv <- tXv #%>% dplyr::select(-branch_)
  x$oXt <- oXt
  x
}



triangulate.default <- function(x, y = NULL, ...) {
  # if (requireNamespace("rgl", quietly = TRUE)) {
  #   rgl::triangulate(x, ...)
  # } else {
  #   
  xy <- xy.coords(x,y)
  RTriangle::triangulate(p = cbind(xy$x, xy$y), ...)
}

triangulate.pslg <- function(x, ...) {
  RTriangle::triangulate(p = x, ...)
}

plotT <- function(x, ...) {
  if (is.null(x$tXv)) stop("no triangles in this object, try:\n\n  x <- triangulate(x)")
  tXv <- x$tXv
  v <- x$v
  for (i in seq(nrow(tXv))) {
    XY1 <- v  %>% inner_join(tXv[i, ], c("vertex_" = ".vx1")) %>% select(x, y)
    XY2 <- v  %>% inner_join(tXv[i, ], c("vertex_" = ".vx2")) %>% select(x, y)
    XY3 <- v  %>% inner_join(tXv[i, ], c("vertex_" = ".vx3")) %>% select(x, y)
    
 
    polypath(c(XY1$x, XY2$x, XY3$x), c(XY1$y, XY2$y, XY3$y), ...)
    
  }
  NULL
}


grisTri2rgl <- function(x, verts = c("x_", "y_"), globe = FALSE, objid = NULL) {
  if (!length(verts) %in% c(2, 3)) stop("named vertices must be 2- or 3- in length")
  v <- x$v
  v$structural_index <- seq(nrow(v))
  t_3d <- t3d
  t_3d$it <- t(cbind(v$structural_index[match(x$tXv$.vx1, x$v$vertex_)], 
                    v$structural_index[match(x$tXv$.vx2, x$v$vertex_)], 
                    v$structural_index[match(x$tXv$.vx3, x$v$vertex_)]))
  
  o <- x$o %>% dplyr::select_("object_")
  o$structural_index <- seq(nrow(o))
  if (is.null(objid)) {
    cols <- sample(grey(seq(0, 1, length = nrow(x$tXv))), replace = FALSE)
    
  } else {
    cols <- sample(grey(seq(0.1, .9, length = length(unique(objid)))))[factor(objid)]
  }
  
  cols <- cols[(x$tXv %>% inner_join(x$oXt, ".tr0") %>% inner_join(o, "object_"))$structural_index]
 t_3d$vb <- v[, verts]
  
  if (ncol(t_3d$vb) == 1L) stop("vertex attributes not found", setdiff(verts, names(v[, verts])))
  if (ncol(t_3d$vb) < 1L) stop("vertex attributes not found", verts)
  if (ncol(t_3d$vb) == 2) t_3d$vb <- cbind(t_3d$vb, z = 0)
  t_3d$vb <- t(cbind(t_3d$vb, w = 1))
  if (globe) t_3d$vb[1:3, ] <- t(llh2xyz(t(t_3d$vb[1:3, ])))
  t_3d$material$color <- rep(cols, each = 3)
    #rep(sample(grey(seq(0, 1, length = ncol(t_3d$it)))), each = 3)
  t_3d
}

#' @export
plot3d <- function(x, ...) UseMethod("plot3d")
#' @rawNamespace 
#' if ( requireNamespace("rgl", quietly = TRUE)) {
#' importFrom("rgl",  plot3d)
#' }
#' @export
plot3d.gris <- function(x, globe = TRUE, verts = c("x_", "y_"), objname = NULL, ...) {
  if (requireNamespace("rgl", quietly = TRUE)) {
    gx <- grisTri2rgl(x, globe = globe, verts = verts, 
                      objid = if(is.null(objname)) NULL else x$o[[objname]])
    rgl::plot3d(gx, ...)
  } else {
    ## persp somesuch
    stop("cannot plot in 3d")
  }
}
