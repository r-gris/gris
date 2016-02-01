
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




#' Add triangle primitives
#'
#' @param x gris object
#' @param ... passed on to \code{\link[Rtriangle]{triangulate}}, as arguments after the input \code{\link[Rtriangle]{pslg}}
#' @details only \code{a} has been extensively explored, this is the minimum triangle area in the units of the coordinates
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
  nobject <- nrow(x$o)
  
 bXv <- tXv <-  oXt <- NULL
  maxtr <- 0
  maxvt <- 0
  
  for (i in seq(nobject)) {
    g0 <- x[i, ]
    bXv0 <- g0$bXv %>% inner_join(g0$v, ".vx0")
    ps <- mkpslg(g0)
    tri <- RTriangle::triangulate(ps)  #;plot(tri)
    oX <- data_frame(.ob0 = rep(g0$o$.ob0[1L], nrow(tri$T)), .tr0 = seq(nrow(tri$T)) + maxtr)
    oXt <- bind_rows(oXt, oX)
     
    bXv <- bind_rows(bXv, bXv0) 
    tX <-  data_frame(.vx1 = bXv0$.vx0[tri$T[,1]], .vx2 = bXv0$.vx0[tri$T[,2]], .vx3 = bXv0$.vx0[tri$T[,3]],
                                                .tr0 = oX$.tr0)
    
    tXv <- bind_rows(tXv, tX)
    maxtr <- maxtr + nrow(tri$T)
    maxvt <- maxvt + nrow(tri$P) 
  }
  x$tXv <- tXv
  x$oXt <- oXt
  x$v <- bXv %>% select(-.br0, -.br_order)
  x$bXv <- bXv %>% select(.vx0, .br0, .br_order)
  x 
}
  ## find edges and pull out all segments that trace a border
  # bounds <- cycles(boundaryEdges(tri))
  
  # for (i in seq_along(oid)) {
  #   id <- oid[i]
  #   ##bXv <- g$bXv %>% filter(.br0 == id) %>% inner_join(g$v, ".vx0") %>% select(x, y, .vx0, .br0 )
  #   g0 <- x[x$o$.ob0 == id, ]
  #   v0 <- g0$v
  #   bXv0 <- g0$bXv
  #   ps <- mkpslg(g0)
  #   #ps <- pslg(P = bXv %>%  dplyr::select(x, y) %>% as.matrix(), S = gris:::prs1(seq(nrow(bXv))))
  #   tr <- RTriangle::triangulate(ps)
  #   #bXt <- data_frame(.br0 = rep(id, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
  #   oX <- data_frame(.ob0 = rep(id, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
  #   oXt <- bind_rows(oXt, oX)
  #   
  #   ## here we are using RTriangle's vertices, as there may be new ones added 
  #   ## and so need to record links to the old ones so that branches still work (or maybe we don't care?)
  #   tv0 <- data_frame(x = tr$P[,1], y = tr$P[,2], .vx0 = seq(nrow(tr$P)) + maxvt)
  #   tv <- bind_rows(tv, tv0)
  #   ## this is not right . . .
  #   bXv0$.vx0 <- tv0seq(nrow(bXv0 %>% inner_join(v0, ".vx0")))## tv0$.vx0[seq(1, nrow(bXv0))]
  #   bXv <- bind_rows(bXv, bXv0)
  #   tX <- data_frame(.vx1 = tv0$.vx0[tr$T[,1]], .vx2 = tv0$.vx0[tr$T[,2]], .vx3 = tv0$.vx0[tr$T[,3]],
  #                    .tr0 = oX$.tr0)
  #   
  #   tXv <- bind_rows(tXv, tX)
  #   maxtr <- maxtr + nrow(tr$T)
  #   maxvt <- maxvt + nrow(tr$P)
  # }
 # x <- gris(wrld_simpl)[180, ]
 #  ps <- mkpslg(x)
 #  holecen <- x$b  %>% dplyr::filter(.h0 > 0)  %>% 
 #    inner_join(x$bXv, ".br0")  %>% inner_join(x$v, ".vx0")  %>% 
 #    group_by(.br0)  %>% summarize(x = mean(x), y = mean(y)) %>% 
 #    select(x, y)
 #  ps$H <- as.matrix(holecen)
 #  
 #  tr <- RTriangle::triangulate(ps)
 #  # ## need to remove any triangles that aren't within the branches
  # centroids <- bind_rows(
  #   tv %>% 
  #   inner_join(tXv, c(".vx0" = ".vx1")) %>% dplyr::select_("x", "y", ".vx0", ".tr0"), 
  #   tv %>% 
  #   inner_join(tXv, c(".vx0" = ".vx2")) %>% dplyr::select_("x", "y", ".vx0", ".tr0"), 
  #   tv %>% 
  #   inner_join(tXv, c(".vx0" = ".vx3"))) %>% 
  #   dplyr::select_("x", "y", ".tr0") %>% 
  #   group_by_(".tr0")  %>% arrange_(".tr0") %>% 
  #   distinct(x, y) %>% 
  #   summarize(x = mean(x), y = mean(y)) %>% select_("x", "y", ".tr0")
  # 
  # 
  #   
  # ## use over for now
  #  bad <- is.na(sp::over(SpatialPoints(as.matrix(centroids %>% select(x, y))), grisToSpatialPolygons(x)))
  #  if (any(bad)) {
  #    badtri <- centroids$.tr0[bad]
  #    tXv <- tXv %>% dplyr::filter_(!".tr0" %in% badtri)
  #    oXt <- oXt %>% dplyr::filter_(!".tr0" %in% badtri)
  #  }
  # x$tXv <- tXv
  # x$oXt <- oXt
  # x$v <- tv
  # x$bXv <- bXv
  # x
#}


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
    XY1 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx1")) %>% select(x, y)
    XY2 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx2")) %>% select(x, y)
    XY3 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx3")) %>% select(x, y)
    
 
    polypath(c(XY1$x, XY2$x, XY3$x), c(XY1$y, XY2$y, XY3$y), ...)
    
  }
  NULL
}


grisTri2rgl <- function(x, verts = c("x", "y"), globe = FALSE) {
  if (!length(verts) %in% c(2, 3)) stop("named vertices must be 2- or 3- in length")
  v <- x$v
  v$structural_index <- seq(nrow(v))
  t_3d <- t3d
  t_3d$it <- t(cbind(v$structural_index[match(x$tXv$.vx1, x$v$.vx0)], 
                    v$structural_index[match(x$tXv$.vx2, x$v$.vx0)], 
                    v$structural_index[match(x$tXv$.vx3, x$v$.vx0)]))
  
  o <- x$o %>% dplyr::select_(".ob0")
  o$structural_index <- seq(nrow(o))
  cols <- sample(grey(seq(0, 1, length = nrow(o))), replace = FALSE)
  cols <- cols[(x$tXv %>% inner_join(x$oXt, ".tr0") %>% inner_join(o, ".ob0"))$structural_index]
  
  t_3d$vb <- v[, verts]
  if (ncol(t_3d$vb) == 1L) stop("vertex attributes not found", setdiff(verts, names(v[, verts])))
  if (ncol(t_3d$vb) < 1L) stop("vertex attributes not found", verts)
  if (ncol(t_3d$vb) == 2) t_3d$vb <- cbind(t_3d$vb, z = 0)
  t_3d$vb <- t(cbind(t_3d$vb, w = 1))
  if (globe & length(verts) == 2L) t_3d$vb[1:3, ] <- t(llh2xyz(t(t_3d$vb[1:3, ])))
  t_3d$material$color <- rep(cols, each = 3)
    #rep(sample(grey(seq(0, 1, length = ncol(t_3d$it)))), each = 3)
  t_3d
}

#' @rawNamespace 
#' if ( requireNamespace("rgl", quietly = TRUE)) {
#' importFrom("rgl",  plot3d)
#' }
#' @export
plot3d.gris <- function(x, globe = TRUE, ...) {
  if (requireNamespace("rgl", quietly = TRUE)) {
    gx <- grisTri2rgl(x, globe = globe)
    rgl::plot3d(gx)
  } else {
    ## persp somesuch
    stop("cannot plot in 3d")
  }
}
