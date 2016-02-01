## adapted from raster package R/geom.R
.polysGeom <-   function(x, sepNA=FALSE, ...) {
            
            nobs <- length(x@polygons)
            objlist <- list()
            cnt <- 0
            if (sepNA) {
              sep <- rep(NA,5)
              for (i in 1:nobs) {
                nsubobs <- length(x@polygons[[i]]@Polygons)
                ps <- lapply(1:nsubobs, 
                             function(j)
                               rbind(cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords), sep)
                )
                objlist[[i]] <- cbind(i, do.call(rbind, ps))
                cnt <- cnt+nsubobs
              }
            } else {
              for (i in 1:nobs) {
                nsubobs <- length(x@polygons[[i]]@Polygons)
                ps <- lapply(1:nsubobs, 
                             function(j) 
                               cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords)
                )
                objlist[[i]] <- cbind(i, do.call(rbind, ps))
                cnt <- cnt+nsubobs
              }
            }
            
            obs <- do.call(rbind, objlist)
            colnames(obs) <- c('object', 'part', 'cump', 'hole', 'x', 'y')
            rownames(obs) <- NULL
            
            if (sepNA) {
              obs[is.na(obs[,2]), ] <- NA
            }
            return( obs )
          }




.linesGeom <-  function(x, sepNA=FALSE, ...) {
            
            nobs <- length(x@lines)
            objlist <- list()
            cnt <- 0
            if (sepNA) {
              sep <- rep(NA, 4)
              for (i in 1:nobs) {
                nsubobj <- length(x@lines[[i]]@Lines)
                ps <- lapply(1:nsubobj, 
                             function(j) 
                               rbind(cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords), sep)
                )
                objlist[[i]] <- cbind(i, do.call(rbind, ps))
                cnt <- cnt+nsubobj
              }
            } else {
              for (i in 1:nobs) {
                nsubobj <- length(x@lines[[i]]@Lines)
                ps <- lapply(1:nsubobj, function(j) cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords))
                objlist[[i]] <- cbind(i, do.call(rbind, ps))
                cnt <- cnt+nsubobj
              }
            }
            obs <- do.call(rbind, objlist)
            colnames(obs) <- c('object', 'part', 'cump', 'x', 'y')
            rownames(obs) <- NULL
            
            if (sepNA) {
              obs[is.na(obs[,2]), ] <- NA
            }
            return (obs)
          }



.pointsGeom <-  function(x, ...) {
            xy <- coordinates(x)
            ##xy <- cbind(1:nrow(xy), xy)
            if (is.list(x@coords)) {
              br <- rep(seq_along(x@coords), unlist(lapply(x@coords, nrow)))
             
            } else {
              br <- seq(nrow(xy))
           
             
            }
            xy <- cbind(br, br, xy)
            colnames(xy) <- c('cump', 'object', 'x', 'y')
            return(xy)
          }


#  older method, was much slower and not working for MultiPoints
# exall <- function(x) {  
#   g <- sp::geometry(x)
#   proj <- proj4string(x)
#   if (inherits(x, "SpatialPoints"))
#     mcoords <- coordinates(g)
#   xx <- vector("list", nrow(x))
#   for (i in seq_along(x)) {
#     if (inherits(x, "SpatialPolygons"))
#       rawcoords <-
#         lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
#           m <- head(g@polygons[[i]]@Polygons[[xi]]@coords,-1)
#           dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
#         })
#     
#     if (inherits(x, "SpatialLines"))
#       rawcoords <- lapply(seq_along(g@lines[[i]]@Lines), function(xi) {
#         m <- g@lines[[i]]@Lines[[xi]]@coords
#         dplyr::data_frame(x = m[,1], y = m[,2], .br0 = xi)
#       })
#     ## obviously this could be much faster without the loop
#     if (inherits(x, "SpatialPoints"))
#       rawcoords <-
#         list(dplyr::data_frame(x = mcoords[i,1], y = mcoords[i,2], .br0 = i))
#     
#     ## d$nbranches[i] <- length(rawcoords)
#     l <- do.call(bind_rows, rawcoords)
#     if (i > 1)
#       l$.br0 <- l$.br0 + max(xx[[i - 1]]$.br0)
#     l <- l %>% dplyr::mutate(.ob0 = i)
#     xx[[i]] <- l
#   }
#   do.call(bind_rows, xx) %>% mutate(.vx0 = row_number())
# }