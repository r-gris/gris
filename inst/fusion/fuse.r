library(maptools)
data(wrld_simpl)
library(raster)
library(gris)
m <- subset(wrld_simpl, NAME == "Australia", select = "NAME")
r <- raster(m, nrow = 15, ncol = 25)
p <- as(r, "SpatialPolygonsDataFrame")
p$NAME <- "grid"
p$layer <- NULL
row.names(m)
plot(p)
plot(m, add = TRUE)

## wtf
#' Combine a list of PolygonDataFrame objects into a single object. 
#' 
#' Objects are combined by modifying sequential IDs to increase. 
c_poly <- function(x) {
  poly <- x[[1]]
  if (length(x) == 1L) {
    warning("input of length one, returning first element")
    return(poly)
  }
  for (i in seq_along(x)[-1]) {
    x0 <- spChFIDs(x[[i]], as.character(seq(nrow(x[[i]])) + max(as.integer(row.names(poly)))))
    poly <- maptools::spRbind(poly, x0)
  }
  poly
}

pp <- c_poly(list(p, m))

## something like this

gg <- gris(pp)
gt <- triangulate(gg)
