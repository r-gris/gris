## maps
m1 <- c("county", "france", "italy", "nz", "state", "state.carto", 
        "state.vbm", "usa", "world", "world2")

## mapdata
m2 <- c("china", "japan", "nzHires", "rivers", "world2Hires", "worldHires")

library(maps)
library(mapdata)
assessMap <- function(x, name = "") {
  nax <- sum(is.na(x$x)) 
  nay <- sum(is.na(x$y)) 
  nnm <- length(x$names)
  data.frame(nax = nax, nay = nay, nnm = nnm, name = name, sensible = (nax == nay) & nax == nnm - 1)
}

do.call(rbind, lapply(c(m1, m2), function(x) assessMap(map(x, plot = FALSE), x)))

x <- map("county", plot = FALSE)
na <- is.na(x$x)
segs <- c(0, cumsum(abs(diff(is.na(x$x)))))
clist <- split(data.frame(x$x, x$y)[!na, ], segs[!na])
#lapply(seq_along(clist), function(xx) points(clist[[xx]], pch = ".", col = "red"))
