
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
pl <- function(x, col = NULL, debug = FALSE, asp = NULL,  ..., type = "p") {
  plot(dplyr::select(x, x, y), type = "n", asp = asp)
  uoid <- unique(x$.ob0)
  if (is.null(col)) col <- sample(grey(seq_along(uoid)/length(uoid)))
  col <- rep(col, length(uoid))
  for (i in seq(length(uoid))) {
    asub <- x %>% filter(.ob0 == uoid[i]) %>% select(x, y, .ob0, .br0)
    if (type == "p") mpolypath(asub, g = asub$.br0, col = col[i], rule = "evenodd", ...)
    if (type == "l") mlinepath(asub, g = asub$.br0, col = col[i])
    }
}

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

build <- function(x, ...) {
  g <- geometry(x)
  d <- as.data.frame(x)
  x <- vector("list", nrow(d))
  for (i in seq(nrow(d))) {
    l <- do.call(bind_rows, lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      data_frame(x = m[,1], y = m[,2], .br0 = xi)
    }))
    if (i > 1) l$.br0 <- l$.br0 + max(x[[i-1]]$.br0)
    l$.ob0 <- i
    x[[i]] <- l
  }
  x <- do.call(bind_rows, x)
  x$id <- seq(nrow(x))
  x
}
