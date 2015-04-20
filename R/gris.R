
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
  uoid <- unique(x$oid)
  if (is.null(col)) col <- sample(grey(seq_along(uoid)/length(uoid)))
  col <- rep(col, length(uoid))
  for (i in seq(length(uoid))) {
    asub <- x %>% filter(oid == uoid[i])
    if (type == "p") mpolypath(asub, g = asub$oid, col = col[i], rule = "evenodd", ...)
    if (type == "l") mlinepath(asub, g = asub$oid, col = col[i])
    }
}

dv <- function(x, ...) {
  g <- geometry(x)
  d <- as.data.frame(x)
  x <- NULL
  for (i in seq(nrow(d))) {
    l <- do.call(bind_rows, lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      data_frame(x = m[,1], y = m[,2], bid = xi)
    }))
    l$oid <- i
    x <- bind_rows(x, l)
  }
  x$id <- seq(nrow(x))
  x
}

