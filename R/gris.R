## classes
## domain (extent + projection)
##
## objects
## grids
setOldClass("tbl_df")
setClass("domain", slots = list(extent = "tbl_df", projection = "character"),
         prototype = list(extent = nextent_vertices(), projection = "+proj=ortho +ellps=sphere"))

##setClass("objects", slots = list(objects = "tbl_df", branches = "tbl_df", vertices = "tbl_df"),
setClass("objects", slots = list(vertices = "tbl_df", domain = "domain"),
         prototype = list())

dom <- new("domain", extent = nextent_vertices(v))
obj <- new("objects", domain = dom, vertices = v)
## methods
## plot
vertices_default <- function(object) {
  slot(object, "vertices")
}
nextent_vertices <- function(object) {
  x <- object
  if (missing(x)) return(data_frame())
  ignore <- c("bid__00", "oid__00", "id")
  for (i in seq_along(ignore)) x <- x %>% select(-matches(ignore[i]))
  bind_rows(x %>%  summarise_each(funs(min)),
            x %>%  summarise_each(funs(max)))
}
nextent_objects <- function(object) nextent_vertices(vertices(object))

nextent_raster <- function(object) {
  x <- object
  dm <- dim(x)
  ## yikes, data_frame evaluates in context
  yex <- c(ymin(x), ymax(x))
  nex <- data_frame(x = c(xmin(x), xmax(x)), y = yex)
  if (length(dm) > 2) nex$z <- range(getZ(x))
  if (length(dm) > 3) stop("greater than 3D not yet supported")
  nex
}

if (!isGeneric("nextent")) setGeneric("nextent", function(object) standardGeneric("nextent"))
if (!isGeneric("vertices")) setGeneric("vertices", function(object) standardGeneric("vertices"))
setMethod("vertices", "objects", vertices_default)
setMethod("nextent", "objects", nextent_objects)



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
  uoid <- unique(x$oid__00)
  if (is.null(col)) col <- sample(grey(seq_along(uoid)/length(uoid)))
  col <- rep(col, length(uoid))
  for (i in seq(length(uoid))) {
    asub <- x %>% filter(oid__00 == uoid[i]) %>% select(x, y, oid__00, bid__00)
    if (type == "p") mpolypath(asub, g = asub$bid__00, col = col[i], rule = "evenodd", ...)
    if (type == "l") mlinepath(asub, g = asub$bid__00, col = col[i])
    }
}

dv <- function(x, ...) {
  g <- geometry(x)
  d <- as.data.frame(x)
  x <- NULL
  for (i in seq(nrow(d))) {
    l <- do.call(bind_rows, lapply(seq_along(g@polygons[[i]]@Polygons), function(xi) {
      m <- g@polygons[[i]]@Polygons[[xi]]@coords
      data_frame(x = m[,1], y = m[,2], bid__00 = xi)
    }))
    l$oid__00 <- i
    x <- bind_rows(x, l)
  }
  x$id <- seq(nrow(x))
  x
}

