
#' @importFrom dplyr data_frame select summarise_each funs bind_rows
nextent_vertices <- function(object) {

  if (missing(object)) return(data_frame())
  x <- object
  ignore <- c("bid__00", "oid__00", "id")
  for (i in seq_along(ignore)) x <- x %>% select(-matches(ignore[i]))
  bind_rows(x %>%  summarise_each(funs(min)),
            x %>%  summarise_each(funs(max)))
}
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

#dom <- new("domain", extent = nextent_vertices(v))
#obj <- new("objects", domain = dom, vertices = v)
## methods
## plot
vertices_default <- function(object) {
  slot(object, "vertices")
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

