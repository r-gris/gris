source("R/gris.R")

v <- bld(brokeCountries)
o <- as_data_frame(bind_cols(data_frame(object = unique(d$.ob0)), as_data_frame(as.list(as.data.frame(brokeCountries)[,4, drop = FALSE]))))
obj <- list(vert = v, obj = o)
class(obj) <- c("gris", "list")
subset.gris <- function(x, subset, ...) {
  e <- substitute(subset)
  r <- eval(e, x, parent.frame())
  d <- x$obj %>% filter(r)
  pl(left_join(d, x$vert, by = c("object" = ".ob0"))   %>% mutate(.ob0 = object))
  NULL
}
