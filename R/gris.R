library(dplyr)

v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), bid = 1, oid = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), bid = 2, oid = 1)

v3 <- v1 %>% mutate(x = x + 2, oid = 2)
v4 <- v2 %>% mutate(x = x + 2, oid = 2)
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), bid = 3, oid = 1)

v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(id = seq(n()))

##' x is a tbl_df
mpolypath <- function(x, mg = 1, ...) {

}

pl <- function(x, col = NULL, debug = FALSE) {
  plot(select(x, x, y), type = "n")
  uoid <- unique(x$oid)
  if (is.null(col)) col <- grey(seq_along(uoid)/length(uoid))
  col <- rep(col, length(uoid))
  for (i in seq(length(uoid))) {
    asub <- x %>% filter(oid == uoid[i]) %>% group_by(bid) %>% do(rbind(., NA_real_))
    polypath(asub[-nrow(asub), ], col = col[i], rule = "evenodd")
  }
}

ts <- pl(v, debug = TRUE)

pl(v, col = c("grey", "aliceblue"))


library(rgdal)

