# g <- gris(subset(wrld_simpl, NAME == "Australia"))
# library(RTriangle)
# TR <- NULL
# maxtr <- 0
# ## triangulate each piece
# ## this needs to happen per branch, but mkpsg code will need to come out to that level (not object level)
# for (i in seq(nrow(g$b))) {
#   g1 <- g[i, ]
#   
#   tr <- RTriangle::triangulate(mkpslg(g1))
#   bXt <- data_frame(.br0 = rep(g1$b$.br0, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
#   TR <- bind_rows(TR, data_frame(.tr0 = rep(bXt$.tr0, each = 3), .vx0 = g1$v$.vx0[t(tr$T)]))
#   
#   maxtr <- nrow(TR)
# }
# 
# 
# 
# ## gris version of disaggregate
# 
# g$b$.ob0 <- seq(nrow(g$b))
# g$o <- g$o[rep(1, nrow(g$b)), ]
# g$o$.ob0 <- g$b$.ob0
# g$bXv
# g$b
# library(RTriangle)
# TR <- NULL
# maxtr <- 0
# ## triangulate each piece
# for (i in seq(nrow(g$o))) {
#   g1 <- g[i, ]
# 
#   tr <- RTriangle::triangulate(mkpslg(g1))
#   bXt <- data_frame(.br0 = rep(g1$b$.br0, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
#   TR <- bind_rows(TR, data_frame(.tr0 = rep(bXt$.tr0, each = 3), .vx0 = g1$v$.vx0[t(tr$T)]))
# 
#   maxtr <- nrow(TR)
# }