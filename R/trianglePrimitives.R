# library(gris)
# library(maptools)
# data(wrld_simpl)
# system.time(g <- gris(subset(wrld_simpl, NAME == "Australia")))
#  library(RTriangle)
#  TR <- NULL
#  maxtr <- 0
# 
#  brid <- unique(g$b$.br0)
# for (i in seq_along(brid)) {
#   id <- brid[i]
#   bXv <- g$bXv %>% filter(.br0 == id) %>% inner_join(g$v, ".vx0") %>% select(x, y, .vx0, .br0 )
#   
#   ps <- pslg(P = bXv %>%  dplyr::select(x, y) %>% as.matrix(), S = gris:::prs1(seq(nrow(bXv))))
#   tr <- RTriangle::triangulate(ps)
#   bXt <- data_frame(.br0 = rep(id, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
#   
#   TR <- bind_rows(TR, data_frame(.tr0 = rep(bXt$.tr0, each = 3), .vx0 = bXv$.vx0[t(tr$T)]))
#   # 
#     maxtr <- nrow(TR)
#   
#   
# }
#  
#  g$TR <- TR
#  g$bXt <- bXt
#  
#  
#  ## get out the triangles
#  v <- g$v %>% select(x, y) %>% cbind(0) %>% as.matrix #g$TR %>% inner_join(g$v, ".vx0") %>% select(x, y) %>% cbind(0) %>% as.matrix
#  library(rgl)
#  triangles3d(v[TR$.vx0[1:3], ])
#  
#  
#  
#  
#  
 
 
 
 
 
 
 
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
# ## gris version of disaggregate
# system.time({
# g$b$.ob0 <- seq(nrow(g$b))
# g$o <- g$o[rep(1, nrow(g$b)), ]
# g$o$.ob0 <- g$b$.ob0
# g$bXv
# g$b
# library(RTriangle)
# 
# 
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
# })
