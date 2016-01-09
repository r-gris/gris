#  library(gris)
#  library(maptools)
#  data(wrld_simpl)
#  gall <- gris(wrld_simpl)
#  g <- gall[gall$o$NAME %in% c("Australia", "New Zealand"), ]
#  
#   library(RTriangle)
#   tXv <-  oXt <- NULL
#   maxtr <- 0
#   
#  # tXv - table with .vx1, .vx2, .vx3 and .tr0
# #  oXt - table with .tr0 and .ob0
# 
#  oid <- unique(g$o$.ob0)
#  for (i in seq_along(oid)) {
#    id <- oid[i]
#    ## we triangulate each branch individually, so we can keep track of them
#    ##bXv <- g$bXv %>% filter(.br0 == id) %>% inner_join(g$v, ".vx0") %>% select(x, y, .vx0, .br0 )
#    g0 <- g[g$o$.ob0 == id, ]
#   ps <- mkpslg(g0)
#    #ps <- pslg(P = bXv %>%  dplyr::select(x, y) %>% as.matrix(), S = gris:::prs1(seq(nrow(bXv))))
#    tr <- RTriangle::triangulate(ps)
#    #bXt <- data_frame(.br0 = rep(id, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
#    oX <- data_frame(.ob0 = rep(id, nrow(tr$T)), .tr0 = seq(nrow(tr$T)) + maxtr)
#    oXt <- bind_rows(oXt, oX)
#    
#    tX <- data_frame(.vx1 = g$v$.vx0[tr$T[,1]], .vx2 = g$v$.vx0[tr$T[,2]], .vx3 = g$v$.vx0[tr$T[,3]], 
#                     .tr0 = oX$.tr0)
#    
#   tXv <- bind_rows(tXv, tX)
#      maxtr <- maxtr + nrow(tr$T)
# 
# 
#  }
# 
#  
#  plotT <- function(tXv, v) {
#    for (i in seq(nrow(tXv))) {
#      XY1 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx1")) %>% select(x, y)
#      XY2 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx2")) %>% select(x, y)
#      XY3 <- v  %>% inner_join(tXv[i, ], c(".vx0" = ".vx3")) %>% select(x, y)
#      
#      
#      polypath(c(XY1$x, XY2$x, XY3$x), c(XY1$y, XY2$y, XY3$y))
#      
#    }
#    NULL
#  }
#  