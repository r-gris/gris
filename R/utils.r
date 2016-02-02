v3ToLong <- function(x) data_frame(.vx0 = as.vector(t(as.matrix(x[,1:3]))), .tr0 = rep(x$.tr0, each = 3))
tricentroids <- function(x) {
  x$tXv %>% dplyr::select(.vx1, .vx2, .vx3, .tr0) %>% v3ToLong %>% inner_join(gt$v) %>% 
    group_by(.tr0) %>% summarize(x = mean(x), y = mean(y)) %>% dplyr::select(x, y, .tr0)
}

tribbox <- function(x) {
  x$tXv %>% dplyr::select(.vx1, .vx2, .vx3, .tr0) %>% v3ToLong %>% inner_join(gt$v) %>% 
    group_by(.tr0) %>% summarize(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y))
}

vcrossp <- function( a, b ) {
  result <- matrix( NA, nrow( a ), 3 )
  result[,1] <- a[,2] * b[,3] - a[,3] * b[,2]
  result[,2] <- a[,3] * b[,1] - a[,1] * b[,3]
  result[,3] <- a[,1] * b[,2] - a[,2] * b[,1]
  result
}
