library(rbenchmark)
library(gris)
library(maptools)
data(wrld_simpl)
dd <- wrld_simpl
library(rworldxtra)
data(countriesHigh)
#dd <- countriesHigh

g <- gris(dd)
graphics.off()
cols <- sample(grey(seq(0, 1, length = nrow(wrld_simpl))))
benchmark(sp = {plot(dd, col = cols, axes = TRUE);graphics.off()}, 
          gris = {plot(g, col = cols, asp = 1.118039,  xlab = "", ylab = "");graphics.off()}, 
          replications = 10)



library(profr)
a <- profr({
  system.time({
plot(g$v$x, g$v$y, type = "n", asp = 1/cos(-30 * pi/180))
dd <- data_frame(x = NA_real_, y = NA_real_) ##, .br0 = NA_real_)
a <- g$o %>% select(.ob0) %>% inner_join(g$b, '.ob0') %>% inner_join(g$bXv, '.br0') %>% inner_join(g$v, ".vx0") %>% select(x, y, .br0, .ob0)
uid <- g$o$.ob0
for (i in seq_along(uid))  {
 # g$o[i,] %>% select(.ob0) %>% inner_join(g$b, '.ob0') %>% inner_join(g$bXv, '.br0') %>% inner_join(g$v, ".vx0") %>% 
#    group_by(.br0) %>% select(x, y, .br0) %>% do(bind_rows(., dd)) %>% mutate(n = row_number()) %>% filter(n < max(n)) %>% polypath(col = cl[i])
  
  #a <- g$o[i,] %>% select(.ob0) %>% inner_join(g$b, '.ob0') %>% inner_join(g$bXv, '.br0') %>% inner_join(g$v, ".vx0") %>% select(x, y, .br0)
  aa <- a[a$.ob0 == uid[i], ]
  xx <- do.call(bind_rows, lapply(split(aa[, c("x", "y")], aa$.br0), function(x) bind_rows(x, dd)))
  polypath(xx[-nrow(xx), ], col = cl[i])
}
})
  
})
