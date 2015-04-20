library(dplyr)

v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), bid = 1, oid = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), bid = 2, oid = 1)

v3 <- v1 %>% mutate(x = x + 2, bid = 4, oid = 2)
v4 <- v2 %>% mutate(x = x + 2, bid = 5, oid = 2)
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), bid = 3, oid = 1)

v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(id = seq(n()))
##ts <- pl(v, debug = TRUE)
b <- data_frame
pl(v, col = c("grey", "aliceblue"))

pl(v, col = c("black", "red"), type = "l", lwd = c(3, 5))

library(rworldmap)
data(countriesLow)

load("inst/extdata/brokeCountries")
dpc <- dv(brokeCountries)

system.time({
  prj <- "+proj=laea +lat_0=-90"
  xy <- project(as.matrix(dpc[,1:2]), prj)
  dpc$x <- xy[,1]
  dpc$y <- xy[,2]
  pl(dpc, asp = 1)
})

system.time(plot(spTransform(brokeCountries, CRS(prj)), col = grey(seq(0, 1, length = nrow(brokeCountries)))))


