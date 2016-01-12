
library(rworldmap)
data(countriesLow)

coun <- c("Indonesia", "China", "Mongolia")
xn <- gris(countriesLow, normalize_verts = TRUE, triangulate = FALSE)
x1 <- xn[xn$o$SOVEREIGNT %in% coun, ]

xo <- gris(countriesLow, normalize_verts = FALSE, triangulate = FALSE)
x2 <- xo[xo$o$SOVEREIGNT %in% coun, ]

xnn <- gris(subset(countriesLow, SOVEREIGNT %in% coun), normalize_verts = TRUE, triangulate = TRUE)
xon <- gris(subset(countriesLow, SOVEREIGNT %in% coun), normalize_verts = FALSE, triangulate = FALSE)

nearto <- function(x, target, thresh = 1) abs(target - x) < thresh

x1$v %>% mutate(near = nearto(x, 116.7606, thresh = 0.005)) %>% filter(near)
x2$v %>% mutate(near = nearto(x, 116.7606, thresh = 0.005)) %>% filter(near)



g <- gris(wrld_simpl, normalize_verts = FALSE, triangulate = FALSE)
v <- g$v %>% inner_join(g$bXv)
v$branchorder <- unlist(lapply(group_size(group_by(v, .br0)), seq))

vv <- g$v %>% distinct(x, y)
bXv <- vv %>% transmute(x, y, new = .vx0) %>% inner_join(v) %>% transmute(.vx0 = new, .br0, branchorder) #%>% arrange(.br0, branchorder)
g$bXv <- bXv
g$v <- v %>% select(-.br0, -branchorder)

plot(g)

## this is more like it
library(gris)
library(dplyr)
## one object, two branches
v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), .br0 = 1, .ob0 = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), .br0 = 2, .ob0 = 1)

## another object two branches
v3 <- v1 %>% mutate(x = x + 2, .br0 = 4, .ob0 = 2)
v4 <- v2 %>% mutate(x = x + 2, .br0 = 5, .ob0 = 2)
## third branch in first  object
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), .br0 = 3, .ob0 = 1)
v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(.vx0 = seq(n())) 


## build a composite with data attributes on the individual objects
b <- v %>% distinct(.br0) %>% select(.br0, .ob0)
o <- b %>% distinct(.ob0) %>% select(-.br0)
o$Name <- c("p", "q")
v <- v %>% select(-.ob0)

vv <- v %>% distinct(x, y)

bXv <- vv %>% transmute(x, y, new = .vx0) %>% inner_join(v) %>% transmute(.vx0 = new, .br0)
v$.br0 <- NULL

vx <- list(v = v, b = b, o = o, bXv = bXv)
class(vx) <- c("gris", "list")

## subset by name
dq <- sbs(x, filter(x$o, Name == "q"))
pl(dq$v, col = "green")