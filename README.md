# gris


## Examples

Build objects from other packages. 

```{r}
library(maptools)
data(wrld_simpl)
dat <- bld(wrld_simpl)
dat1 <- sbs(dat, filter(dat$o, NAME %in% c("Australia", "Indonesia")))
pl(dat1$v)
```

Triangulate with CGAL via [cgalgris](https://github.com/mdsumner/cgalgris). 

```{r}
library(cgalgris)
## Delaunay triangulation (unconstrained)
dat$vi <- tri_xy(dat$v$x, dat$v$y) + 1  ## plus 1 for R indexing

## centroids of triangles
centr <- data_frame(x = x$v$x[x$vi], y = x$v$y[x$vi], t = rep(seq(length(x$vi/3)), each = 3)) %>% group_by(t) %>% summarize(x = mean(x), y = mean(y))
   x1 <- ab %>% mutate(mg = .br0) %>%  group_by(mg) %>% do(rbind(., NA_real_))
   inside <- which(point.in.polygon(centr$x, centr$y, x1$x, x1$y) == 1)
   col <- sample(cols, 1)
   apply(del[inside, ], 1, function(x) polypath(cbind(ab$x[x], ab$y[x]), col = col, border = NA))


Build up the objects from scratch. 

```{r}
library(gris)

## one object, two branches
v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), .br0 = 1, .ob0 = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), .br0 = 2, .ob0 = 1)

## another object two branches
v3 <- v1 %>% mutate(x = x + 2, .br0 = 4, .ob0 = 2)
v4 <- v2 %>% mutate(x = x + 2, .br0 = 5, .ob0 = 2)
## third branch in first  object
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), .br0 = 3, .ob0 = 1)
v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(id = seq(n()))

## plot with two colours
pl(v, col = c("lightgrey", "darkgrey"))

## build a composite with data attributes on the individual objects
b <- v %>% distinct(.br0) %>% select(.br0, .ob0)
o <- b %>% distinct(.ob0) %>% mutate(id = .ob0) %>% select(id)
o$Name <- c("p", "q")
##v <- v %>% select(-.ob0)

x <- list(v = v, b = b, o = o)

## subset by name
dq <- sbs(x, filter(x$o, Name == "q"))
pl(dq$v, col = "green")
```



