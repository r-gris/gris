library(dplyr)
source("R/gris.R")
v1 <- data_frame(x = c(0, 1, 0.5), y = c(0, 0, 1), .br0 = 1, .ob0 = 1)
v2 <- data_frame(x = c(1, 1, 0.5), y = c(0, 1, 1), .br0 = 2, .ob0 = 1)

v3 <- v1 %>% mutate(x = x + 2, .br0 = 4, .ob0 = 2)
v4 <- v2 %>% mutate(x = x + 2, .br0 = 5, .ob0 = 2)
v0 <- data_frame(x = c(0.1, 0.4, 0.2), y = c(0.05, 0.05, 0.12), .br0 = 3, .ob0 = 1)

v <- bind_rows(v1,  v2, v0,  v3, v4) %>% mutate(id = seq(n()))

b <- v %>% distinct(.br0) %>% select(.br0, .ob0)
o <- b %>% distinct(.ob0) %>% mutate(id = .ob0) %>% select(id)
o$Name <- c("p", "q")

v <- v %>% select(-.ob0)

## subset by name
o %>% filter(Name == "p") %>% inner_join(b, by = c(id = ".ob0")) %>% inner_join(v, by = c(.br0 = ".br0"))

sbs <- function(x, subset, ...) {
  o <- subset
  b <- x$b %>% semi_join(o, by = c(".ob0" = "id"))
  v <- x$v %>%  semi_join(b, by = c(".br0" = ".br0"))
  list(o = o, b = b, v = v)
}

sbs(list(o = o, b = b, v = v), filter(o, Name == "p"))

dpc <- build(brokeCountries)
b <- dpc %>% distinct(.br0) %>% select(.br0, .ob0)
o <- b %>% distinct(.ob0) %>% mutate(id = .ob0) %>% select(id)
df <- as.data.frame(brokeCountries)

for (i in 1:ncol(df)) o[[names(df)[i]]] <- df[[i]]
## de-attribute these broken levels attributes see https://github.com/hadley/dplyr/issues/859
o[] <- lapply(o, function(x) {if(isTRUE(all.equal(attr(x, 'levels'), character(0)))) {attr(x, 'levels')  <- NULL}; x})
##o$ScaleRank <- o$LabelRank <- o$ADM0_DIF <- NULL
sbs(list(o = o, b = b, v = dpc), filter(o, SOVEREIGNT == "Australia"))


