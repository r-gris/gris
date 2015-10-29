library(gris) ## devtools::install_github("mdsumner/gris")
library(graticule) ## devtools::install_github("mdsumner/graticule")
library(rbokeh)
library(maptools)
data(wrld_simpl)

gworld <- gris:::sp2map(subset(wrld_simpl, coordinates(wrld_simpl)[,2] < 0))
library(readr)
#voyage0 <- read_csv("201213001.csv")
# save(voyage0, file = "voyage0.RData")
#load("voyage0.RData")
#voyage <- voyage0[seq(1, nrow(voyage0), length = 5e3), ]
#save(voyage, file = "voyage.RData")
load("voyage.RData")

figure(width = 800, padding_factor = 0) %>%
  ly_map(gworld, col = "gray") %>%
  ly_points(LONGITUDE_DEGEAST, LATITUDE_DEGNORTH, data = voyage, size = 1,
            hover = c(WND_SPD_PORT_CORR_KNOT))




## projection
prj <- "+proj=stere +lat_ts=-71 +lat_0=-90 +lon_0=147 +ellps=WGS84"
xs <- seq(-180, 180, by = 15)
ys <- seq(-85, 15, by = 10)
grat <- gris:::sp2map(graticule(xs, ys, proj = prj))
gratlabs <- graticule_labels(xs, ys, xline = 50, yline = -30, proj = prj)
pworld <- gworld
xy <- rgdal::project(cbind(pworld$x, pworld$y), prj)
pworld$x <- xy[,1]
pworld$y <- xy[,2]
xy <- rgdal::project(cbind(voyage$LONGITUDE_DEGEAST, voyage$LATITUDE_DEGNORTH), prj)

voyage$X <-  xy[,1]
voyage$Y <- xy[,2]

gratlabs$xx <- coordinates(gratlabs)[,1]
gratlabs$yy <- coordinates(gratlabs)[,2]
gratlabs$lab2 <- gsub("degree", "", gsub("\\*", "", gratlabs$lab))
gratlabs <- as.data.frame(gratlabs)
figure(width = 800, padding_factor = 0, xlim = range(pworld$x, na.rm = TRUE), 
       ylim = range(pworld$x, na.rm = TRUE), xgrid = FALSE, ygrid = FALSE, xaxes = FALSE, yaxes = FALSE) %>%
  ly_map(pworld, col = "gray") %>%
  ly_map(grat, fill_alpha = 0) %>% 
  ##ly_text(gratlabs$x, gratlabs$y,  text = gratlabs$lab2) %>% 
  ly_text(xx, yy, text = lab2, data = gratlabs,
          font_style = "bold", font_size = "15pt",
          align = "left", baseline = "middle") %>%
  ly_points(X, Y, data = voyage, size = 1,
            hover = c(WND_SPD_PORT_CORR_KNOT))

