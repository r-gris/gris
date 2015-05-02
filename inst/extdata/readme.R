
library(raster)
library(rworldmap)
data(countriesLow)
## cut a hole in Australia
##e <- drawPoly()
library(rgeos)
el <- SpatialPolygonsDataFrame(e, data.frame(x = 1))
##b <- gDifference(countriesLow, el)
brokeCountries  <- SpatialPolygonsDataFrame(gDifference(countriesLow, el, byid = TRUE), as.data.frame(countriesLow), match.ID = FALSE)

save(brokeCountries, file = "inst/extdata/brokeCountries.Rdata")
