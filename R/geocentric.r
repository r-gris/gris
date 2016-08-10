#' Geocentric coordinates. 
#' 
#' Transform to geocentric coordinates. 
#'
#' @param x table of long, lat, and optionally "height"
#' @param crs 
#' @param exag exaggeration to apply to height values (added to radius)
#'
#' @return tibble
#' @export
geocentric <- function(x, 
                    crs = "+proj=geocent +ellps=WGS84") {
  as_tibble(proj4::ptransform(x, "+proj=longlat +ellps=WGS84", 
                    crs))
}
