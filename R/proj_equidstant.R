#' Projection for the area of interest with equidistant projection
#'
#' Auto selecting equidistant projections based on the geological shape and projection characteristics. Function will show messages of the basis how the projection is selected.
#' @param obj Input geo data, should be one of:\cr
#'  - An object can be accepted by [sf::st_bbox()] to compute the bounding box\cr
#'  - A named list of named list of longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#' @param output_type A string for expected output, either "proj4" or "WKT"
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A `proj4` or `WKT` string
#' @export
#'
#' @examples proj_equal_area(sf::st_as_sf(quakes,coords = c("long","lat"),crs = 4326))
proj_equidstant <- function(obj,output_type = "proj4",datum = "WGS84", unit = "m") {
  if (!(is.vector(obj) & identical(sort(names(obj)), sort(c("xmin", "xmax", "ymin","ymax"))))) {
    if(!sf::st_is_longlat(obj)) {
      obj = sf::st_transform(obj, 4326)
    }
    obj = sf::st_bbox(obj)
  }
  lonmax = obj[["xmax"]]
  lonmin = obj[["xmin"]]
  latmax = obj[["ymax"]]
  latmin = obj[["ymin"]]
  if (lonmin+270 < lonmax) {
    lonmax = obj[["xmin"]]
    lonmin = obj[["xmax"]]
  }
  if (lonmin > 180 | lonmin < -180 |
      lonmax > 180 | lonmax < -180 |
      latmin > 90 | latmin < -90 |
      latmax > 90 | latmax < -90) {
    stop("Please input valid extent!")
  }
  # computing longitude extent
  dlon0 <- abs(lonmax - lonmin)
  dlon <- ifelse(dlon0 > 180, 360-dlon0, dlon0)
  if (lonmax < lonmin) {
    temp_mid = (lonmax + 360 + lonmin) / 2
    mid_lon = ifelse(temp_mid < 180, temp_mid, temp_mid-360)
  } else {
    mid_lon = (lonmax + lonmin) / 2
  }
  center <- list(lng = mid_lon, lat = (latmax + latmin) / 2)
  scale <- 720 / (lonmax - lonmin) / (sin(latmax * pi / 180) - sin(latmin * pi / 180))



  # reading central meridian - Assuming outputLON is defined elsewhere
  lng <- center$lng
  # getting the height-to-width ratio
  ratio <- (latmax - latmin) / dlon
  if (latmin > 0.0) {
    ratio <- ratio / cos(latmin * pi / 180)
  } else if (latmax < 0.0) {
    ratio <- ratio / cos(latmax * pi / 180)
  }
  if (center$lat > 70) {
    message("## Close to poles")
    # case: close to poles
    outputTEXT <- stringLinks("aeqd", NaN, 90.0, NaN, NaN, center$lng, NaN, datum, unit)
  } else if (center$lat < -70) {
    message("## Close to poles")
    outputTEXT <- stringLinks("aeqd", NaN, -90.0, NaN, NaN, center$lng, NaN, datum, unit)
  } else if (ratio > 1.25) {
    message("## North-south extent")
    # case: with an north-south extent
    outputTEXT <- stringLinks("cass", NaN, NaN, NaN, NaN, center$lng, NaN, datum, unit)
  } else if (abs(center$lat) < 15) {
    message("## Close to equator")
    # case: close to equator
    if ((latmax * latmin) <= 0 ) {
      message("## Extent is touching or crossing equator")
      latS = max(abs(latmax), abs(latmin)) / 2
    } else{
      message("## Extent is not crossing equator")
      latS = center$lat
    }
    outputTEXT <- stringLinks("eqc", NaN, NaN, latS, NaN, center$lng, NaN, datum, unit)
  } else {
    message("## Mid-Latitude away from pole and equator")
    # case: between pole and equator
    # computing standard paralles
    interval <- (latmax - latmin) / 6
    # Oblique azimuthal equidistant
    message("## Select Oblique azimuthal equidistant projection")
    outputTEXT <- stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN, datum, unit)
    # outputTEXT <- list(
    #   "Equidistant conic" = stringLinks("eqdc", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN),
    #   "Oblique azimuthal equidistant" = stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN)
    # )
  }
  if (requireNamespace("geosphere",quietly = TRUE)) {
    p1 = matrix(
      c(lonmin, latmin,lonmin, latmin,lonmax, latmax,lonmax, latmax),
      ncol = 2, byrow = TRUE
    )
    p2 =  matrix(
      c(lonmax, latmin,lonmin, latmax, lonmin, latmax, lonmax,latmin),
      ncol = 2, byrow = TRUE
    )
    if (max(geosphere::distHaversine(p1,p2)) / 1e6 < 1) {
      message("## The map extent is not quite large, you could try official projection, as well as other projections like 'laea' or 'stere', which won't produce much error at this scale")
    }
  }
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }

}


