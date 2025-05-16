#' Funcion for regional maps with an east-west extent
#'
#' @param property Projection property, should be one of "Equal area", "Conformal" and "Equidistant".
#' @param center A list with with numerical values named with `lng` & `lat` element, which are the median longitude and latitude values
#' @param latmax A numerical value, max latitude of the map extent
#' @param latmin A numerical value, min latitude of the map extent
#' @param dlon A numerical value, Longitudal extent of the region
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A list of strings named with `PROJ` & `WKT`
#' @keywords internal
printEWextent <- function(property, center, latmax, latmin,dlon, datum, unit) {
  if (abs(center$lat) > 70) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", NaN, sign(center$lat) * 90.0, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equal area') {
      message("## Select Lambert azimuthal equal area projection")
      outputTEXT <- stringLinks("laea", NaN, sign(center$lat) * 90.0, NaN, NaN, center$lng, NaN, datum, unit)
    }
  } else if (abs(center$lat) < 15) {
    message("## Close to equator")
    if ((latmax * latmin) <= 0 ) {
      message("## Extent is touching or crossing equator")
      latS <- max(abs(latmax), abs(latmin)) / 2
    } else {
      message("## Extent is not crossing equator")
      latS <- center$lat
    }
    if (property == "Conformal") {
      message("## Select Mercator projection")
      outputTEXT <- stringLinks("merc", NaN, NaN, latS, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equal area') {
      message("## Select Cylindrical equal area projection")
      outputTEXT <- stringLinks("cea", NaN, NaN, latS, NaN, center$lng, NaN, datum, unit)
    }
  } else {
    message("## Mid-Latitude away from pole and equator")
    interval = (latmax - latmin)/6
    if (property == "Conformal") {
      message("## Select Lambert conformal conic projection")
      outputTEXT <-stringLinks("lcc", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN, datum, unit)
    } else if (property == 'Equal area') {
      message("## Select Albers equal area conic projection")
      outputTEXT <- stringLinks("aea", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN, datum, unit)
    }

  }

  return(outputTEXT)
}
