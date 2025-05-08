#' Funcion for regional maps with an square-shaped extent
#'
#' @param property Projection property, should be one of "Equalarea", "Conformal" and "Equidistant".
#' @param center A list with with numerical values named with `lng` & `lat` element, which are the median longitude and latitude values
#' @param latmax A numerical value, max latitude of the map extent
#' @param latmin A numerical value, min latitude of the map extent
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A list of strings named with `PROJ` & `WKT`
#' @keywords internal
#'
#' @examples printSquareFormat("Equalarea", center, latmax, latmin)
printSquareFormat <- function(property, center, latmax, latmin, datum, unit) {
  lng <- center$lng
  # case: close to poles
  if (center$lat > 75) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 90°N as central latitude")
      outputTEXT <- stringLinks("stere", NaN, 90.0, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 90°N as central latitude")
      outputTEXT <- stringLinks("laea", NaN, 90.0, NaN, NaN, center$lng, NaN, datum, unit)
    }
  } else if (center$lat < -75) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 90°S as central latitude")
      outputTEXT <- stringLinks("stere", NaN, -90.0, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 90°S as central latitude")
      outputTEXT <- stringLinks("laea", NaN, -90.0, NaN, NaN, center$lng, NaN, datum, unit)
    }
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    # case: close to equator and crossing it
    message("## Close to equator and crossing it")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 0° as central latitude")
      outputTEXT <- stringLinks("stere", NaN, 0.0, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 0° as central latitude")
      outputTEXT <- stringLinks("laea", NaN, 0.0, NaN, NaN, center$lng, NaN, datum, unit)
    }
  } else {
    # case: between pole and equator
    message("## Mid-Latitude away from pole and equator")
    if (property == "Conformal") {
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", NaN, center$lat, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection")
      outputTEXT <- stringLinks("laea", NaN, center$lat, NaN, NaN, center$lng, NaN, datum, unit)
    }
  }
  return(outputTEXT)
}
# printSquareFormat("Equalarea",center, latmax, latmin)
