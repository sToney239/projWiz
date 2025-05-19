#' Funcion for regional maps with an square-shaped extent
#'
#' @param property Projection property, should be one of "Equal area", "Conformal" and "Equidistant".
#' @param center A list with with numerical values named with `lng` & `lat` element, which are the median longitude and latitude values
#' @param latmax A numerical value, max latitude of the map extent
#' @param latmin A numerical value, min latitude of the map extent
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A list of strings named with `PROJ` & `WKT`
#' @keywords internal
printSquareFormat <- function(property, center, latmax, latmin, datum = datum, unit = unit) {
  if (abs(center$lat) > 75) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection with pole as central latitude")
      outputTEXT <- stringLinks("stere", lat0 = sign(center$lat) * 90.0, lon0 = center$lng, datum = datum, unit = unit)
    } else if (property == 'Equal area') {
      message("## Select Lambert azimuthal equal area projection with pole as central latitude")
      outputTEXT <- stringLinks("laea", lat0 = sign(center$lat) * 90.0, lon0 = center$lng, datum = datum, unit = unit)
    }
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    message("## Extent is touching or crossing equator")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 0 as central latitude")
      outputTEXT <- stringLinks("stere", lat0 = 0.0, lon0 = center$lng, datum = datum, unit = unit)
    } else if (property == 'Equal area') {
      message("## Select Lambert azimuthal equal area projection with 0 as central latitude")
      outputTEXT <- stringLinks("laea", lat0 = 0.0, lon0 = center$lng, datum = datum, unit = unit)
    }
  } else {
    message("## Mid-Latitude away from pole and equator")
    if (property == "Conformal") {
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", lat0 = center$lat, lon0 = center$lng, datum = datum, unit = unit)
    } else if (property == 'Equal area') {
      message("## Select Lambert azimuthal equal area projection")
      outputTEXT <- stringLinks("laea", lat0 = center$lat, lon0 = center$lng, datum = datum, unit = unit)
    }
  }
  return(outputTEXT)
}
