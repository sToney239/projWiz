#' Funcion for regional maps with an north-south extent
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
printNSextent <- function(property, center,latmax,latmin, datum, unit) {
  if (property == "Conformal") {
    # "Transverse Mercator"
    message("## Select Transverse Mercator projection")
    outputTEXT <- stringLinks("tmerc", NaN, NaN, NaN, NaN, center$lng, NaN, datum, unit)
  } else if (property == 'Equalarea') {
    # "Transverse cylindrical equal area"
    # if (latmax-latmin > 6) {
    #   cat("\nNote:To reduce overall distortion on the map, one can also compress the map in the north-south direction and expand the map in east-west direction.")
    # }
    message("## Select Transverse cylindrical equal area projection")
    outputTEXT <- stringLinks("tcea", NaN, NaN, NaN, NaN, center$lng, NaN, datum, unit)
  }
  return(outputTEXT)
}
# printNSextent("Equalarea",center)
