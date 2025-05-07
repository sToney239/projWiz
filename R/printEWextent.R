#' Funcion for regional maps with an east-west extent
#'
#' @param property Longitude value to be corrected
#' @param center Median longitude value
#' @param latmax description
#' @param latmin description
#' @param lonmax description
#' @param lonmin description
#'
#' @returns A string
#' @keywords internal
#'
#' @examples printEWextent("Equalarea", center, latmax, latmin)
printEWextent <- function(property, center, latmax, latmin,lonmax,lonmin) {
  lng <- center$lng
  # case: close to poles
  if (center$lat > 70) {
    message("## Close to poles")
    if (property == "Conformal") {
      # "Stereographic"
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", NaN, 90.0, NaN, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection")
      # "Lambert azimuthal equal area"
      outputTEXT <- stringLinks("laea", NaN, 90.0, NaN, NaN, center$lng, NaN)
    }
  } else if (center$lat < -70) {
    message("## Close to poles")
    if (property == "Conformal") {
      # "Stereographic"
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", NaN, -90.0, NaN, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      #  "Lambert azimuthal equal area"
      message("## Select Lambert azimuthal equal area projection")
      outputTEXT <- stringLinks("laea", NaN, -90.0, NaN, NaN, center$lng, NaN)
    }
  } else if (abs(center$lat) < 15) {
    # case: close to equator
    message("## Close to equator")
    if ((latmax * latmin) <= 0 ) { # extent is touching or crossing equator
      latS <- max(abs(latmax), abs(latmin)) / 2
    } else { # extent is not crossing equator
      latS <- center$lat
    }
    if (property == "Conformal") {
      # "Mercator"
      message("## Select Mercator projection")
      outputTEXT <- stringLinks("merc", NaN, NaN, latS, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      # "Cylindrical equal area"
      message("## Select Cylindrical equal area projection")
      outputTEXT <- stringLinks("cea", NaN, NaN, latS, NaN, center$lng, NaN)
    }

  } else {
    # case: between pole and equator
    message("## Mid-Latitude away from pole and equator")
    # formating coordinates of the center
    interval <- (latmax - latmin) / 6

    if (property == "Conformal") {
      # Check if the fan of the selected extent exposes a cone opening at a pole
      if (checkConicOK(center$lat, center$lng, "Lambert conformal conic",latmax,latmin,lonmax,lonmin) > 0) {
        message("## Lambert conformal conic passed the conic check\n## Select Lambert conformal conic projection")
        outputTEXT <- stringLinks("lcc", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN)
      } else {
      # When the fan of the selected extent exposes a cone opening at a pole
        message("## Lambert conformal conic proj failed to pass the conic check\n## Select Stereographic projection")
        # "Stereographic"
        if (center$lat > 0) { # North Pole case
          outputTEXT <- stringLinks("stere", NaN, 90.0, NaN, NaN, center$lng, NaN)
        } else { # South Pole case
          outputTEXT <- stringLinks("stere", NaN, -90.0, NaN, NaN, center$lng, NaN)
        }
      }
    } else if (property == 'Equalarea') {
      # Check if the fan of the selected extent exposes a cone opening at a pole
      conicTest <- checkConicOK(center$lat, center$lng, "Albers equal area conic",latmax,latmin,lonmax,lonmin)
      if (conicTest > 0) {
        message("## Albers equal area conic passed the conic check\n## Select Albers equal area conic projection")
        outputTEXT <- stringLinks("aea", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN)
      } else {
        message("## Albers equal area conic failed to pass the conic check\n## Select Lambert azimuthal equal area projection")
        # When the fan of the selected extent exposes a cone opening at a pole
        conicTest <- checkConicOK(center$lat, center$lng, "Lambert azimuthal equal area",latmax,latmin,lonmax,lonmin)
        # Case when the fan of the selected extent spans less than 180deg around a pole
        if (conicTest == 0) {
          outputTEXT <- stringLinks("laea", NaN, center$lat, NaN, NaN, center$lng, NaN)
        } else if (center$lat > 0) {
          # North Pole case
          message("## The fan of the selected extent spans around the pole")
          outputTEXT <- stringLinks("laea", NaN, 90.0, NaN, NaN, center$lng, NaN)
        } else {
          # South Pole case
          message("## The fan of the selected extent spans around the pole")
          outputTEXT <- stringLinks("laea", NaN, -90.0, NaN, NaN, center$lng, NaN)
        }
      }
    }
  }

  return(outputTEXT)
}
