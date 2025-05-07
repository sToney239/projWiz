# Function for regional maps in square format
# Conformal, Equalarea

printSquareFormat <- function(property, center, latmax, latmin) {
  lng <- center$lng
  # case: close to poles
  if (center$lat > 75) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 90°N as central latitude")
      outputTEXT <- stringLinks("stere", NaN, 90.0, NaN, NaN, center$lng, NaN) 
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 90°N as central latitude")
      outputTEXT <- stringLinks("laea", NaN, 90.0, NaN, NaN, center$lng, NaN) 
    }
  } else if (center$lat < -75) {
    message("## Close to poles")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 90°S as central latitude")
      outputTEXT <- stringLinks("stere", NaN, -90.0, NaN, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 90°S as central latitude")
      outputTEXT <- stringLinks("laea", NaN, -90.0, NaN, NaN, center$lng, NaN)
    }
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    # case: close to equator and crossing it
    message("## Close to equator and crossing it")
    if (property == "Conformal") {
      message("## Select Stereographic projection with 0° as central latitude")
      outputTEXT <- stringLinks("stere", NaN, 0.0, NaN, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection with 0° as central latitude")
      outputTEXT <- stringLinks("laea", NaN, 0.0, NaN, NaN, center$lng, NaN)
    }
  } else {
    # case: between pole and equator
    message("## Mid-Latitude away from pole and equator")
    if (property == "Conformal") {
      message("## Select Stereographic projection")
      outputTEXT <- stringLinks("stere", NaN, center$lat, NaN, NaN, center$lng, NaN)
    } else if (property == 'Equalarea') {
      message("## Select Lambert azimuthal equal area projection")
      outputTEXT <- stringLinks("laea", NaN, center$lat, NaN, NaN, center$lng, NaN)
    }
  }
  return(outputTEXT)
}
# printSquareFormat("Equalarea",center, latmax, latmin)
