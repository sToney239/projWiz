# Funcion for regional maps with an north-south extent
printNSextent <- function(property, center,latmax,latmin) {
  if (property == "Conformal") {
    # "Transverse Mercator"
    message("## Select Transverse Mercator projection")
    outputTEXT <- stringLinks("tmerc", NaN, NaN, NaN, NaN, center$lng, NaN)
  } else if (property == 'Equalarea') {
    # "Transverse cylindrical equal area"
    # if (latmax-latmin > 6) {
    #   cat("\nNote:To reduce overall distortion on the map, one can also compress the map in the north-south direction and expand the map in east-west direction.")
    # }
    message("## Select Transverse cylindrical equal area projection")
    outputTEXT <- stringLinks("tcea", NaN, NaN, NaN, NaN, center$lng, NaN)
  }
  return(outputTEXT)
}
# printNSextent("Equalarea",center)
