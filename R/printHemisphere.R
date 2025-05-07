printHemisphere = function(input_sf, property) {
  new_boundary = st_bbox(input_sf)
  lon = (new_boundary$xmax + new_boundary$xmin)/2
  lat = (new_boundary$ymax + new_boundary$ymin)/2
  if (property == 'Equalarea') {
    # "Lambert azimuthal equal area"
    outputTEXT = stringLinks("laea", NaN, lat, NaN, NaN, lon, NaN) 
  } else if (property == "Equidistant") {
    # "Azimuthal equidistant"
    outputTEXT = stringLinks("aeqd", NaN, lat, NaN, NaN, lon, NaN) 
  } else {
    # orthographic projection shows the globe as seen from space at an infinite distance
    outputTEXT = outputTEXT = stringLinks("ortho", NaN, lat, NaN, NaN, lon, NaN) 
  }
  return(outputTEXT)
}

