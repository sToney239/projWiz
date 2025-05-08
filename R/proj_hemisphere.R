#' Projection for hemishpere
#'
#' @param obj An object to compute the bounding box from, which can be accepted by [sf::st_bbox()]
#' @param property Projection property, should be one of "Equalarea", "Conformal" and "ortho", the default value is ortho, for orthographic projection.
#' @param output_type A string for expected output, either "proj4" or "WKT"
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A `proj4` or `WKT` string
#' @export
#'
#' @examples proj_hemisphere(sf::st_as_sf(quakes,coords = c("long","lat"),crs = 4326))
proj_hemisphere = function(obj, property="ortho",output_type = "proj4",datum = "WGS84", unit = "m") {
  if(!sf::st_is_longlat(obj)) {
    obj = sf::st_transform(obj, 4326)
  }
  new_boundary = sf::st_bbox(obj)
  lonmax = new_boundary$xmax
  lonmin = new_boundary$xmin
  if (lonmin+180 < lonmax) {
    lonmax = new_boundary$xmin
    lonmin = new_boundary$xmax
  }
  if (lonmax < lonmin) {
    temp_mid = (lonmax + 360 + lonmin) / 2
    lon = ifelse(temp_mid < 180, temp_mid, temp_mid-360)
  } else {
    lon = (lonmax + lonmin) / 2
  }
  # lon = (new_boundary$xmax + new_boundary$xmin)/2
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
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }
}

