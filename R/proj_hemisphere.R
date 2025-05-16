#' Projection for hemishpere
#'
#' @param obj Input geo data, should be one of:\cr
#'  - An object can be accepted by [sf::st_bbox()] to compute the bounding box\cr
#'  - A named list of longitude and latitude at centroid with names of "x" and "y"\cr
#'  - A named list with longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#' @param property Projection property, should be one of "Equal area", "Conformal" and "ortho", the default value is ortho, for orthographic projection.
#' @param output_type A string for expected output, either "proj4" or "WKT"
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A `proj4` or `WKT` string
#' @export
#'
#' @examples proj_hemisphere(c(x = 123, y = 13), "Equal area")
#' @examples proj_hemisphere(c(xmax=112,xmin=156,ymin=6,ymax=23), "Conformal")
proj_hemisphere = function(obj, property="ortho",output_type = "proj4",datum = "WGS84", unit = "m") {
  if (identical(sort(names(obj)), c("x", "y"))) {
     lon = obj[["x"]]
     lat = obj[["y"]]
     if (lon > 180 | lon < -180 |
         lat > 90 | lat < -90) {
       stop("Please input valid x y value!")
     }
  } else {
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
      stop("Please input valid xy extent!")
    }

    if (lonmax < lonmin) {
      temp_mid = (lonmax + 360 + lonmin) / 2
      lon = ifelse(temp_mid < 180, temp_mid, temp_mid-360)
    } else {
      lon = (lonmax + lonmin) / 2
    }
    lat = (latmax + latmin)/2
  }

  if (property == 'Equal area') {
    # "Lambert azimuthal equal area"
    outputTEXT = stringLinks("laea", NaN, lat, NaN, NaN, lon, NaN)
  } else if (property == "Equidistant") {
    # "Azimuthal equidistant"
    outputTEXT = stringLinks("aeqd", NaN, lat, NaN, NaN, lon, NaN)
  } else {
    # orthographic projection shows the globe as seen from space at an infinite distance
    outputTEXT = stringLinks("ortho", NaN, lat, NaN, NaN, lon, NaN)
  }
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }
}


