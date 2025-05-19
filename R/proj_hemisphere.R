#' Projection for hemishpere
#'
#' @param obj Input geo data, should be one of:\cr
#'  - An object can be accepted by [sf::st_bbox()] to compute the bounding box\cr
#'  - A named list of longitude and latitude at centroid with names of "x" and "y"\cr
#'  - A named list with longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#' @param property Projection property, should be one of "Equal area", "Conformal", "Equidistant" and "ortho", the default value is ortho, for orthographic projection.
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
    input_ext = calc_extent(obj)
    center = with(input_ext, calc_center(lonmin, lonmax, latmin, latmax))
    lon = center[["lng"]]
    lat = center[["lat"]]
  }
  outputTEXT <- switch (property,
                        'Equal area' = stringLinks("laea", lat0 = lat, lon0 = lon),
                        "Conformal" = stringLinks("stere", lat0 = lat, lon0 = lon),
                        "Equidistant" = stringLinks("aeqd", lat0 = lat, lon0 = lon),
                        "ortho" = stringLinks("ortho", lat0 = lat, lon0 = lon)
  )

  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }
}


