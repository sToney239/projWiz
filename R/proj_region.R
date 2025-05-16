#' Projection for the area of interest with specified way of projection
#'
#' Auto selecting projections with specified property based on the geological shape and projection characteristics. Function will show messages of the basis how the projection is selected.
#' Please note that there's a longitudinal (less than 160) and latitudinal (less than 80) range limit. If area of interest larger than this please use [proj_hemisphere()] or use [proj_specify()].
#' @param obj Input geo data, should be one of:\cr
#'  - An object can be accepted by [sf::st_bbox()] to compute the bounding box\cr
#'  - A named list with longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#' @param property Projection property, should be one of "Equal area", "Conformal" and "Equidistant".
#' @param output_type A string for expected output, either "proj4" or "WKT"
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A `proj4` or `WKT` string
#' @export
#' @seealso [proj_equal_area()], [proj_conformal()], [proj_equidistant()]
#'
#' @examples proj_region(c(xmax=112,xmin=156,ymin=6,ymax=23), "Equal area")
proj_region <- function(obj, property="Equal area",output_type = "proj4",datum = "WGS84", unit = "m") {
  PROJstr <- switch(
    property,
    "Equal area" = proj_equal_area(obj = obj, output_type = output_type, datum = datum, unit = unit),
    "Conformal" = proj_conformal(obj = obj, output_type = output_type, datum = datum, unit = unit),
    "Equidistant" = proj_equidistant(obj = obj, output_type = output_type, datum = datum, unit = unit),
    "default"
  )
  return(PROJstr)
}


