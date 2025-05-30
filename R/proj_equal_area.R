#' Projection for the area of interest with equal area projection
#'
#' Auto selecting equal-area projections based on the geological shape and projection characteristics. Function will show messages of the basis how the projection is selected.\cr
#' Please note that there's a longitudinal (less than 160) and latitudinal (less than 80) range limit. If area of interest larger than this please use [proj_hemisphere()] or use [proj_specify()].
#' @param obj Input geo data, should be one of:\cr
#'  - An object can be accepted by [sf::st_bbox()] to compute the bounding box\cr
#'  - A named list with longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#' @param output_type A string for expected output, either "proj4" or "WKT"
#' @param datum A string for the datum used with the coordinates (currently only 'WGS84', 'ETRS89' and 'NAD83' supported)
#' @param unit A string for horizontal coordinate system units (currently only 'm' and 'ft' supported)
#'
#' @returns A `proj4` or `WKT` string
#' @export
#' @seealso [proj_region()], [proj_conformal()], [proj_equidistant()]
#'
#' @examples proj_equal_area(c(xmax=112,xmin=156,ymin=6,ymax=23))
proj_equal_area <- function(obj,output_type = "proj4",datum = "WGS84", unit = "m") {
  input_ext = calc_extent(obj)
  lonmax = input_ext[["lonmax"]]
  lonmin = input_ext[["lonmin"]]
  latmax = input_ext[["latmax"]]
  latmin = input_ext[["latmin"]]

  center = calc_center(lonmin, lonmax, latmin, latmax)
  dlon = calc_dlon(lonmin, lonmax, latmin, latmax)

  # distance check
  lonlat_m = calc_lonlat_dis(latmin, latmax, dlon)

  if (max(lonlat_m)  < 1e6) {
    message("## The map extent is not quite large")
    message("## Select Lambert azimuthal equal area projection")
    outputTEXT <- stringLinks("laea", lat0 = center[["lat"]], lon0 = center[["lng"]], datum = datum, unit = unit)
  } else {
    message("## The map extent is relatively large, choose projection considering map shape")
    # ratio check
    ratio <- lonlat_m[["dlat_m"]] / lonlat_m[["dlon_m"]]
    if (ratio > 1.25) {
      message("## North-south extent")
      outputTEXT <- printNSextent("Equal area", center,latmax,latmin, datum, unit)
    } else if (ratio < 0.8) {
      message("## East-west extent")
      outputTEXT <- printEWextent("Equal area", center,latmax,latmin,dlon, datum, unit)
    } else {
      message("## Square-shaped extent")
      outputTEXT <- printSquareFormat("Equal area", center,latmax,latmin, datum, unit)
    }
  }
  if(output_type == "proj4") {
    return(outputTEXT[["PROJ"]])
  } else {
    return(outputTEXT[["WKT"]])
  }

}




