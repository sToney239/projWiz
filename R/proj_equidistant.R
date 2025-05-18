#' Projection for the area of interest with equidistant projection
#'
#' Auto selecting equidistant projections based on the geological shape and projection characteristics. Function will show messages of the basis how the projection is selected.
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
#' @seealso [proj_region()], [proj_equal_area()], [proj_conformal()]
#'
#' @examples proj_equidistant(c(xmax=112,xmin=156,ymin=6,ymax=23))
proj_equidistant <- function(obj, output_type = "proj4", datum = "WGS84", unit = "m") {
  input_ext = calc_extent(obj)

  center = with(input_ext, calc_center(lonmin, lonmax, latmin, latmax))
  dlon = with(input_ext, calc_dlon(lonmin, lonmax, latmin, latmax))

  latmax = input_ext[["latmax"]]
  latmin = input_ext[["latmin"]]
  # distance check
  lonlat_m = check_lonlat_dis(latmin, latmax, dlon)

  ratio <- lonlat_m[["dlat_m"]] / lonlat_m[["dlon_m"]]
  if (max(lonlat_m)  < 1e6) {
    message("## The map extent is not quite large")
    message("## Select Oblique azimuthal equidistant projection")
    outputTEXT <- stringLinks("aeqd", lat0 = center[["lat"]], lon0 = center[["lng"]], datum = datum, unit =unit)
  } else {
    message("## The map extent is relatively large, choose projection considering map shape")
    if (abs(center[["lat"]]) > 70) {
      message("## Close to poles")
      outputTEXT <- stringLinks("aeqd", lat0 = sign(center[["lat"]]) * 90.0, lon0 = center[["lng"]], datum = datum, unit =unit)
    } else if (abs(center[["lat"]]) < 15) {
      message("## Close to equator")
      if ((latmax * latmin) <= 0 ) {
        message("## Extent is touching or crossing equator")
        latS = max(abs(latmax), abs(latmin)) / 2
      } else{
        message("## Extent is not crossing equator")
        latS = center[["lat"]]
      }
      outputTEXT <- stringLinks("eqc", lat1 = latS, lon0 = center[["lng"]], datum = datum, unit =unit)
    } else {
      message("## Mid-Latitude away from pole and equator")
      if (ratio > 1.25) {
        message("## North-south extent")
        message("## Select Cassini projection")
        outputTEXT <- stringLinks("cass", lon0 = center[["lng"]], datum = datum, unit =unit)
      } else if (ratio < 0.8) {
        message("## East-west extent")
        message("## Select Equidistant conic projection")
        interval <- (latmax - latmin) / 6
        outputTEXT <- stringLinks("eqdc", lat0 = center[["lat"]], lat1 = latmin + interval, lat2 = latmax - interval, lon0 = center[["lng"]], datum = datum, unit =unit)
      } else {
        message("## Square-shaped extent")
        message("## Select Oblique azimuthal equidistant projection")
        outputTEXT <- stringLinks("aeqd", lat0 = center[["lat"]], lon0 = center[["lng"]], datum = datum, unit =unit)
      }
    }
  }

  if(output_type == "proj4") {
    return(outputTEXT[["PROJ"]])
  } else {
    return(outputTEXT[["WKT"]])
  }

}


