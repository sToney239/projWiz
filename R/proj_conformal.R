#' Projection for the area of interest with conformal projection
#'
#' Auto selecting conformal projections based on the geological shape and projection characteristics. Function will show messages of the basis how the projection is selected.
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
#' @seealso [proj_region()], [proj_equal_area()], [proj_equidistant()]
#'
#' @examples proj_conformal(c(xmax=112,xmin=156,ymin=6,ymax=23))
proj_conformal <- function(obj,output_type = "proj4",datum = "WGS84", unit = "m") {
  input_ext = calc_extent(obj)
  attach(input_ext)

  center = calc_center(lonmin, lonmax, latmin, latmax)
  dlon = calc_dlon(lonmin, lonmax, latmin, latmax)

  # distance check
  lonlat_m = check_lonlat_dis(latmin, latmax, dlon)
  # ratio check
  ratio <- lonlat_m[["dlat_m"]] / lonlat_m[["dlon_m"]]
  # zone number check
  gauss_kruger_3deg_para = check_gauss_kruger_3_deg(lonmax, lonmin)
  gauss_kruger_6deg_para = check_gauss_kruger_6_deg(lonmax, lonmin)
  utm_zone_para = check_utm_zone(c(xmin = lonmin, xmax = lonmax, ymin = latmin, ymax = latmax))

  if (max(lonlat_m)  < 1e6) {
    message("## The map extent is not quite large")
    message("## Select Stereographic projection")
    outputTEXT <- stringLinks("stere", lat0 = center[["lat"]], lon0 = center[["lng"]], datum=datum, unit=unit)
  } else {
    message("## The map extent is relatively large, choose projection considering map shape")
    if (dlon <= 3) {
      message("## longitude delta<=3, use mercarto family projection")
      outputTEXT <- stringLinks("tmerc", x0 = 500000, lon0 = center[["lng"]], k0 = 0.9999, datum = datum, unit = unit)
    } else if (dlon <= 6) {
      message("## longitude delta between 3 and 6, use mercarto family projection")
      outputTEXT <- stringLinks("tmerc", x0 = 500000, lon0 = center[["lng"]], k0 = 0.9996, datum = datum, unit = unit)
    } else {
      if (ratio > 1.25) {
        message("## North-south extent")
        outputTEXT <- printNSextent("Conformal", center,latmax,latmin, datum, unit)
      } else if (ratio < 0.8) {
        message("## East-west extent")
        outputTEXT <- printEWextent("Conformal", center,latmax,latmin,dlon, datum, unit)
      } else {
        message("## Square-shaped extent")
        outputTEXT <- printSquareFormat("Conformal", center,latmax,latmin, datum, unit)
      }
    }
  }
  if (!is.na(utm_zone_para[["utm_zone_num"]])) {
    hemisphrere = ifelse(utm_zone_para[["hemisphere"]] == "N", "norhtern", "southern")
    message(paste0("## You could also try UTM projection",
                   " of zone ", utm_zone_para[["utm_zone_num"]], " in ",hemisphrere, " hemisphere"),".")
  }
  if (!is.na(gauss_kruger_3deg_para[["zone_num"]])) {
    message(paste0("## You could also try 3-degree Gauss-Kruger projection",
                   " of zone ", gauss_kruger_3deg_para[["zone_num"]], " with central longitdue of ",gauss_kruger_3deg_para[["mid_lon"]]),".")
  } else if (!is.na(gauss_kruger_6deg_para[["zone_num"]])) {
    message(paste0("## You could also try 6-degree Gauss-Kruger projection",
                   " of zone ", gauss_kruger_6deg_para[["zone_num"]], " with central longitdue of ",gauss_kruger_6deg_para[["mid_lon"]]),".")
  }
  detach(input_ext)
  if(output_type == "proj4") {
    return(outputTEXT[["PROJ"]])
  } else {
    return(outputTEXT[["WKT"]])
  }
}


