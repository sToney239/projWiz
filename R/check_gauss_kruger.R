#' Calculate the 3-degree Gauss Kruger zone number and the central longitude of the input longitude range
#'
#' @param lonmax A numerical value, max longitude of the map extent
#' @param lonmin A numerical value, min longitude of the map extent
#'
#' @returns A list with name of "zone_num" and "mid_lon", if the input range doesn't fall in any zone, the list will be NAs.
#' @export
#'
#' @examples check_gauss_kruger_3_deg(11,12)
check_gauss_kruger_3_deg = function(lonmax, lonmin) {
  zone_number_3 = \(lon) as.integer((lon + 1.5) / 3)
  N1 = zone_number_3(lonmax)
  N2 = zone_number_3(lonmin)
  if (N1 == N2) {
    lon0 = 3 * N1
    return(list(zone_num = N1, mid_lon = lon0))
  } else {
    return(list(zone_num = NA_real_, mid_lon = NA_real_))
  }
}


#' Calculate the 6-degree Gauss Kruger zone number and the central longitude of the input longitude range
#'
#' @param lonmax A numerical value, max longitude of the map extent
#' @param lonmin A numerical value, min longitude of the map extent
#'
#' @returns A list with name of "zone_num" and "mid_lon", if the input range doesn't fall in any zone, the list will be NAs.
#' @export
#'
#' @examples check_gauss_kruger_6_deg(11,12)
check_gauss_kruger_6_deg = function(lonmax, lonmin) {
  zone_number_6 = \(lon) as.integer((lon + 6) / 6)
  N1 = zone_number_6(lonmax)
  N2 = zone_number_6(lonmin)
  if (N1 == N2) {
    lon0 = 6 * N1 - 3
    return(list(zone_num = N1, mid_lon = lon0))
  } else {
    return(list(zone_num = NA_real_, mid_lon = NA_real_))
  }
}
