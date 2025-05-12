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
proj_equidistant <- function(obj,output_type = "proj4",datum = "WGS84", unit = "m") {
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
    stop("Please input valid extent!")
  }
  # computing longitude extent
  dlon0 <- abs(lonmax - lonmin)
  dlon <- ifelse(dlon0 > 180, 360-dlon0, dlon0)

  # extent check
  if (dlon >= 160 | (latmax-latmin) >= 80) {
    stop("Longitude or latitude extent too large, please consider hemisphere or azimuthal projection")
  }

  if (lonmax < lonmin) {
    temp_mid = (lonmax + 360 + lonmin) / 2
    mid_lon = ifelse(temp_mid < 180, temp_mid, temp_mid-360)
  } else {
    mid_lon = (lonmax + lonmin) / 2
  }
  center <- list(lng = mid_lon, lat = (latmax + latmin) / 2)

  # distance check
  lonlat_m = check_lonlat_dis(latmin, latmax, dlon)
  ratio <- lonlat_m$dlat_m / lonlat_m$dlon_m
  if (max(lonlat_m$dlat_m, lonlat_m$dlon_m)  < 1e6) {
    message("## The map extent is not quite large")
    message("## Select Oblique azimuthal equidistant projection")
    outputTEXT <- stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN, datum, unit)
  } else {
    message("## The map extent is relatively large, choose projection considering map shape")
    if (abs(center$lat) > 70) {
      message("## Close to poles")
      # case: close to poles
      outputTEXT <- stringLinks("aeqd", NaN, sign(center$lat) * 90.0, NaN, NaN, center$lng, NaN, datum, unit)
    }  else if (ratio > 1.25) {
      message("## North-south extent")
      # case: with an north-south extent
      outputTEXT <- stringLinks("cass", NaN, NaN, NaN, NaN, center$lng, NaN, datum, unit)
    } else if (abs(center$lat) < 15) {
      message("## Close to equator")
      # case: close to equator
      if ((latmax * latmin) <= 0 ) {
        message("## Extent is touching or crossing equator")
        latS = max(abs(latmax), abs(latmin)) / 2
      } else{
        message("## Extent is not crossing equator")
        latS = center$lat
      }
      outputTEXT <- stringLinks("eqc", NaN, NaN, latS, NaN, center$lng, NaN, datum, unit)
    } else {
      message("## Mid-Latitude away from pole and equator")
      # case: between pole and equator
      # computing standard paralles
      # interval <- (latmax - latmin) / 6
      # Oblique azimuthal equidistant
      message("## Select Oblique azimuthal equidistant projection")
      outputTEXT <- stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN, datum, unit)
      # outputTEXT <- list(
      #   "Equidistant conic" = stringLinks("eqdc", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN),
      #   "Oblique azimuthal equidistant" = stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN)
      # )
    }
  }


  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }

}


