calc_extent = function(obj) {
  if (!(is.vector(obj) & identical(sort(names(obj)), sort(c("xmin", "xmax", "ymin","ymax"))))) {
    if(!sf::st_is_longlat(obj)) {
      obj = sf::st_transform(obj, 4326)
    }
    obj = sf::st_bbox(obj)
  }
  lonmax <- obj[["xmax"]]
  lonmin <- obj[["xmin"]]
  latmax <- obj[["ymax"]]
  latmin <- obj[["ymin"]]
  if (lonmin+270 < lonmax) {
    lonmax <- obj[["xmin"]]
    lonmin <- obj[["xmax"]]
  }
  if (lonmin > 180 | lonmin < -180 | lonmax > 180 | lonmax < -180 |
      latmin > 90 | latmin < -90 | latmax > 90 | latmax < -90 |
      latmax < latmin) {
    stop("Please input valid extent!")
  }
  return(list(
    "lonmax"= lonmax,
    "lonmin"= lonmin,
    "latmax"= latmax,
    "latmin"= latmin
  ))
}

# computing longitude extent
calc_dlon = function(lonmax, lonmin, latmin, latmax) {
  dlon0 <- abs(lonmax - lonmin)
  dlon <- ifelse(dlon0 > 180, 360-dlon0, dlon0)
  # extent check
  if (dlon >= 160 | (latmax-latmin) >= 80) {
    stop("Longitude or latitude range exceeds limits, please consider hemisphere or azimuthal projection")
  }
  return(dlon)
}

calc_center <- function(lonmin, lonmax, latmin, latmax) {
  if (lonmax < lonmin | abs(lonmax - lonmin)>180) {
    temp_mid = (lonmax + 360 + lonmin) / 2
    mid_lon = ifelse(temp_mid < 180, temp_mid, temp_mid-360)
  } else {
    mid_lon = (lonmax + lonmin) / 2
  }
  center <- list(lng = mid_lon, lat = (latmax + latmin) / 2)
  return(center)
}
