#' check if Conic projection will have cone at polars
#'
#' @param lat0
#' @param lon0
#' @param projectionString
#' @param latmax
#' @param latmin
#' @param lonmax
#' @param lonmin
#'
#' @returns
#' @keywords internal
#'
#' @examples
checkConicOK = function(lat0, lon0, projectionString,latmax,latmin,lonmax,lonmin) {
  res = 1
  the_type = switch (projectionString,
                     "Lambert conformal conic" = "lcc",
                     "Lambert azimuthal equal area" = "laea",
                     "Albers equal area conic" = "aea"
  )

  interval = latmax - latmin
  new_proj = stringLinks(the_type, NaN, lat0, latmin + interval, latmax - interval, lon0, NaN)$PROJ

  test_pts_before_proj = list(
    sf::st_point(c(lon0, -90)),
    sf::st_point(c(lon0, 90)),
    sf::st_point(c(lonmin, latmin)),
    sf::st_point(c(lonmax, latmax))
  ) |>
    sf::st_sfc(crs = 4326)
  test_pts = sf::st_transform(test_pts_before_proj, new_proj)
  new_xy = sf::st_coordinates(test_pts)
  ymin = min(new_xy[,"Y"],na.rm = TRUE)
  ymax = max(new_xy[,"Y"],na.rm = TRUE)

  # there's a case that the standard parallel are too close,
  # one polar could never be properly projected,
  # therefore return null value from transform
  if (is.na(new_xy[[1,"Y"]])) {
    polar_criteria = (ymax - new_xy[2,"Y"]) >  1e-6
  } else if (is.na(new_xy[[2,"Y"]])) {
    polar_criteria = (ymin - new_xy[1,"Y"]) < -1e-6
  } else {
    polar_criteria = ((ymax - new_xy[2,"Y"]) >  1e-6) ||
      ((ymin - new_xy[1,"Y"]) < -1e-6)
  }

  if (polar_criteria) {
    if (projectionString == 'Lambert conformal conic') {
      res = -1;
    } else if (new_xy[4,"Y"] > new_xy[3,"Y"]) {
      # Case of Albers when the fan of the selected extent spans less than 180deg around a pole
      res = 0;
    } else {
      res = -1;
    }
  }

  return(res)
}
