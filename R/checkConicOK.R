# source("scripts/stringLinks.R")
# lonmin = 90.50
# lonmax = 103.45  
# latmin = 32.15 
# latmax = 36.15 
# lat0 = (latmin + latmax) / 2
# lon0 = (lonmin + lonmax) / 2
# lat0 = center$lat
# lon0 = center$lng
# projectionString = activeProjection
normalizeLON = function(lon, lon0) {
  while (lon < (lon0 - 180.0)) {
    lon = lon + 360.0
  }
  while (lon > (lon0 + 180.0)) {
    lon = lon - 360.0
  }
  return(lon)
}

checkConicOK = function(lat0, lon0, projectionString,latmax,latmin,lonmax,lonmin) {
  res = 1
  the_type = switch (projectionString,
                     "Lambert conformal conic" = "lcc",
                     "Lambert azimuthal equal area" = "laea",
                     "Albers equal area conic" = "aea"
  )
  # lat_1_2 = (latmax - latmin)/6 * c(1,5) + latmin
  # new_proj = paste0(
  #   "+type=crs +proj=",the_type," +lon_0=",lon0,
  #   " +lat_1=", lat_1_2[[1]],
  #   " +lat_2=", lat_1_2[[2]],
  #   " +datum=WGS84 +units=m +no_defs"
  # )
  interval = latmax - latmin
  new_proj = stringLinks(the_type, NaN, lat0, latmin + interval, latmax - interval, lon0, NaN)$PROJ
  
  test_pts_before_proj = list(
    sf::st_point(c(lon0, -90)),
    sf::st_point(c(lon0, 90)),
    sf::st_point(c(normalizeLON(lonmin, lon0), latmin)),
    sf::st_point(c(normalizeLON(lonmax, lon0), latmax))
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
