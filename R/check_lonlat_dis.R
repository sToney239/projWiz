check_lonlat_dis = function(latmin, latmax, dlon) {
  earth_radius <- 6378137
  dlat <- latmax - latmin
  dlat_m <- dlat * (pi / 180) * earth_radius
  dlon_m <- max(haversine_lon_dist(c(latmin,latmax), dlon), na.rm = TRUE)
  return(list(dlon_m = dlon_m, dlat_m = dlat_m))
}




haversine_lon_dist = function(lat, dlon) {
  lat_rad = lat * pi/180
  dlon = dlon * pi/180
  earth_radius <- 6378137
  a <- cos(lat_rad) * cos(lat_rad) * (sin(dlon/2))^2
  a <- pmin(a, 1)
  dist <- 2 * atan2(sqrt(a), sqrt(1 - a)) * earth_radius
  return(dist)
}





