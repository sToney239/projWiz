proj_equidstant <- function(input_sf,output_type = "proj4") {
  new_boundary = sf::st_bbox(input_sf)
  lonmax = new_boundary$xmax
  lonmin = new_boundary$xmin
  latmax = new_boundary$ymax
  latmin = new_boundary$ymin
  center <- list(lng = (lonmax + lonmin) / 2, lat = (latmax + latmin) / 2)
  scale <- 720 / (lonmax - lonmin) / (sin(latmax * pi / 180) - sin(latmin * pi / 180))

  # computing longitude extent
  dlon <- (lonmax - lonmin)

  # reading central meridian - Assuming outputLON is defined elsewhere
  lng <- center$lng
  # getting the height-to-width ratio
  ratio <- (latmax - latmin) / dlon
  if (latmin > 0.0) {
    ratio <- ratio / cos(latmin * pi / 180)
  } else if (latmax < 0.0) {
    ratio <- ratio / cos(latmax * pi / 180)
  }
  if (center$lat > 70) {
    message("## Close to poles")
    # case: close to poles
    outputTEXT <- stringLinks("aeqd", NaN, 90.0, NaN, NaN, center$lng, NaN)
  } else if (center$lat < -70) {
    message("## Close to poles")
    outputTEXT <- stringLinks("aeqd", NaN, -90.0, NaN, NaN, center$lng, NaN)
  } else if (ratio > 1.25) {
    message("## North-south extent")
    # case: with an north-south extent
    outputTEXT <- stringLinks("cass", NaN, NaN, NaN, NaN, center$lng, NaN)
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
    outputTEXT <- stringLinks("eqc", NaN, NaN, latS, NaN, center$lng, NaN)
  } else {
    message("## Mid-Latitude away from pole and equator")
    # case: between pole and equator
    # computing standard paralles
    interval <- (latmax - latmin) / 6
    # Oblique azimuthal equidistant
    message("## Select Oblique azimuthal equidistant projection")
    outputTEXT <- stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN)
    # outputTEXT <- list(
    #   "Equidistant conic" = stringLinks("eqdc", NaN, center$lat, latmin + interval, latmax - interval, center$lng, NaN),
    #   "Oblique azimuthal equidistant" = stringLinks("aeqd", NaN, center$lat, NaN, NaN, center$lng, NaN)
    # )
  }
  if (requireNamespace(geosphere,quietly = TRUE)) {
    p1 = matrix(
      c(lonmin, latmin,lonmin, latmin,lonmax, latmax,lonmax, latmax),
      ncol = 2, byrow = TRUE
    )
    p2 =  matrix(
      c(lonmax, latmin,lonmin, latmax, lonmin, latmax, lonmax,latmin),
      ncol = 2, byrow = TRUE
    )
    if (max(geosphere::distHaversine(p1,p2)) / 1e6 < 1) {
      message("## The map extent is not quite large, you could try official projection, as well as other projections like 'laea' or 'stere', which won't produce much error at this scale")
    }
  }
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }

}


