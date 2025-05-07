proj_conformal <- function(input_sf,output_type = "proj4") {
  new_boundary = st_bbox(input_sf)
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
  if (latmin >= 84)  {
    message("## very large scale, Universal Polar Stereographic")
    # case: very large scale, Universal Polar Stereographic - North Pole
    outputTEXT <- stringLinks("stere", NaN, 90.0, NaN, NaN, center$lng, 0.994)
  } else if (latmax <= -80)  {
    message("## very large scale, Universal Polar Stereographic")
    # case: very large scale, Universal Polar Stereographic - South Pole
    outputTEXT <- stringLinks("stere", NaN, -90.0, NaN, NaN, center$lng, 0.994)
  } else if (dlon <= 3) {
    message("## longitude delta<=3, like on 'state plane' coordinate system")
    # case: very large scale, like on "state plane" coord. sys.
    # False easting: 500000.0 & Scale factor: 0.9999
    outputTEXT <- stringLinks("tmerc", 500000.0, NaN, NaN, NaN, center$lng, 0.9999)
  } else if (dlon <= 6) {
    message("## longitude delta between 3 and 6, like on 'state plane' coordinate system")
    # case: very large scale, like Universal Transverse Mercator
    # False easting: 500000.0 & Scale factor: 0.9996
    outputTEXT <- stringLinks("tmerc", 500000.0, NaN, NaN, NaN, center$lng, 0.9996)
  } else {
    # Different map formats
    if (ratio > 1.25) {
      # Regional maps with an north-south extent
      message("## North-south extent")
      outputTEXT <- printNSextent("Conformal", center,latmax,latmin)
    } else if (ratio < 0.8) {
      message("## East-west extent")
      # Regional maps with an east-west extent
      outputTEXT <- printEWextent("Conformal", center, scale,latmax,latmin,lonmax,lonmin)
    } else {
      message("## Square-shaped extent")
      # Regional maps in square format
      outputTEXT <- printSquareFormat("Conformal", center,latmax,latmin)
    }
  }
  # if (scale > 260) {
  #   # general note for maps showing a smaller area
  #   message("## For maps at this scale, you can try some official projections.\nMost countries use a conformal projection for their official large-scale maps.\nYou can search for official projections in https://epsg.org/")
  # }
  if (require(geosphere,quietly = TRUE)) {
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


