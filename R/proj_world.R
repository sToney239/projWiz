#' Specify a world map projection
#'
#' @param world_proj A string of proj shorthand, could be extracted from `world_proj_list`
#' @param lng A numerical value of the expected central longitude
#' @param output_type A string for expected output, either "proj4" or "WKT"
#'
#' @returns A `proj4` or `WKT` string
#' @export
#'
#' @examples proj_world(world_proj_list$compromise$barrel_shape$Natural_Earth, 150)
proj_world <- function(world_proj, lng=0, output_type="proj4") {
  outputTEXT = switch (world_proj,
      "behrmann" = stringLinks("cea", lat1 = 30, lon0 = lng),
      "gall_peters" = stringLinks("cea", lat1 = 45, lon0 = lng),
      stringLinks(world_proj, lon0 = lng)
    )
  if (world_proj %in%  c("wintri", "natearth", "eqearth", "hammer") & lng != 0) {
    # Especially for raster data reprojections
    message("Please note that using this projection without 0 as the central longitude may result in unsatisfactory outcomes for raster data reprojections.")
  }
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }
}


# strongly recommend to go to "https://projectionwizard.org" to select one
