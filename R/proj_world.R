#' Specify a world map projection
#'
#' @param world_proj A string of proj shorthand, could be extracted from `world_proj_list`
#' @param lng A numerical value of the expected central longitude
#' @param output_type A string for expected output, either "proj4" or "WKT"
#'
#' @returns A `proj4` or `WKT` string
#' @export
#'
#' @examples proj_world(world_proj_list$compromise$round_boudnary$Natural_Earth, 150)
proj_world <- function(world_proj, lng, output_type="proj4") {
  outputTEXT = stringLinks(world_proj, NaN, NaN, NaN, NaN, lng, NaN)
  if(output_type == "proj4") {
    return(outputTEXT$PROJ)
  } else {
    return(outputTEXT$WKT)
  }
}


# strongly recommend to go to "https://projectionwizard.org" to select one
