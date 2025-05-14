#' World Map projection list
#'
#' @format ## "world_proj_list"
#' A list with popular world projections:
#'    - `equal_area`: Projections to keep the area property approximately correct
#'        - `point_polar`: Equal-area world map projections with poles represented as points
#'        - `line_polar`: Equal-area world map projections with poles represented as lines
#'    - `compromise`: Projections compromising shape and area
#'        - `round_boudnary`: Boundary on east and west as curve
#'        - `rectangular`: Boundary as rectange
#'
#' @source <https://github.com/ProjectionWizard/projectionwizard.github.io/blob/263b9ff09128e371ee923ab57bfc1ed41bbdc4ba/outputFormat.js#L115>
"world_proj_list"



#' UTM zone geometry data
#'
#' @format ## `utm_zone`
#' A Simple feature collection with 120 rows and 3 columns:
#' \describe{
#'   \item{zone}{UTM zone number}
#'   \item{hemisphere}{"N" for northern hemisphere, "S" for southern hemisphere}
#'   \item{geometry}{UTM zone geometries}
#' }
#'
#' @source <https://hub.arcgis.com/datasets/esri::world-utm-grid>
"utm_zone"
