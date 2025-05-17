#' World Map projection list
#'
#' @format ## "world_proj_list"
#' A list with popular world projections:
#'    - `equal_area`: Projections to keep the area property approximately correct
#'        - `barrel_shape`: Equal-area world map projections with poles represented as lines
#'        - `ellipse`: Equal-area world map projections with poles represented as points
#'        - `rectangular`: cylindrical projection with world map shape like rectangular
#'    - `compromise`: Projections compromising shape and area
#'        - `barrel_shape`: pseudocylindrical projection with parallel straight lines at the north and south edges and nearly elliptical lines on the east and west sides
#'        - `rectangular`: cylindrical projection with world map shape like rectangular
#'
#' @source <https://github.com/ProjectionWizard/projectionwizard.github.io/blob/263b9ff09128e371ee923ab57bfc1ed41bbdc4ba/outputFormat.js#L115>
"world_proj_list"



#' UTM zone geometry data
#'
#' @format ## "utm_zone"
#' A Simple feature collection with 120 rows and 3 columns:
#' \describe{
#'   \item{zone}{UTM zone number}
#'   \item{hemisphere}{"N" for northern hemisphere, "S" for southern hemisphere}
#'   \item{geometry}{UTM zone geometries}
#' }
#' @keywords internal
#'
#' @source <https://hub.arcgis.com/datasets/esri::world-utm-grid>
"utm_zone"


#' Downsampled Natural Earth 1:50 Shaded Relief World Map
#'
#' @format ## "world_shaded_relief"
#' A array of dimention of 360, 720, 3 for 3 layer for R, G, B band with 0.5 degree on lon and lat direction, please use `terra::rast()` when using
#'
#' @examples
#' world_base_map = terra::rast(projWiz::world_shaded_relief)
#' terra::ext(world_base_map) <- c(-180,180,-90,90)
#' terra::crs(world_base_map) <- "epsg:4326"
#'
#' @source <https://www.naturalearthdata.com/downloads/50m-raster-data/50m-cross-blend-hypso/>
"world_shaded_relief"

#' 1:110m Spain & Bolivia map
#'
#' @format ## "example_country"
#' A list with Spain and Bolivia 110m map from Natural Earth 1:110m Cultural Vectors
#'
#' @examples
#' spain_polygon = sf::st_polygon(list(projWiz::example_country$Spain))
#' plot(sf::st_sfc(spain_polygon, crs = 4326), graticule = TRUE, key.pos = NULL, axes = TRUE)
#'
#' @source <https://www.naturalearthdata.com/downloads/110m-cultural-vectors/>
"example_country"
