#' Calculate the UTM zone number and the proj4 string
#'
#' @param obj Input geo data, should be one of:\cr
#'  - An object of class `sf` / `sfc` / `bbox` (from [sf::st_bbox()])  \cr
#'  - A named list with longitude and latitude extents with names of "xmin", "xmax" "ymin" and "ymax"
#'
#' @returns A list with "utm_zone_num", "hemisphere" and "proj4", if the boundary you input doensn't fall in any of the utm zone, all these values would be NA
#' @export
#'
#' @examples check_utm_zone(c(xmin = 6.15, xmax = 6.3, ymin = 49.7, ymax = 49.8))
check_utm_zone <- function(obj) {
  if (identical(sort(names(obj)), sort(c("xmin", "xmax", "ymin","ymax")))) {
    rec_mat = matrix(
      c(obj[["xmin"]],obj[["ymin"]],
        obj[["xmin"]],obj[["ymax"]],
        obj[["xmax"]],obj[["ymax"]],
        obj[["xmax"]],obj[["ymin"]],
        obj[["xmin"]],obj[["ymin"]]
      ), byrow = TRUE, ncol = 2
    )
    obj = sf::st_sfc(sf::st_polygon(list(rec_mat)), crs = 4326)
  } else {
    if(!sf::st_is_longlat(obj)) {
      obj = sf::st_transform(obj, 4326)
    }
    obj = sf::st_union(obj)
  }

  utm_zone_row_num = sf::st_within(obj, projWiz::utm_zone)[[1]]

  if (length(utm_zone_row_num) != 0) {
    utm_zone_num = projWiz::utm_zone[[utm_zone_row_num,"zone"]]
    utm_zone_hemi = projWiz::utm_zone[[utm_zone_row_num,"hemisphere"]]
    epsg_south = ifelse(utm_zone_hemi == 'S', " +south", "")
    utm_proj = paste0("+proj=utm +zone=", utm_zone_num,epsg_south, " +datum=WGS84 +units=m +no_defs +type=crs")
  } else {
    utm_zone_num = NA_real_
    utm_zone_hemi = NA_character_
    utm_proj = NA_character_
  }

  return(
    list(
      utm_zone_num = utm_zone_num,
      hemisphere = utm_zone_hemi,
      proj4 = utm_proj
    )
  )
}



