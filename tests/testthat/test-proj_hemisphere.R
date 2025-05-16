test_that("proj_hemisphere works", {
  expect_equal(
    proj_hemisphere(sf::st_sfc(sf::st_polygon(  list(matrix(c(68, 8, 97, 8, 97, 35, 68, 35, 68, 8),
                                                        ncol = 2,
                                                        byrow = TRUE)) ),crs = 4326), "ortho"),
    "+proj=ortho +lon_0=82.5 +lat_0=21.5 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(
    proj_hemisphere(c("x"=123,"y"=13), "Equal area"),
    "+proj=laea +lon_0=123 +lat_0=13 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(
    proj_hemisphere(list("xmin"=120,"xmax"=126,"ymin"=10,"ymax"=16), "Equal area"),
    "+proj=laea +lon_0=123 +lat_0=13 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(
    proj_hemisphere(c("x"=123,"y"=13), "Equidistant"),
    "+proj=aeqd +lon_0=123 +lat_0=13 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(
    proj_hemisphere(c("x"=123,"y"=13), "Equal area",output_type = "WKT"),
    'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Lambert_Azimuthal_Equal_Area"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",123],
 PARAMETER["Latitude_Of_Origin",13],
 UNIT["Meter",1.0]]'
  )
  expect_equal(proj_hemisphere(c(
    sf::st_point(c(12801741,-3248974)),
    sf::st_point(c(12801741,-2391879)),
    sf::st_point(c(13692297,-3248974)),
    sf::st_point(c(13692297,-2391879))
  ) |>
    sf::st_sfc(crs = 3857), "Equal area"),
  "+proj=laea +lon_0=118.9999964 +lat_0=-24.5000026 +datum=WGS84 +units=m +no_defs")
  expect_error(proj_hemisphere(c(xmin = 2,ymin = -23,xmax = 190 ,ymax = -1), "Equal area"))
  expect_error(proj_hemisphere(c(x=3,y = 190), "Equal area"))
})




