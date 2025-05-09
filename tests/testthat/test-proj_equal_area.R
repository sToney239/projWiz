test_that("proj_equal_area works", {
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 68,ymin = 8,xmax = 97,ymax = 35),crs = 4326)),
               "+proj=laea +lon_0=82.5 +lat_0=21.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
               "+proj=cea +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
               "+proj=tcea +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326)),
               "+proj=aea +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326)),
               "+proj=cea +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 168,ymin = 40,xmax = -171 ,ymax = 55),crs = 4326) ),
               "+proj=laea +lon_0=178.5 +lat_0=47.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 52,ymin = 42,xmax = -132 ,ymax = 55),crs = 4326)),
               "+proj=laea +lon_0=140 +lat_0=48.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = -82,ymin = 70,xmax = -36 ,ymax = 84),crs = 4326)),
               "+proj=laea +lon_0=-59 +lat_0=90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = -87,ymin = -4,xmax = -71 ,ymax = 10),crs = 4326)),
               "+proj=laea +lon_0=-79 +lat_0=0 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 60,ymin = 68,xmax = 119 ,ymax = 80),crs = 4326),output_type="WKT"),
               'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Lambert_Azimuthal_Equal_Area"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",89.5],
 PARAMETER["Latitude_Of_Origin",90],
 UNIT["Meter",1.0]]')
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 67,ymin = -82,xmax = 101 ,ymax = -74),crs = 4326)),
               "+proj=laea +lon_0=84 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(c(
    sf::st_point(c(12801741,-3248974)),
    sf::st_point(c(12801741,-2391879)),
    sf::st_point(c(13692297,-3248974)),
    sf::st_point(c(13692297,-2391879))
  ) |>
    sf::st_sfc(crs = 3857)),
               "+proj=laea +lon_0=118.9999964 +lat_0=-24.5000026 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 44,ymin = -80,xmax = 101 ,ymax = -74),crs = 4326)),
               "+proj=laea +lon_0=72.5 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
})

