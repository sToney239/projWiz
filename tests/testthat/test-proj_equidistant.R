test_that("proj_equidistant works", {
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
               "+proj=eqc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
               "+proj=eqc +lon_0=28 +lat_ts=17.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326)),
               "+proj=eqdc +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_error(proj_equidistant(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326) ))

  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -10,ymin = 78,xmax = -4 ,ymax = 80),crs = 4326)),
               "+proj=aeqd +lon_0=-7 +lat_0=79 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -10,ymin = 78,xmax = -4 ,ymax = 80),crs = 4326)),
               "+proj=aeqd +lon_0=-7 +lat_0=79 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -10,ymin = -80,xmax = -4 ,ymax = -78),crs = 4326), output_type="WKT"),
               'PROJCS["Customized_Azimuthal_Equidistant",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",-7],
 PARAMETER["Latitude_Of_Origin",-79],
 UNIT["Meter",1.0]]')
  expect_error(proj_equidistant(c(xmin = 2,ymin = -23,xmax = 190 ,ymax = -1)))
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -10,ymin = 70,xmax = 34 ,ymax = 86),crs = 4326)),
               "+proj=aeqd +lon_0=12 +lat_0=90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = 70,ymin = -56,xmax =  80,ymax = -22),crs = 4326)),
               "+proj=cass +lon_0=75 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = 70,ymin = -26,xmax =  90,ymax = -2),crs = 4326)),
               "+proj=eqc +lon_0=80 +lat_ts=-14 +datum=WGS84 +units=m +no_defs")

  expect_equal(proj_equidistant(sf::st_bbox(c(xmin = -10,ymin = 78,xmax = -4 ,ymax = 80),crs = 4326)),
               "+proj=aeqd +lon_0=-7 +lat_0=79 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidistant(c(xmin = -14,ymin = 44,xmax = 0 ,ymax = 56) ),
               "+proj=aeqd +lon_0=-7 +lat_0=50 +datum=WGS84 +units=m +no_defs")

})
