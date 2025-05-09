test_that("proj_equidstant works", {
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
               "+proj=eqc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
               "+proj=cass +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326)),
               "+proj=aeqd +lon_0=46.5 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326) ),
               "+proj=eqc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = -10,ymin = 78,xmax = -4 ,ymax = 80),crs = 4326)),
               "+proj=aeqd +lon_0=-7 +lat_0=90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = -10,ymin = 78,xmax = -4 ,ymax = 80),crs = 4326)),
               "+proj=aeqd +lon_0=-7 +lat_0=90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = -10,ymin = -80,xmax = -4 ,ymax = -78),crs = 4326), output_type="WKT"),
               'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",-7],
 PARAMETER["Latitude_Of_Origin",-90],
 UNIT["Meter",1.0]]')
  expect_error(proj_equidstant(c(xmin = 2,ymin = -23,xmax = 190 ,ymax = -1)))
})
