test_that("proj_conformal works", {
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
                 "+proj=merc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
                 "+proj=tmerc +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326) ),
                 "+proj=lcc +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_error(proj_conformal(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326)))

  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 160,ymin = 43,xmax = -148,ymax = 56),crs = 4326) ),
                 "+proj=lcc +lon_0=-174 +lat_1=45.1666667 +lat_2=53.8333333 +lat_0=49.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 146,ymin = 24,xmax = -174,ymax = 55),crs = 4326)),
                 "+proj=stere +lon_0=166 +lat_0=39.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 168,ymin = 50,xmax = -166,ymax = 62),crs = 4326)),
                 "+proj=lcc +lon_0=-179 +lat_1=52 +lat_2=60 +lat_0=56 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -46,ymin = 52,xmax = 171,ymax = 64),crs = 4326)),
               "+proj=lcc +lon_0=-117.5 +lat_1=54 +lat_2=62 +lat_0=58 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -82,ymin = 70,xmax = -36 ,ymax = 84),crs = 4326)),
               "+proj=stere +lon_0=-59 +lat_0=90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -87,ymin = -4,xmax = -71 ,ymax = 10),crs = 4326)),
               "+proj=stere +lon_0=-79 +lat_0=0 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 70,ymin = -78,xmax = 162 ,ymax = -67),crs = 4326)),
               "+proj=stere +lon_0=116 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 60,ymin = 68,xmax = 119 ,ymax = 80),crs = 4326),output_type="WKT"),
               'PROJCS["Customized_Stereographic",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Stereographic"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",89.5],
 PARAMETER["Scale_Factor",1.0],
 PARAMETER["Latitude_Of_Origin",90],
 UNIT["Meter",1.0]]'
  )
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 67,ymin = -82,xmax = 101 ,ymax = -74),crs = 4326)),
               "+proj=stere +lon_0=84 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 80,ymin = 84,xmax = 91 ,ymax = 85),crs = 4326)),
               "+proj=stere +lon_0=85.5 +lat_0=84.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 38,ymin = -83,xmax = 110 ,ymax = -80),crs = 4326)),
               "+proj=stere +lon_0=74 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 106,ymin = 28,xmax = 107 ,ymax = 31),crs = 4326)),
               "+proj=stere +lon_0=106.5 +lat_0=29.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 106,ymin = 28,xmax = 110 ,ymax = 30),crs = 4326)),
               "+proj=stere +lon_0=108 +lat_0=29 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -47,ymin = -59,xmax = 179 ,ymax = -47),crs = 4326)),
               "+proj=lcc +lon_0=-114 +lat_1=-57 +lat_2=-49 +lat_0=-53 +datum=WGS84 +units=m +no_defs")
  expect_error(proj_conformal(c(xmin = 2,ymin = -23,xmax = 190 ,ymax = -1)))
  expect_equal(proj_conformal(c(xmin = 5,ymin = 12,xmax = 7 ,ymax = 13)),
               "+proj=stere +lon_0=6 +lat_0=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(c(xmin = 5,ymin = 12,xmax = 7 ,ymax = 30)),
               "+proj=tmerc +x_0=5e+05 +lon_0=6 +k_0=0.9999 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(c(xmin = 7,ymin = 12,xmax = 11 ,ymax = 30)),
               "+proj=tmerc +x_0=5e+05 +lon_0=9 +k_0=0.9996 +datum=WGS84 +units=m +no_defs")
})


