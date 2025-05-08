test_that("proj_conformal works", {
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
                 "+proj=merc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
                 "+proj=tmerc +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326) ),
                 "+proj=lcc +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326)),
                 "+proj=merc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 160,ymin = 43,xmax = -148,ymax = 56),crs = 4326) ),
                 "+proj=lcc +lon_0=-174 +lat_1=45.1666667 +lat_2=53.8333333 +lat_0=49.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 146,ymin = 24,xmax = -174,ymax = 55),crs = 4326)),
                 "+proj=stere +lon_0=166 +lat_0=39.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = 168,ymin = 50,xmax = -166,ymax = 62),crs = 4326)),
                 "+proj=lcc +lon_0=-179 +lat_1=52 +lat_2=60 +lat_0=56 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_conformal(sf::st_bbox(c(xmin = -46,ymin = 52,xmax = 171,ymax = 64),crs = 4326)),
               "+proj=stere +lon_0=62.5 +lat_0=90 +datum=WGS84 +units=m +no_defs")
})
