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
  expect_equal(proj_equal_area(sf::st_bbox(c(xmin = 70,ymin = -78,xmax = 162 ,ymax = -67),crs = 4326)),
               "+proj=laea +lon_0=116 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
})

