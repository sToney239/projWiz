test_that("proj_equidstant works", {
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326)),
               "+proj=eqc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326)),
               "+proj=cass +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326)),
               "+proj=aeqd +lon_0=46.5 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326) ),
               "+proj=eqc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_equidstant(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326)),
               "+proj=aeqd +lon_0=179.5 +lat_0=28.5 +datum=WGS84 +units=m +no_defs")
})
