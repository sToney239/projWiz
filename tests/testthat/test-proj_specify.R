test_that("proj_specify works", {
  expect_equal(proj_specify(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326), "merc"),
               "+proj=merc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 146,ymin = 24,xmax = -174,ymax = 55),crs = 4326),"stere"),
               "+proj=stere +lon_0=166 +lat_0=39.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326),"aea" ),
                 "+proj=aea +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326),"cea"),
               "+proj=cea +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326),"eqc" ),
               "+proj=eqc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd"),
               "+proj=aeqd +lon_0=179.5 +lat_0=28.5 +datum=WGS84 +units=m +no_defs")
  expect_error(
    proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd", unit = NULL)
  )
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd",datum = "ETRS89"),
               "+proj=aeqd +lon_0=179.5 +lat_0=28.5 +ellps=GRS80 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd",datum = "NAD83"),
               "+proj=aeqd +lon_0=179.5 +lat_0=28.5 +datum=NAD83 +units=m +no_defs")
  expect_error(
    proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd",datum = "ETRS9")
  )
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"aeqd",x0 = 300),
               "+proj=aeqd +x_0=300 +lon_0=179.5 +lat_0=28.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"cass"),
               "+proj=cass +lon_0=179.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"mill"),
               "+proj=mill +lon_0=179.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"wintri"),
               "+proj=wintri +lon_0=179.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"tmerc", k0=0.9999),
               "+proj=tmerc +lon_0=179.5 +k_0=0.9999 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"cass", unit = "ft"),
               "+proj=cass +lon_0=179.5 +datum=WGS84 +units=ft +no_defs")
  expect_error(
    proj_specify(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"wintri", unit="mile")
  )
  expect_equal(proj_specify(sf::st_bbox(c(xmin = 146,ymin = 24,xmax = -174,ymax = 55),crs = 4326),"stere",k0=0.9997),
               "+proj=stere +lon_0=166 +lat_0=39.5 +k_0=0.9997 +datum=WGS84 +units=m +no_defs")
})
