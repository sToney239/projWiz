test_that("proj_region Equalarea works", {
  expect_equal(sf::st_bbox(c(xmin = 68,ymin = 8,xmax = 97,ymax = 35),crs = 4326) |>
                 proj_region("Equalarea"), "+proj=laea +lon_0=82.5 +lat_0=21.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326) |>
                 proj_region("Equalarea"),"+proj=cea +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326) |>
                 proj_region("Equalarea"),"+proj=tcea +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326) |>
                 proj_region("Equalarea"),"+proj=aea +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326) |>
                 proj_region("Equalarea"),"+proj=cea +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 168,ymin = 40,xmax = -171 ,ymax = 55),crs = 4326) |>
                 proj_region("Equalarea"),"+proj=laea +lon_0=178.5 +lat_0=47.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = -87,ymin = -4,xmax = -71 ,ymax = 10),crs = 4326),property = "Equalarea"),
               "+proj=laea +lon_0=-79 +lat_0=0 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 60,ymin = 68,xmax = 119 ,ymax = 80),crs = 4326),property = "Equalarea",output_type="WKT"),
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

  expect_equal(proj_region(c(
    sf::st_point(c(12801741,-3248974)),
    sf::st_point(c(12801741,-2391879)),
    sf::st_point(c(13692297,-3248974)),
    sf::st_point(c(13692297,-2391879))
  ) |>
    sf::st_sfc(crs = 3857), "Equalarea"),
  "+proj=laea +lon_0=118.9999964 +lat_0=-24.5000026 +datum=WGS84 +units=m +no_defs")
})


test_that("proj_region Conformal works", {
  expect_equal(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326) |>
                 proj_region("Conformal"),"+proj=merc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326) |>
                 proj_region("Conformal"),"+proj=tmerc +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326) |>
                 proj_region("Conformal"),"+proj=lcc +lon_0=46.5 +lat_1=-48.3333333 +lat_2=-29.6666667 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326) |>
                 proj_region("Conformal"),"+proj=merc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 160,ymin = 43,xmax = -148,ymax = 56),crs = 4326) |>
                 proj_region("Conformal"),"+proj=lcc +lon_0=-174 +lat_1=45.1666667 +lat_2=53.8333333 +lat_0=49.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 146,ymin = 24,xmax = -174,ymax = 55),crs = 4326) |>
                 proj_region("Conformal"),"+proj=stere +lon_0=166 +lat_0=39.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(sf::st_bbox(c(xmin = 168,ymin = 50,xmax = -166,ymax = 62),crs = 4326) |>
                 proj_region("Conformal"),"+proj=lcc +lon_0=-179 +lat_1=52 +lat_2=60 +lat_0=56 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 70,ymin = -78,xmax = 162 ,ymax = -67),crs = 4326),property = "Conformal"),
               "+proj=stere +lon_0=116 +lat_0=-90 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 60,ymin = 68,xmax = 119 ,ymax = 80),crs = 4326),property = "Conformal",output_type="WKT"),
               'PROJCS["ProjWiz_Custom_Stereographic",
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
  expect_equal(proj_region(sf::st_bbox(c(xmin = 80,ymin = 84,xmax = 91 ,ymax = 85),crs = 4326),property = "Conformal"),
               "+proj=stere +lon_0=85.5 +lat_0=90 +k_0=0.994 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 38,ymin = -83,xmax = 110 ,ymax = -80),crs = 4326),property = "Conformal"),
               "+proj=stere +lon_0=74 +lat_0=-90 +k_0=0.994 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 106,ymin = 28,xmax = 107 ,ymax = 31),crs = 4326),property = "Conformal"),
               "+proj=tmerc +x_0=5e+05 +lon_0=106.5 +k_0=0.9999 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 106,ymin = 28,xmax = 110 ,ymax = 30),crs = 4326),property = "Conformal"),
               "+proj=tmerc +x_0=5e+05 +lon_0=108 +k_0=0.9996 +datum=WGS84 +units=m +no_defs")
  expect_error(proj_region(c(xmin = 2,ymin = -23,xmax = 190 ,ymax = -1),"Equidistant"))
  expect_equal(proj_region(list(xmin = 106,ymin = 28,xmax = 110 ,ymax = 30),property = "Conformal"),
               "+proj=tmerc +x_0=5e+05 +lon_0=108 +k_0=0.9996 +datum=WGS84 +units=m +no_defs")
})


test_that("proj_region Equidistant works", {
  expect_equal(proj_region(sf::st_bbox(c(xmin = -30,ymin = -25,xmax = 20,ymax = 5),crs = 4326) ,"Equidistant"),
                 "+proj=eqc +lon_0=-5 +lat_ts=12.5 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 13,ymin = -25,xmax = 43 ,ymax = 35),crs = 4326),"Equidistant"),
                 "+proj=cass +lon_0=28 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 13,ymin = -53,xmax = 80 ,ymax = -25),crs = 4326),"Equidistant"),
                 "+proj=aeqd +lon_0=46.5 +lat_0=-39 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),crs = 4326),"Equidistant"),
                "+proj=eqc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(c(xmin = 2,ymin = -23,xmax = 170 ,ymax = -1),"Equidistant"),
               "+proj=eqc +lon_0=86 +lat_ts=-12 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_region(sf::st_bbox(c(xmin = 142,ymin = 25,xmax = -143 ,ymax = 32),crs = 4326),"Equidistant"),
               "+proj=aeqd +lon_0=179.5 +lat_0=28.5 +datum=WGS84 +units=m +no_defs")
})
