test_that("multiplication works", {
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,unit = "ft")[["PROJ"]],
               "+proj=laea +lon_0=126.5 +lat_0=48 +datum=WGS84 +units=ft +no_defs")
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,unit = "m")[["WKT"]],
               'PROJCS["Customized_Lambert_Azimuthal",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Lambert_Azimuthal_Equal_Area"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",126.5],
 PARAMETER["Latitude_Of_Origin",48],
 UNIT["Meter",1.0]]')
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,datum = "ETRS89")[["PROJ"]],
               "+proj=laea +lon_0=126.5 +lat_0=48 +ellps=GRS80 +units=m +no_defs")
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,datum = "NAD83")[["PROJ"]],
               "+proj=laea +lon_0=126.5 +lat_0=48 +datum=NAD83 +units=m +no_defs")
  expect_error(
    stringLinks("laea",NA,48,NA,NA,126.5,NA,datum = "NA083")[["PROJ"]]
  )
  expect_error(
    stringLinks("laea",NA,48,NA,NA,126.5,NA,datum = NA)
  )
  expect_error(
    stringLinks("laea",NA,48,NA,NA,126.5,NA,unit = "fm")
  )
  expect_equal(
    stringLinks("latlong",NA,48,NA,NA,126.5,NA)[["PROJ"]],
    "+proj=eqc +lon_0=126.5 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(stringLinks("stere",NA,48,NA,NA,126.5,0.9997)[["PROJ"]],
               "+proj=stere +lon_0=126.5 +lat_0=48 +k_0=0.9997 +datum=WGS84 +units=m +no_defs")
})

