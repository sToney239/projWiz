test_that("multiplication works", {
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,unit = "ft")[["PROJ"]],
               "+proj=laea +lon_0=126.5 +lat_0=48 +datum=WGS84 +units=ft +no_defs")
  expect_equal(stringLinks("laea",NA,48,NA,NA,126.5,NA,unit = "m")[["WKT"]],
               'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
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
})
