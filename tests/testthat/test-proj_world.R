test_that("proj_world works", {
  expect_equal(proj_world(world_proj_list$compromise$round_boudnary$Natural_Earth, 150),
               "+proj=natearth +lon_0=150 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_world(world_proj_list$compromise$round_boudnary$Winkel_Tripel, 150),
               "+proj=wintri +lon_0=150 +datum=WGS84 +units=m +no_defs")
  expect_equal(proj_world(projWiz::world_proj_list$equal_area$point_polar$Mollweide,0,output_type = "WKT"),
               'PROJCS["ProjWiz_Custom_Mollweide",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Mollweide"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",0],
 UNIT["Meter",1.0]]')
})
