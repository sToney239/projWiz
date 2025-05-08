test_that("multiplication works", {
  expect_equal(proj_world(world_proj_list$compromise$round_boudnary$Natural_Earth, 150),
               "+proj=natearth +lon_0=150 +datum=WGS84 +units=m +no_defs")
})
