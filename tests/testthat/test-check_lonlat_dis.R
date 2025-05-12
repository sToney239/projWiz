# test_that("haversine_lon_dist works", {
#   expect_equal(
#     haversine_lon_dist(-25, 67),
#     geosphere::distHaversine(c(13,-25), c(80, -25))
#   )
# })

test_that("haversine_lon_dist works", {
  expect_equal(haversine_lon_dist(-25, 67),6682480.75)
  expect_equal(haversine_lon_dist(-57, 67),3894853.061)
})
