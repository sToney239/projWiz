test_that("proj_hemisphere works", {
  expect_equal(
    proj_hemisphere(sf::st_sfc(sf::st_polygon(  list(matrix(c(68, 8, 97, 8, 97, 35, 68, 35, 68, 8),
                                                        ncol = 2,
                                                        byrow = TRUE)) ),crs = 4326), "ortho"),
    "+proj=ortho +lon_0=82.5 +lat_0=21.5 +datum=WGS84 +units=m +no_defs"
  )
  expect_equal(
    proj_hemisphere(c("lon"=123,"lat"=13), "Equalarea"),
    "+proj=laea +lon_0=123 +lat_0=13 +datum=WGS84 +units=m +no_defs"
  )
})


