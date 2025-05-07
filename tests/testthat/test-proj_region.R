test_that("multiplication works", {
  expect_equal(matrix(c(68, 8, 97, 8, 97, 35, 68, 35, 68, 8),
                      ncol = 2,
                      byrow = TRUE) |>
                 list() |>
                 sf::st_polygon() |>
                 st_sfc(crs = 4326) |>
                 proj_region("Equalarea"), "+proj=laea +lon_0=82.5 +lat_0=21.5 +datum=WGS84 +units=m +no_defs")
})
