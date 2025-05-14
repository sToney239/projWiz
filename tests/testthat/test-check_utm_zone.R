test_that("check_utm_zone works", {
  expect_equal(check_utm_zone(c(xmin = -3, xmax = 5, ymin = 3, ymax = 5)),
               list(utm_zone_num = NA_real_, hemisphere = NA_character_, proj4 = NA_character_))
  expect_equal(check_utm_zone(c(xmin = 118.36, xmax = 119.24, ymin = 31.22, ymax = 32.61)),
               list(utm_zone_num = 50, hemisphere = "N", proj4 = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +type=crs"))
  expect_equal(check_utm_zone(c(xmin = 18.3, xmax = 21, ymin = -31, ymax = -29)),
               list(utm_zone_num = 34, hemisphere = "S", proj4 = "+proj=utm +zone=34 +south +datum=WGS84 +units=m +no_defs +type=crs"))
  expect_equal(check_utm_zone(spData::alaska),
               list(utm_zone_num = NA_real_, hemisphere = NA_character_, proj4 = NA_character_))
})
