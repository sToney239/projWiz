test_that("printEWextent works", {
  expect_equal(
    printEWextent("Conformal", center = list("lng" = 13, "lat" = 11),latmax = 13,latmin = 9, dlon = 11,datum = "WGS84", unit = "m"),
    stringLinks("merc", lat1 = 11, lon0 = 13)
  )
})
