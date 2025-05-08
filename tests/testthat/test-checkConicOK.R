test_that("multiplication works", {
  expect_equal(
    checkConicOK(
      48.5, 140,"Lambert azimuthal equal area",55,42,-132,52
    ), 1
  )
  expect_equal(
    checkConicOK(
      48.5, 140,"Lambert conformal conic",55,42,-132,52
    ), 1
  )
  expect_equal(
    checkConicOK(
      48.5, 140,"Albers equal area conic",55,42,-132,52
    ), 0
  )
})
