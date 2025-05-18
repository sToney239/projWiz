expect_equal(
  calc_extent(obj = c("xmin" =  179, "xmax"=-179,"ymin"=12, "ymax"=15)),
  expected = list("lonmax"=-179, "lonmin" =  179, "latmax"=15,"latmin"=12)
)
