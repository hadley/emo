context("clock")

test_that("clock handles 12 o clock", {
  x <- strptime("2017/07/12 12:05", "%Y/%m/%d %H:%M")
  expect_equal( attr(clock(x), "data")$name, "twelve o’clock" )

  x <- strptime("2017/07/12 11:52", "%Y/%m/%d %H:%M")
  expect_equal( attr(clock(x), "data")$name, "twelve o’clock" )

  x <- strptime("2017/07/12 00:05", "%Y/%m/%d %H:%M")
  expect_equal( attr(clock(x), "data")$name, "twelve o’clock" )

  x <- strptime("2017/07/12 23:52", "%Y/%m/%d %H:%M")
  expect_equal( attr(clock(x), "data")$name, "twelve o’clock" )
})
