context("ji_find")

test_that("can find names", {
  out <- ji_find("beer")
  expect_equal(out$emoji, as.character(ji("beer")))
})
