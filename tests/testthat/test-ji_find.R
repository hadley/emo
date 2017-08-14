context("ji_find")

test_that("can find names", {
  out <- ji_find("beer")
  expect_equal(out$emoji, as.character(ji("beer")))
})

test_that("can find names from regex", {
  out <- ji_find("zeal", regex = TRUE)
  expect_equal(out$emoji, as.character(ji("new_zealand")))
})
