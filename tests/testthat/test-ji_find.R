context("ji_find")

test_that("can find names", {
  out <- ji_find("ant")
  expect_equal(out$emoji, as.character(ji("ant")))
})
