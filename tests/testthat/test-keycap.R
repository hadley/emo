context("keycap")

test_that("keycap refuses things", {
  expect_error( keycap( "grze" ) )
})

test_that("keycap keeps some metadata", {
  ten <- keycap(10)
  expect_equal( attr(ten, "char"), "10" )
})
