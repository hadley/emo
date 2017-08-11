context("flag")

test_that("flag errors if pattern does not identify a single name", {
  expect_error( flag( "fr" ) )
  expect_error( flag( "fezffez" ) )
})

test_that("flag keeps some metadata", {
  chile <- flag( "Chile")
  expect_equal( attr(chile, "name"), "Chile" )
})

