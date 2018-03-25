context( "emoji coverage" )

test_that( "dark_sunglasses is known (#36)", {
  expect_equal( unclass(ji("dark_sunglasses")), "\U0001f576" )
})

