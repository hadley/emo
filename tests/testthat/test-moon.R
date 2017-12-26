context("moon")

test_that( "1900/01/01 is new moon", {

  expect_equal(
    attr( moon( day = 0 ), "data")$name,
    "new moon"
  )

  expect_equal(
    attr( moon( day = 0 ), "day") ,
    attr( moon( date = lubridate::ymd("1900/01/01") ), "day" )
  )

})
