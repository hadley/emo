context("moon")

test_that( "1900/01/01 is new moon", {
  id <- function(x) attr(x, "data")$id

  expect_equal(
    attr( moon( day = 0 ), "data")$name,
    "new moon"
  )

  expect_equal(
    id(moon( day = 0 )),
    id(moon( date = lubridate::ymd("1900/01/01") ))
  )

})
