context( "regular expression" )

test_that( "ji_rx matches all emojis in emo::jis", {
  # with stringr/icu
  expect_true(
    all( stringr::str_detect(jis$emoji, ji_rx) )
  )

  # with base R regex
  expect_true(
    all( grepl(ji_rx, jis$emoji) )
  )

})

test_that("ji_detect detects all emojis", {
  expect_true( all( ji_detect( emo::jis$emoji ) ) )
})


