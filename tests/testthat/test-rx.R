context( "regular expression" )

test_that( "ji_rx matches all fully qualified emojis in emo::jis", {

  # with stringr/icu
  expect_true(
    all( stringr::str_detect( emo::jis$emoji[emo::jis$qualified == "fully-qualified"], ji_rx) )
  )

  # with base R regex
  expect_true(
    all( grepl(ji_rx, emo::jis$emoji[emo::jis$qualified == "fully-qualified"]) )
  )

})

test_that("ji_detect detects all emojis", {
  expect_true( all( ji_detect( emo::jis$emoji[emo::jis$qualified == "fully-qualified"] ) ) )
})


