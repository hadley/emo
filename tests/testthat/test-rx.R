context( "regular expression" )

test_that( "ji_rx matches all emojis in emo::jis", {
  expect_true(
    all( stringr::str_detect(jis$emoji, ji_rx) )
  )

  expect_true(
    all( grepl(ji_rx, jis$emoji) )
  )

})

