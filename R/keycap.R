
#' Keycap emoji sequence
#'
#' @param x character to emoji keycap
#'
#' @return a keycap version of `x`
#'
#' @examples
#' \dontrun{
#'   keycap(3)
#'   keycap(10)
#'   keycap('#')
#' }
#'
#' @importFrom assertthat assert_that
#' @export
keycap <- function( x ){
  choices <- c("#", "*", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  x <- as.character(x)
  assert_that( length(x) == 1)
  assert_that( x %in% choices )

  jis <- emo::jis
  emoji <- jis$emoji[ jis$subgroup == "keycap" & jis$qualified == "fully-qualified" ][ x == choices]

  structure(
    emoji,
    char = x,
    class = c("keycap", "emoji")
  )
}

#' @export
print.keycap <- function( x, ...){
  NextMethod()
  cat_discreet("<keycap for '", attr(x, "char"), "'>\n")
  invisible(x)
}
