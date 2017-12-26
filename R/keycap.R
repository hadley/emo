
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
#' @importFrom magrittr %>%
#' @importFrom dplyr filter slice
#' @export
keycap <- function( x ){
  choices <- c("#", "*", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  x <- as.character(x)
  assert_that( length(x) == 1)
  assert_that( x %in% choices )

  idx <- which( x == choices )
  data <- filter( emo::jis, subgroup == "keycap" ) %>%
    slice(idx)

  structure(
    data$emoji,
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
