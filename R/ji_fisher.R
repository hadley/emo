#' Convert text to emoji (by letter)
#'
#' @param x Character string. Text you'd like to be emojified.
#'
#' @return Character string of emoji.
#' @export
#'
#' @examples
#' ji_fisher("Carrie Fisher is a delight")
ji_fisher <- function(x) {
  x <- unlist(strsplit(x, ""))
  x <- purrr::map_chr(x, get_emoji)
  structure(
    paste0(x, collapse = ""),
    class = "emoji")
}

get_emoji <- function(x) {
  emojis <- emo::fisher_lst[[tolower(x)]]
  if (length(emojis) == 0L) {
    return(x)
  }
  sample(emojis, 1)
}

#' Letter to emoji list
#'
#'
#' @format List
"fisher_lst"

#' @export
print.fisher_alphabet <- function(x, ...){
  letters <- names(x)
  emojis  <- map_chr( x, paste, collapse = " ")
  writeLines( paste( letters, "  :  ", emojis) )
  invisible(x)
}
