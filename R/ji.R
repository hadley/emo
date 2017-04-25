#' Find a single emoji
#'
#' @param keyword Either name or keyword. If more than one emoji has the
#'   specified keyword, will pick one at random.
#' @export
#' @examples
#' emo::ji("banana")
#' emo::ji("monkey")
ji <- function(keyword) {
  knitr::asis_output(find_emoji(keyword))
}

find_emoji <- function(keyword) {
  stopifnot(is.character(keyword), length(keyword) == 1)

  # First look in names
  if (keyword %in% names(emoji_name)) {
    return(emoji_name[[keyword]])
  }

  if (keyword %in% names(emoji_keyword)) {
    candidates <- emoji_keyword[[keyword]]
    name <- sample(candidates, 1)

    return(emoji_name[[name]])
  }

  stop("Couldn't find emoji '", keyword, "'")
}
