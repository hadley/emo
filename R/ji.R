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


#' List all emoji with a given keyword
#'
#' Note that this is unlikely to print correctly on your R console, but
#' it will work in (e.g.) the RStudio viewer.
#'
#' @param keyword Emoji keyword
#' @export
#' @examples
#' emo::ji_find("happy")
ji_find <- function(keyword) {
  names <- emoji_keyword[[keyword]]
  if (length(names) == 0) {
    stop("Couldn't find any emoji with '", keyword, "'")
  }

  tibble::tibble(
    name = names,
    emoji = unname(emoji_name[names])
  )
}
