#' @importFrom glue glue_collapse
emoji_transformer <- function(text, envir) {
  has_star <- grepl("[*]$", text)
  if (has_star) {
    text <- sub("[*]$", "", text)
    glue_collapse(ji_find(text)$emoji)
  } else {
    ji(text)
  }
}

#' emoji glue
#'
#' @param \dots strings to format, where `:x:` is replaced by an emoji for "x", using [ji()] and `:y*:` is replaced by all
#' emojis that match "y", using [ji_find()].
#' @param .envir see [glue::glue()]
#'
#' @seealso [glue::glue()] for how the strings are concatenated
#'
#' @examples
#' \dontrun{
#'   ji_glue("one :heart:")
#'   ji_glue("many :heart*:")
#' }
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @export
ji_glue <- function(..., .envir=parent.frame()){
  glue(..., .open = ":", .close = ":", .envir = .envir, .transformer = emoji_transformer)
}


