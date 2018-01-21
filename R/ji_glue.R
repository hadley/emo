#' @importFrom glue collapse evaluate
emoji_transformer <- function(code, envir) {
  has_dollar <- grepl("[*]$", code)
  if( has_dollar ) code <- sub("[*]$", "", code)
  res <- evaluate( glue( "emo::jitsu_set({code})" ), envir )

  if (has_dollar) {
    collapse(res)
  } else {
    sample(res, 1)
  }
}

#' emoji glue
#'
#' @param txt character vector in which to replace `:something:` by an
#' emoji for `something`.
#'
#' This is insipred from [glue::glue()] but uses `:` instead of braces
#' because that's typically how emojis are
#' [expressed](https://www.webpagefx.com/tools/emoji-cheat-sheet/)
#'
#' @examples
#' \dontrun{
#'   # simple syntax, uses `ji`
#'   ji_glue( ":cat:s love to :zzz:" )
#'
#'   # we can also use the `jitsu` syntax (with `~` and `,`)
#'   ji_glue( ":~cat,face: love to :~sleep:" )
#'
#'   # and get sets of emojis instead of just one by
#'   # using the wildcard suffix
#'   ji_glue( ":monkey,face: love to :party:" )
#'
#' }
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @export
ji_glue <- function(txt){
  .envir <- parent.frame()
  structure(
    map_chr(txt, ~glue(., .open = ":", .close = ":", .envir = .envir, .transformer = emoji_transformer)),
    class = "emoji"
  )
}


