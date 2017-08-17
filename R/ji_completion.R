#' emoji completion
#'
#' @param token start of an emoji alias
#'
#' @examples
#' ji_completion( "key" )
#'
#' @importFrom stringr str_detect
#' @export
ji_completion <- function( token ){
  matches <- emo::ji_name[ str_detect(names(emo::ji_name), token ) ]
  matches <- matches[ !duplicated(matches) ]
  structure(
    unique( paste0( matches, " :", names(matches), ":" ) ),
    class = c("emoji_completion")
  )
}

#' @importFrom purrr walk
#' @export
print.emoji_completion <- function(x, ...){
  walk(x, writeLines )
  invisible(x)
}
