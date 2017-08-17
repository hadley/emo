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
    matches ,
    class = c("emoji_completion")
  )
}

#' @importFrom purrr walk2
#' @export
print.emoji_completion <- function(x, ...){
  writeLines( paste(x, " : ", names(x)) )
  invisible(x)
}
