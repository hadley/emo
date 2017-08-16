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
  structure(
    unique( emo::ji_name[ str_detect(names(emo::ji_name), token ) ] ),
    class = c("completion", "emoji")
  )
}
