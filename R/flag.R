#' Flag emoji
#'
#' @param pattern pattern suitable for [stringr::str_detect] matched against the name of the flag
#'
#' @return If the pattern matches a single flag, the emoji ( with classes "flag" and "emoji" ) is returned
#'
#' @examples
#' \dontrun{
#'
#' flag( "^Fra" )
#' flag( "New Zealand" )
#'
#' # name of all the flags
#' if( require(dplyr) ){
#'  emo::jis %>%
#'    filter( category == "Flags" ) %>%
#'    pull(name)
#' }
#'
#' }
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
flag <- function( pattern ){
  data <- filter( emo::jis, category == "Flags", str_detect(name, pattern) )
  n <- nrow(data)
  if( n == 0 ){
    stop( glue( "Cannot find flag '{pattern}' ") )
  } else if( n > 1){
    flags <- paste("'", data$name, "'", collapse = ", ", sep = "" )
    stop( glue( "Query not specific enough, found {n} flags: {flags}"))
  }

  structure( data$emoji, name = data$name, data = data, class = c("flag", "emoji") )
}


#' @importFrom crayon silver
cat_discreet <- function(...){
  cat( silver( paste0(...) ) )
}

#' @export
print.flag <- function(x, ... ){
  discreet <- silver
  NextMethod()
  cat_discreet("<flag for '", attr(x, "name"), "'>\n")
  invisible(x)
}

#' @importFrom utils globalVariables
globalVariables( c("category", "name") )

