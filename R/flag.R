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
  data <- filter( jis, category == "Flags", str_detect(name, pattern) )
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
#' @export
print.flag <- function(x, ... ){
  # discreet <- make_style( "#E4E4E4", grey = TRUE )
  discreet <- silver
  NextMethod()
  cat( discreet( paste0( "<flag for '", attr(x, "name"), "'>\n"  ) ) )
  invisible(x)
}

