#' emoji version of time
#'
#' @param time a `POSIXct` object
#'
#' @return an emoji clock that is the closest to the given time
#'
#' @examples
#' \dontrun{
#' clock( Sys.time() )
#' }
#'
#' @importFrom stringr str_detect
#' @export
clock <- function(time){
  hour   <- as.numeric( format(time, "%I") )
  minute <- as.numeric( format(time, "%M") )

  x <- round( 1 + 2*hour %% 12 + minute / 30 ) %% 24

  jis <- emo::jis
  idx <- which( jis$subgroup == "time" & str_detect(jis$name, "(o\\u2019clock|thirty)") )

  data <- jis[ idx[x], ]
  structure( data$emoji, time = time, data = data,  class = c("clock", "emoji" ) )

}

#' @export
print.clock <- function(x, ...){
  NextMethod()
  cat_discreet( "<clock for ", format( attr(x, "time"), "%H:%M" ), " (~", attr(x, "data")$name, ") >"  )
  invisible(x)
}
