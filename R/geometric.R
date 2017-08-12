
#' square emoji
#'
#' @param size size of the square ("small", "medium", "medium-small" or "large")
#' @param color "white" or "black"
#'
#' @examples
#' \dontrun{
#' square( "small", "black" )
#' square( "large", "white" )
#' }
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom glue glue
#'
#' @export
square <- function(size = c( "small", "medium", "medium-small", "large" ), color = c( "white", "black") ){
  size <- match.arg(size)
  color <- match.arg(color)

  data <- emo::jis %>%
    filter( name == glue("{color} {size} square") )

  structure(
    data$emoji,
    class = c("square", "geometric", "emoji"),
    size = size,
    color = color
  )

}

#' @export
print.square <- function(x, ...) {
  NextMethod()
}
