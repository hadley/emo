
#' geometric emoji
#'
#' @rdname geometric
#' @param size size
#' @param color color
#'
#' @details
#' For `square`:
#' - `size` should be one of `c( "small", "medium", "medium-small", "large" )`
#' - `color` should be "white" or "black"
#'
#' For `diamond`
#' - `size` should be "large" or "small"
#' - `color` should be "orange" or "blue"
#'
#' @examples
#' \dontrun{
#' square( "small", "black" )
#' square( "large", "white" )
#'
#' diamond( "small", "orange")
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

#' @rdname geometric
#' @export
diamond <- function( size = c("large", "small"), color = c("orange", "blue") ){
  size <- match.arg(size)
  color <- match.arg(color)

  data <- emo::jis %>%
    filter( name == glue("{size} {color} diamond") )

  structure(
    data$emoji,
    class = c("diamond", "geometric", "emoji"),
    size = size,
    color = color
  )

}

#' @export
print.diamond <- function(x, ...) {
  NextMethod()
}
