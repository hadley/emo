#' Summarise your p-values with emoji
#'
#' @param x A vector of p-values.
#' @export
#' @examples
#' emo::ji_p(1)
#' emo::ji_p(0.1)
#' emo::ji_p(0.05)
#' emo::ji_p(0.01)
#' emo::ji_p(1e-6)
#'
#' emo::ji_p(rbeta(50, 2, 5))
ji_p <- function(x) {
  stopifnot(is.numeric(x))

  out <- stats::symnum(x,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 1e-5, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c(ji("laughing"), ji("joy"), ji("grin"), ji("smile"), ji("thinking"), ji("poop"))
  )

  structure(out, class = c("emoji", class(out)))

}
