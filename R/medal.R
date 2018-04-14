
#' medals
#'
#' @param position 1, 2, 3, or `first`, `second`, `third`, or `gold`, `silver`, `bronze`
#'
#' @examples
#' \dontrun{
#' medal(gold)
#' medal(third)
#' medal(2)
#' }
#'
#' @importFrom rlang eval_tidy enquo
#' @export
medal <- function( position ){
  position <- enquo(position)

  aliases <- list(
    first = 1,
    second = 2,
    third = 3,

    gold = 1,
    silver = 2,
    bronze = 3
  )

  names <- c("1st place medal", "2nd place medal", "3rd place medal")

  pos <- eval_tidy( position, data = aliases )
  data <- emo::jis[ emo::jis$name == names[pos], ]

  structure( data$emoji,
    class = c("medal", "emoji"),
    data = data
  )

}

#' @export
print.medal <- function(x, ...){
  NextMethod()
}
