
# see https://en.wikipedia.org/wiki/Lunar_phase
synodic_month <- 29.530588853

#' Calculate the number of days in the synodic month
#'
#' This uses the approximation described in the
#' [lunar phase wikipedia page](https://en.wikipedia.org/wiki/Lunar_phase), i.e.
#' the number of days since `1900/01/01` modulo the length of
#' a synodic month (`29.530588853` days)
#'
#' @param date a date
#'
#' @examples
#' \dontrun{
#' day_in_synodic_cycle( today() )
#' }
#' @importFrom lubridate ymd
#' @export
day_in_synodic_cycle <- function(date){
  days <- as.numeric( difftime( date , ymd( "1900/01/01"), units = "days" ) )
  days %% synodic_month
}

#' moon phase
#'
#' @param date a date
#' @param day number of days since new moon
#'
#' If not supplied, `day` is calculated using the
#' approximation of [day_in_synodic_cycle], i.e the number of
#' days since a known new moon modulo `29.530588853` days
#'
#' @rdname moon
#' @return a moon emoji
#' @export
#'
#' @examples
#' \dontrun{
#' moon( today() )
#' }
moon <- function( date, day = day_in_synodic_cycle(date) ){
  assert_that( day >= 0 & day <= synodic_month )

  idx <- 1 + (day / synodic_month * 8) %% 8

  data <- emo::jis %>%
    filter( str_detect( name, "moon$" ) ) %>%
    slice(idx)

  structure( data$emoji,
    class = c("moon", "emoji"),
    day = day,
    data = data
  )

}

#' @export
print.moon <- function( x, ...){
  NextMethod()
  cat_discreet( glue( "<moon phase '{phase}' ({days} days since new moon)",
    phase = attr(x, "data")$name,
    days = round(attr(x, "day"))
  ) )
  invisible(x)
}

