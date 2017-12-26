#' A regular expression to catch all emojis
"ji_rx"

#' Detect the presence or absence of emojis in a string
#'
#' Vectorised over `string`
#'
#' @param string Input vector. Either a character vector, or something coercible to one
#' @return A logical vector
#' @seealso [stringr::str_detect()]
#'
#' @importFrom stringr str_detect
#' @export
ji_detect <- function(string) str_detect(string, ji_rx)

#' Keep strings containing an emoji, or find positions
#'
#' @param string input vector
#' @return A character vector
#'
#' @seealso [stringr::str_subset()]
#' @importFrom stringr str_subset
#' @export
ji_subset <- function(string) str_subset(string, ji_rx)

#' @rdname ji_subset
#' @export
ji_which <- function(string) str_which(string, ji_rx)

#' Count the number of emojis in a string
#'
#' Vectorised over `string`
#'
#' @param string Input vector
#' @return An integer vector
#' @importFrom stringr str_count
#' @export
ji_count <- function(string) str_count(string, ji_rx)

#' Extract emojis from a string
#'
#' vectorised over `string`
#'
#' @param string Input vector.
#' @param simplify see [stringr::str_extract_all()]
#'
#' @seealso [stringr::str_extract()] and [stringr::str_extract_all()]
#' @return A character vector
#' @importFrom stringr str_extract str_extract_all
ji_extract <- function(string) str_extract(string, ji_rx)

#' @rdname ji_extract
#' @export
ji_extract_all <- function(string, simplify = FALSE) str_extract_all(string, ji_rx, simplify)

#' Extract emojis from a string
#'
#' Vectorized over `string`
#'
#' @param string Input vector
#' @return see [stringr::str_match()]
#'
#' @seealso [stringr::str_match]
#' @importFrom stringr str_match str_match_all
#' @export
ji_match <- function(string) str_match(string, ji_rx)

#' @rdname ji_match
#' @export
ji_match_all <- function(string) str_match_all(string, ji_rx)

#' Replace emojis in a string
#'
#' Vectorised over `string` and `replacement`
#'
#' @param string Input vector
#' @param replacement A character vector of replacements. Should either be
#'   of length 1 or the same length as `string`. See [stringr::str_replace()] for details
#'
#' @return A character vector
#' @importFrom stringr str_replace str_replace_all
ji_replace <- function( string, replacement) str_replace(string, ji_rx, replacement)

#' @rdname ji_replace
#' @export
ji_replace_all <- function(string, replacement) str_replace_all(string, ji_rx, replacement)

#' Lodate the positio of emojis in a string
#'
#' Vectorised over `string`
#'
#' @param string Input vector
#' @return For `ji_locate` an integer matrix, for `ji_locate_all` a list
#'   of integer matrices
#'
#' @importFrom stringr str_locate str_locate_all
ji_locate <- function(string) str_locate(string, ji_rx)

#' @rdname ji_locate
#' @export
ji_locate_all <- function(string) str_locate_all(string, ji_rx)
