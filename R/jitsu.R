
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @importFrom rlang new_quosure is_formula quo quo_name enquo quo_expr is_symbol is_scalar_character is_unary_lang expr_name
jitsu_words_lookup <- function(s){
  s <- enquo(s)
  wrx <- paste0( "\\b", quo_name(s), "\\b" )

  quo(
    str_detect( name, !!wrx ) |
    map_lgl( keywords, ~ any(. == !!quo_name(s)) ) |
    map_lgl( aliases , ~ any(. == !!quo_name(s)) )
  )
}

jitsu_regex_lookup <- function(expr){
  s <- new_quosure(expr[[2]])
  if( is_symbol(quo_expr(s)) ){
    s <- quo_name(s)
  } else {
    s <- quo_expr(s)
  }

  quo(
    str_detect( name, !!s ) |
    map_lgl( keywords, ~ any(str_detect(., !!s) ) ) |
    map_lgl( aliases , ~ any(str_detect(., !!s) ) )
  )
}


jitsu_filter_exprs <- function( q ){
  expr <- quo_expr(q)

  if( is_symbol(expr) ){
    q <- jitsu_words_lookup( !!quo_name(q) )
  } else if( is_scalar_character(expr) ){
    q <- jitsu_words_lookup( !!q )
  } else if( is_formula(expr) && length(expr) == 2L ){
    q <- jitsu_regex_lookup( expr )
  }

  q
}

#' @rdname jitsu
#' @export
ji_filter <- function( ... ){
  dots <- map( quos(...), jitsu_filter_exprs )
  filter( emo::jis, !!!dots )
}

#' find emoji
#'
#' @rdname jitsu
#' @param ... set of filters, see details
#'
#' @details
#'
#' `...` can contain
#' - bare symbols or strings, e.g. `cat` or `"cat"`. This will look for an exact word match in the name, the aliases or the keywords.
#' - one sided formulas, e.g. `~cat`, `~"cat"`, `~regex("cat")`. The rhs of the formula is a regular expression
#'   suitable for [stringr::regex()] matched against the name
#' - Any other call are just used verbatim in `filter`
#'
#' `ji_filter` reworks the `...` as above and eventually returns the results of [dplyr::filter()] on the [jis] tibble.
#'
#' `jitsu` selects one match at random between the results of `ji_filter`
#'
#' `ji_set` makes a vector of all the selected emojis
#'
#' @examples
#' \dontrun{
#'
#' # look these words in name, aliases and keywords
#' jitsu(cat,face)
#' jitsu(tears)
#' jitsu(poop)
#' jitsu(sad,cat)
#' jitsu(monkey)
#'
#' # regex lookup
#' jitsu(~cat)
#' jitsu(~"^cat")
#' jitsu(~regex("^cat"))
#'
#' # usual filter
#' jitsu(name == "cat")
#' jitsu(skin_tone == "light" )
#' jitsu(subcategory == "person-role", skin_tone == "medium-dark" )
#' jitsu(subcategory == "face-fantasy" )
#'
#' # get all the results in a new tibble
#' ji_filter( monkey )
#' ji_filter( ~"^cat" )
#' ji_filter(hand, skin_tone == "light" )
#'
#' # just get the emojis
#' ji_set( monkey )
#' ji_set( ~"^cat" )
#' ji_set(hand, skin_tone == "medium-dark" )
#'
#'
#' }
#'
#' @importFrom rlang quos
#' @importFrom dplyr filter sample_n
#' @importFrom purrr map
#' @export
jitsu <- function( ... ){
  results <- ji_filter(...)
  data    <- sample_n(results, 1 )

  structure( data$emoji,
    data = data,
    results = results,
    call = match.call(),
    class = c("jitsu", "emoji")
  )
}

#' @rdname jitsu
#' @export
ji_set <- function(...){
  structure(
    c( ji_filter(...)$emoji ),
    class = "emoji"
  )
}


#' @export
print.jitsu <- function(x, ...){
  NextMethod()
  cat_discreet( glue( "<emoji '{name}' selected from {n} matches>",
    name = attr(x, "data")$name,
    n    = nrow(attr(x, "results"))
  ))
  invisible(x)
}

globalVariables( c("category", "keywords", "subcategory", "runes", "skin_tone", "aliases") )
