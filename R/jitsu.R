
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @importFrom rlang new_quosure is_formula quo quo_name enquo quo_expr is_symbol is_scalar_character is_unary_lang expr_name
jitsu_words_lookup <- function(s){
  s <- enquo(s)
  wrx <- paste0( "\\b", quo_name(s), "\\b" )

  quo(
    str_detect( name, !!wrx ) |
    map_lgl( aliases , ~ any(str_detect(., !!wrx)) )
  )
}

jitsu_regex_lookup <- function(q){
  q <- enquo(q)
  q <- new_quosure( quo_expr(q)[[2]], get_env(q) )

  s <- if( is_symbol(quo_expr(q)) ){
    quo_name(q)
  } else {
    quo_expr(q)
  }

  quo(
    str_detect( name, !!s ) |
    map_lgl( aliases , ~ any(str_detect(., !!s) ) )
  )
}

#' @importFrom rlang enquo
jitsu_filter_exprs <- function( q ){
  expr <- quo_expr(q)

  if( is_symbol(expr) ){
    q <- jitsu_words_lookup( !!quo_name(q) )
  } else if( is_scalar_character(expr) ){
    q <- jitsu_words_lookup( !!q )
  } else if( is_formula(expr) && length(expr) == 2L ){
    q <- jitsu_regex_lookup( !!q )
  }

  q
}

#' @rdname
#' @export
jitsu_quos <- function(...){
  map( quos(...), jitsu_filter_exprs )
}

#' @rdname jitsu
#' @export
jitsu_filter <- function( ... ){
  filter( emo::jis, !!!jitsu_quos(...) )
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
#' `jitsu_filter` reworks the `...` as above and eventually returns the results of [dplyr::filter()] on the [jis] tibble.
#'
#' `jitsu` selects one match at random between the results of `jitsu_filter`
#'
#' `jitsu_set` makes a vector of all the selected emojis
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
#' jitsu(subgroup == "face-fantasy" )
#'
#' # get all the results in a new tibble
#' jitsu_filter( monkey )
#' jitsu_filter( ~"^cat" )
#' jitsu_filter(hand, skin_tone == "light" )
#'
#' # just get the emojis
#' jitsu_set( monkey )
#' jitsu_set( ~"^cat" )
#' jitsu_set(hand, skin_tone == "medium-dark" )
#'
#'
#' }
#'
#' @importFrom rlang quos
#' @importFrom dplyr filter sample_n
#' @importFrom purrr map
#' @export
jitsu <- function( ... ){
  results <- jitsu_filter(...)
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
jitsu_set <- function(...){
  structure(
    c( jitsu_filter(...)$emoji ),
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

globalVariables( c("group", "keywords", "subgroup", "runes", "aliases") )
