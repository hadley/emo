
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @importFrom rlang quo quo_name enquo quo_expr is_symbol is_scalar_character is_unary_lang expr_name
jitsu_simple_detect <- function(col, s){
  col <- enquo(col)
  s <- enquo(s)
  quo( str_detect( !!col, !!s) )
}

jitsu_multi_detect <- function(col, s){
  col <- enquo(col)
  s <- enquo(s)
  quo( map_lgl( !!col, ~ any(str_detect( ., !!s))) )
}

jitsu_large_detect <- function(s){
  s <- enquo(s)
  quo(
    str_detect( name, !!quo_name(s) ) |
    map_lgl( keywords, ~ any(str_detect( ., !!quo_name(s))) ) |
    map_lgl( aliases , ~ any(str_detect( ., !!quo_name(s))) )
  )
}

jitsu_filter_exprs <- function( q ){
  expr <- quo_expr(q)

  if( is_symbol(expr) ){
    q <- jitsu_large_detect( !!quo_name(q) )
  } else if( is_scalar_character(expr) ){
    q <- jitsu_large_detect( !!q )
  } else if( is_unary_lang(expr) ){
    q <- switch( expr_name(expr[[1]]),
      "name"        = jitsu_simple_detect( name    , !!expr[[2]] ),
      "category"    = jitsu_simple_detect( category, !!expr[[2]] ),
      "subcategory" = jitsu_simple_detect( subcategory, !!expr[[2]] ),
      "skin_tone"   = jitsu_simple_detect( skin_tone, !!expr[[2]] ),

      "keyword"     = jitsu_multi_detect( keywords, !!expr[[2]]),
      "runes"       = jitsu_multi_detect( runes, !!expr[[2]]),

      q
    )
  }

  q
}

#' @rdname jitsu
#' @export
ji_filter <- function( ... ){
  dots <- map( quos(...), jitsu_filter_exprs )
  filter( emo::jis, !!!dots )
}

#' extract an emoji
#'
#' @rdname jitsu
#' @param `...` set of filters, see details
#'
#' @details
#'
#' `...` can contain
#' - bare symbols or strings, to be looked for in the name or the keywords. Understood as regular expressions.
#' - calls to the pseudo-functions `name`, `category`, `subcategory`, `skin_tone`, `keyword`, `runes`. These accept regular expressions
#'   to match the relevant column, e.g. `name("^cat")` or `name( fixed("cat") )`
#' - Any other call are just used verbatim in `filter`
#'
#' `ji_filter` reworks the `...` as above and eventually returns the results of [dplyr::filter] on [jis]
#'
#' `jitsu` selects one match at random between the results of `ji_filter`
#'
#' `ji_set` makes a vector of all the selected emojis
#'
#' @examples
#' \dontrun{
#'
#' # look in name and keywords with simple syntax
#' jitsu(cat,face)
#' jitsu(tears)
#' jitsu(poop)
#' jitsu(sad,cat)
#' jitsu(monkey)
#'
#' # look only in specific columns
#' jitsu(name("cat"))
#' jitsu(skin_tone("^light"))
#' jitsu(subcategory("person-role"), skin_tone("medium-dark"))
#' jitsu(subcategory("face-fantasy") )
#'
#' ji_filter( monkey )
#' ji_filter( cat, face )
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

globalVariables( c("category", "keywords", "subcategory", "runes", "skin_tone") )
