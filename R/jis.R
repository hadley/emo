#' full list of emojis
#'
#' @source [Unicode® Emoji Charts v5.0](http://unicode.org/emoji/charts/index.html)
#'
#' @format tibble with columns
#' - id: identifier
#' - emoji: character representation of the emoji
#' - name: name
#' - group: group, e.g. "Smileys & People"
#' - subgroup: sub group, e.g. "face-positive"
#' - keywords: vector of keywords
#' - runes: vector of unicode runes, i.e. hexadecimal representations prefixed with "U+"
#' - nrunes: number of runes the emoji uses
#' - apple ... windows: logical indicating if the given vendor supports the emoji
"jis"

#' emoji names
"ji_name"

#' emoji keywords
"ji_keyword"
