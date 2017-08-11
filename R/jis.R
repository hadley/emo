#' full list of emojis
#'
#' @source [Unicode® Emoji Charts v5.0](http://unicode.org/emoji/charts/index.html)
#'
#' @format tibble with columns
#' - id: identifier
#' - emoji: character representation of the emoji
#' - name: name
#' - category: category, e.g. "Smileys & People"
#' - subcategory: sub category, e.g. "face-positive"
#' - keywords: vector of keywords
#' - skin_tone: when applicable the skin tone, otherwise NA
#' - runes: vector of unicode runes, i.e. hexadecimal representations prefixed with "U+"
#' - nrunes: number of runes the emoji uses
#' - apple ... windows: logical indicating if the given vendor supports the emoji
"jis"
