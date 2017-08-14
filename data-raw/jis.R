library(tidyverse)
library(rvest)
library(stringi)

#' Parse Emoji List file
parse_emoji_list <- function(
  emoji_list = "http://unicode.org/emoji/charts/emoji-list.html",
  full_emoji_list = "http://unicode.org/emoji/charts/full-emoji-list.html"
) {

  # most of the information is in the first file
  html <- read_html(emoji_list)
  category <- html %>% html_nodes( "th.bighead" ) %>% html_text()
  subcategory <- html %>% html_nodes( "th.mediumhead" ) %>% html_text()

  table <- html %>%
    html_node("table") %>%
    html_table(header = FALSE)

  jis <- table %>%
    set_names( c("id", "runes", "sample", "name", "keywords") ) %>%
    select(-sample) %>%
      # lines not needed
    filter( runes != "Code" ) %>%
      # expand category
    mutate( category = case_when( id %in% category ~ id, TRUE ~  NA_character_ ) ) %>%
    fill( category ) %>%
    filter( id != category ) %>%
      # expand sub category
    mutate( subcategory = case_when( id %in% subcategory ~ id, TRUE ~  NA_character_ ) ) %>%
    fill( subcategory ) %>%
    filter( id != subcategory ) %>%
    mutate(
      id = as.numeric(id),
      keywords = str_split( keywords, " [|] "),
      emoji = str_replace_all( runes, "U[+]", "" ) %>%
        str_split( " ") %>%
        map( ~ strtoi(., base = 16) ) %>%
        stri_enc_fromutf32(),
      runes = str_split( runes, " "),
      nrunes = map_int( runes, length),
      name = str_replace( name, "⊛ ", ""),
      skin_tone = case_when(
        str_detect(name, ":.*skin tone$") ~ str_trim(str_replace_all(name, "^.*:(.*)skin tone", "\\1")),
        TRUE                              ~ NA_character_
      )
    ) %>%
    as_tibble() %>%
    select( id, emoji, name, category, subcategory, keywords, skin_tone, runes, nrunes )

  # but then the other file can be used to identify vendor coverage
  table <- read_html(full_emoji_list) %>% html_node("table")

  vendor <- function(table, idx = 4){
    selector <- sprintf( "tr td:nth-child(%d)", idx )
    table %>%
      html_nodes(selector) %>%
      str_detect("miss") %>%
      not()
  }

  vendors <- c(apple=4, google=5,twitter=6, one=7, facebook = 8, messenger = 9, samsung=10, windows=11) %>%
    map( ~vendor(table, .) ) %>%
    bind_cols()

  jis <- bind_cols( jis, vendors )
  jis
}


# read data from github/gemoji repo
parse_gemoji <- function(){
  data <- read_json("data-raw/gemoji/db/emoji.json")
  keep <- data %>% map( "emoji" ) %>% map_lgl(negate(is.null))
  data <- data[keep]

  gemoji <- tibble(
    emoji = map_chr(data, "emoji"),
    aliases = map(data, "aliases") %>% map( . %>% flatten_chr()),
    tags = map(data, "tags") %>% map( . %>% flatten_chr()),
    unicode_version = map_chr(data, "unicode_version"),
    ios_version = map_chr(data, "ios_version")
  ) %>%
    mutate( unicode_version = ifelse(unicode_version=="", NA, unicode_version) ) %>%
    mutate_at( vars(ends_with("version")), as.numeric )

  gemoji
}

jis <- parse_emoji_list()
gemoji <- parse_gemoji()

jis <- left_join( jis, gemoji, by = "emoji" )

# merge tags and keywords
jis <- jis %>%
  mutate( keywords = map2(keywords, tags, ~unique( c(.x, .y))) ) %>%
  select( -tags )

alias_from_name <- function(name){
  name %>%
    str_replace_all( "[Åã]", "a" ) %>%
    str_replace_all( "ç", "c" ) %>%
    str_replace_all( "é", "e" ) %>%
    str_replace_all( "í", "i") %>%
    str_replace_all( "ô", "o") %>%
    str_replace_all( "[^0-9a-zA-Z]", "_" ) %>%
    str_replace_all( "_+", "_")
}

jis <- jis %>%
  mutate( aliases = map2( alias_from_name(name), aliases, ~unique( c(.x, .y))) )%>%
  select( id:keywords, aliases, skin_tone:nrunes, unicode_version, ios_version, apple:windows )

use_data( jis, overwrite = TRUE)
