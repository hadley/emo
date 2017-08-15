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

  vendors <- c(vendor_apple=4, vendor_google=5,vendor_twitter=6, vendor_one=7,
    vendor_facebook = 8, vendor_messenger = 9, vendor_samsung=10, vendor_windows=11
    ) %>%
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

merge_names <- function(x, y){
  all_y <- y[ map_lgl(y, negate(is.null)) ] %>% flatten_chr
  map2(x, y, ~unique( c(.x[ ! .x %in% all_y ], .y) ) )
}

alias_from_name <- function(name){
  name %>%
    str_replace_all( "[Åã]", "a" ) %>%
    str_replace_all( "ç", "c" ) %>%
    str_replace_all( "é", "e" ) %>%
    str_replace_all( "í", "i") %>%
    str_replace_all( "ô", "o") %>%
    str_replace_all("[*]", "star") %>%
    str_replace_all("[#]", "hash") %>%
    str_replace_all("[&]", "and") %>%
    str_replace_all( "[^0-9a-zA-Z]", "_" ) %>%
    str_replace_all( "_+", "_")
}

jis_ <- parse_emoji_list()
gemoji <- parse_gemoji()

jis <- left_join( jis_, gemoji, by = "emoji" ) %>%
  mutate( keywords = map2(keywords, tags, ~c(.x,.y) ) ) %>%
  select( -tags ) %>%
  mutate( aliases = merge_names( alias_from_name(name), aliases ) ) %>%
  select( id:keywords, aliases, skin_tone:nrunes, unicode_version, ios_version, starts_with("vendor") )

# get extra names from emojilib
all_alias <- flatten_chr(jis$aliases)
emojilib <- read_json("data-raw/emojilib/emojis.json")
keep <- emojilib %>% map( "char" ) %>% map_lgl(negate(is.null))
emojilib <- emojilib[keep]
emojilib_tbl <- tibble(
  emojilibname = names(emojilib),
  emoji = emojilib %>% map_chr( "char" )
  ) %>%
  filter( ! emojilibname %in% all_alias )

jis <- left_join( jis, emojilib_tbl, by = "emoji" ) %>%
  mutate( aliases = map2(aliases, emojilibname, ~{ res <- c(.x, .y); res[!is.na(res)] } ) ) %>%
  select(-emojilibname)

use_data( jis, overwrite = TRUE)

aliases <- jis %>%
  select( emoji, aliases) %>%
  unnest

ji_alias <- set_names(aliases$emoji, aliases$aliases )
use_data( ji_alias, overwrite = TRUE)

