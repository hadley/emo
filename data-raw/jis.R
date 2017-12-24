library(tidyverse)
library(rvest)
library(stringi)
library(jsonlite)
library(magrittr)
library(devtools)

#' Parse Emoji List file
parse_emoji_list <- function(
  emoji_list = "http://unicode.org/emoji/charts-11.0/emoji-list.html",
  full_emoji_list = "http://unicode.org/emoji/charts-11.0/full-emoji-list.html",
  emoji_variants = "http://unicode.org/emoji/charts-11.0/emoji-variants.html"
) {

  # Emoji Presentation sequences
  #
  # some of these are not included in the other files
  # or with alternative representation
  variants_tab <- read_html(emoji_variants) %>%
    html_node("table") %>%
    html_table() %>%
    select(c(1L,5L)) %>%
    as_tibble() %>%
    set_names( c("code", "name" )) %>%
    filter( ! str_detect(name, "[*]$") ) %>%
    mutate(
      runes = map( code, ~ c( paste0("U+", .), "U+FE0F") ),
      name = str_to_lower(name),
      emoji = runes %>%
        map( ~ strtoi(str_replace_all(., "^U[+]", ""), base = 16) ) %>%
        stri_enc_fromutf32(),
      category = "presentation",
      subcategory = "presentation",
      keywords = str_split( name, " " )
    )

  # most of the information is in the first file: emoji-list.html
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

  tr <- table %>%
    html_nodes( xpath = "//tr/td[@class='code']/.." )
  sizes <- tr %>% map_int(xml_length)

  vendor <- function(idx = 4){
    selector <- sprintf( "td:nth-child(%d)", idx )
    # not supported by default
    # this civers the merged cells case when no vendor supports the emoji
    out <- rep( FALSE, length(tr))

    # when theree's info for all vendors, the emoji is supported if there's no
    # miss in the node
    out[ sizes == 15 ] <- tr[ sizes == 15 ] %>%
      html_nodes(selector) %>%
      str_detect("miss") %>%
      not()
    out
  }

  vendors <- c(vendor_apple=4, vendor_google=5,vendor_twitter=6, vendor_one=7,
    vendor_facebook = 8, vendor_messenger = 9, vendor_samsung=10, vendor_windows=11
    ) %>%
    map( vendor ) %>%
    bind_cols()

  jis <- bind_cols( jis, vendors )
  jis <- bind_rows( jis, variants_tab )
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
    tags = map(data, "tags") %>% map( ~ if(is.list(.)) flatten_chr(.) else character() ),
    unicode_version = map_chr(data, "unicode_version"),
    ios_version = map_chr(data, "ios_version")
  ) %>%
    mutate( unicode_version = ifelse(unicode_version=="", NA, unicode_version) ) %>%
    mutate_at( vars(ends_with("version")), as.numeric )

  gemoji
}

merge_names <- function(x, y){
  all_y <- y[ map_lgl(y, negate(is.null)) ] %>% flatten_chr
  map2(x, y, ~unique( c(.y, .x[ ! .x %in% all_y ] ) ) )
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


gemoji <- parse_gemoji()

jis_ <- parse_emoji_list()

jis <- left_join( jis_, gemoji, by = "emoji" ) %>%
  mutate( keywords = map2(keywords, tags, c ) ) %>%
  select( -tags ) %>%
  mutate( aliases = merge_names( alias_from_name(name), aliases ) ) %>%
  select( id:keywords, aliases, skin_tone:nrunes, unicode_version, ios_version, starts_with("vendor") )

# get extra names from emojilib
all_alias <- flatten_chr(jis$aliases)
emojilib <- read_json("data-raw/emojilib/emojis.json")
keep <- emojilib %>%
  map( "char" ) %>%
  map_lgl(negate(is.null))

emojilib <- emojilib[keep]
emojilib_tbl <- tibble(
    emoji = emojilib %>% map_chr( "char" ),
    emojilibname = names(emojilib),
    emojilibkeyword = emojilib %>% map("keywords") %>% map(. %>% flatten_chr())
  )

jis <- left_join( jis, emojilib_tbl, by = "emoji" ) %>%
  mutate(
    aliases  = map2(aliases, emojilibname, ~{ res <- c(.x, setdiff(.y, all_alias) ); res[!is.na(res)] } ),
    keywords = map2(keywords, emojilibkeyword, unique )
  ) %>%
  select(-emojilibname, -emojilibkeyword)


use_data( jis, overwrite = TRUE)

aliases <- jis %>%
  select( emoji, aliases) %>%
  unnest

ji_name <- set_names(aliases$emoji, aliases$aliases )
use_data( ji_name, overwrite = TRUE)

kw <- select( jis, keywords, aliases ) %>%
  filter( map_int(aliases, length) > 0 ) %>%
  mutate( aliases = map_chr(aliases, 1)) %>%
  unnest() %>%
  group_by( keywords ) %>%
  summarise( name = list(c(aliases)) )

aliases <- unique( flatten_chr(jis$aliases) )
aliases <- aliases[ !aliases %in% kw$keywords ]

kw <- bind_rows( kw, tibble(keywords = aliases, name = as.list(aliases) ) )

ji_keyword <- set_names( kw$name, kw$keywords )
use_data( ji_keyword, overwrite = TRUE)

