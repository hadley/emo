library(tidyverse)
library(stringi)
library(rlang)
library(rvest)
library(xml2)
library(jsonlite)
library(devtools)

test <- read_lines("data-raw/tr51/emoji-test.txt") %>%
  str_subset( "^(# (sub)?group:|[^#].*;.*#.*)" ) %>%
  tibble( txt = .) %>%
  mutate(
    group = case_when(
      str_detect(txt, "# group")    ~ str_replace(txt, "^.*:", ""),
      TRUE                          ~ NA_character_
    ),
    subgroup = case_when(
      str_detect(txt, "# subgroup") ~ str_replace(txt, "^.*:", ""),
      TRUE                          ~ NA_character_
    )
  ) %>%
  fill(group, subgroup) %>%
  filter( !str_detect(txt, "^#") ) %>%
  separate( txt, into = c("runes", "qualified", "description" ), sep = "[;#]", extra = "merge") %>%
  mutate_all( str_trim ) %>%
  separate( description, sep = " ", into = c("emoji", "name"), extra = "merge")

ordering <- read_lines("data-raw/tr51/emoji-ordering.txt") %>%
  str_subset( "^[^#].*;.*#.*" ) %>%
  tibble( txt = .) %>%
  separate( txt, into = c("runes", "version", "description" ), sep = "[;#]", extra = "merge") %>%
  mutate_all( str_trim ) %>%
  separate( description, sep = " ", into = c("emoji", "name"), extra = "merge") %>%
  select(-runes, -name)

data1 <- left_join( test, ordering, by = "emoji" ) %>%
  mutate(
    points = map( str_split(runes, " "), strtoi, base = 16),
    nrunes = map_int(points, length)
  ) %>%
  filter( qualified == "fully-qualified") %>%
  select( -qualified )

# fetch vendor information from full-emoji-list.html

vendor_information <- function(){
  if( !file.exists( "data-raw/emoji11/full-emoji-list.html" ) ){
    download.file("https://www.unicode.org/emoji/charts-11.0/full-emoji-list.html", destfile = "data-raw/emoji11/full-emoji-list" )
  }
  table <- read_html("data-raw/emoji11/full-emoji-list.html") %>%
    html_node("table")

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

  emojis <- tr %>%
    html_nodes( "td:nth-child(2)" ) %>%
    html_text() %>%
    str_replace_all("U[+]", "") %>%
    str_split(" ") %>%
    map( strtoi, base = 16 ) %>%
    stri_enc_fromutf32() %>%
    tibble( emoji = .)

  bind_cols( emojis, vendors)

}

vendors <- vendor_information()
data2 <- left_join( data1, vendors, by = "emoji" )


#### extract keywords from emoji-list.html
keywords_information <- function(){
  if( !file.exists( "data-raw/emoji11/emoji-list.html" ) ){
    download.file("https://www.unicode.org/emoji/charts-11.0/emoji-list.html", destfile = "data-raw/emoji11/emoji-list.html" )
  }

  html <- read_html("data-raw/emoji11/emoji-list.html")
  category <- html %>% html_nodes( "th.bighead" ) %>% html_text()
  subcategory <- html %>% html_nodes( "th.mediumhead" ) %>% html_text()

  html %>%
    html_node("table") %>%
    html_table(header = FALSE) %>%
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
      keywords = str_split( keywords, " [|] "),
      emoji = str_replace_all( runes, "U[+]", "" ) %>%
        str_split( " ") %>%
        map( strtoi, base = 16 ) %>%
        stri_enc_fromutf32()
    ) %>%
    select( emoji, keywords )

}
keywords <- keywords_information()
data3 <- left_join( data2, keywords, by = "emoji" )

##### extract information from gemoji
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
data4 <- left_join( data3, gemoji, by = "emoji" ) %>%
  mutate( keywords = map2(keywords, tags, c ) ) %>%
  select( -tags ) %>%
  mutate( aliases = merge_names( alias_from_name(name), aliases ) )


#### extract information from emojilib
all_alias <- flatten_chr(data4$aliases)
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

data5 <- left_join( data4, emojilib_tbl, by = "emoji" ) %>%
  mutate(
    aliases  = map2(aliases, emojilibname, ~{ res <- c(.x, setdiff(.y, all_alias) ); res[!is.na(res)] } ),
    keywords = map2(keywords, emojilibkeyword, unique )
  ) %>%
  select(-emojilibname, -emojilibkeyword)

jis <- data5

use_data( jis, overwrite = TRUE)

aliases <- jis %>%
  select( emoji, aliases) %>%
  unnest

ji_name <- set_names(aliases$emoji, aliases$aliases )
use_data( ji_name, overwrite = TRUE)

kw <- select( jis, keywords, aliases ) %>%
  filter( map_int(aliases, length) > 0 ) %>%
  mutate(
    aliases = map_chr(aliases, 1),
    keywords = map( keywords, ~data.frame(keywords = ., stringsAsFactors = FALSE) )
  ) %>%
  unnest() %>%
  group_by( keywords ) %>%
  summarise( name = list(c(aliases)) )

aliases <- unique( flatten_chr(jis$aliases) )
aliases <- aliases[ !aliases %in% kw$keywords ]

kw <- bind_rows( kw, tibble(keywords = aliases, name = as.list(aliases) ) )

ji_keyword <- set_names( kw$name, kw$keywords )
use_data( ji_keyword, overwrite = TRUE)



