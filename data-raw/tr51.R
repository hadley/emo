library(tidyverse)
library(glue)

parse_emoji_data <- function(file = "data-raw/unicode-tr51/data/emoji-data.txt"){
  explode_code <- function(code){
    map( str_split(code, "[.]{2}") , ~{
      if( length(.) == 1) {
        strtoi(., base = 16)
      } else {
        x <- strtoi(., base = 16)
        seq( x[1], x[2] )
      }
    })
  }

  read_lines( file) %>%
    str_subset( "^[0-9A-F]" ) %>%
    tibble( txt = .) %>%
    separate(txt, into = c("code", "type_field"), sep = "[;#]", extra = "drop" )%>%
    mutate_all(str_trim) %>%
    mutate(points = explode_code(code)) %>%
    group_by(type_field) %>%
    summarise( points = list(flatten_int(points)) )

}

parse_emoji_sequence <- function(file ){
  read_lines(file) %>%
    str_subset( "^[0-9A-F]" ) %>%
    tibble( txt = .) %>%
    separate(txt, into = c("code", "type_field"), sep = "[;#]", extra = "drop" )%>%
    mutate_all(str_trim) %>%
    mutate(
      points = map( str_split(code, " "), strtoi, base = 16 ),
      nrunes = map_int(points, length)
    ) %>%
    arrange( desc(nrunes) )

}

emoji_data <- parse_emoji_data()



# emoji_sequences <- bind_rows(
#   parse_emoji_sequence("data-raw/unicode-tr51/data/emoji-sequences.txt"),
#   parse_emoji_sequence("data-raw/unicode-tr51/data/emoji-zwj-sequences.txt")
# ) %>%
#   mutate( nrunes = map_int(points, length)) %>%
#   arrange( desc(nrunes) )
#
# sequences_rx <- stri_enc_fromutf32( emoji_sequences$points ) %>%
#   str_replace( "[*]", "[*]") %>%
#  paste( collapse = "|" )


emoji_sequences <- parse_emoji_sequence("data-raw/unicode-tr51/data/emoji-sequences.txt")

rx_keycap <- emoji_sequences %>%
  filter( type_field == "Emoji_Keycap_Sequence" ) %>%
  pull(points) %>%
  map_int(1) %>%
  stri_enc_fromutf32() %>%
  paste0("[", ., "]\uFE0F\u20E3")

rx_flags <- "[\U1F1E6-\U1F1FF]{2}"

