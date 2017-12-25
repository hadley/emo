library(tidyverse)
library(glue)
library(assertthat)

parse_emoji_data <- function(file = "data-raw/unicode-tr51/data/emoji-data.txt"){
  read_lines( file) %>%
    str_subset( "^[0-9A-F]" ) %>%
    tibble( txt = .) %>%
    separate(txt, into = c("code", "type_field"), sep = "[;#]", extra = "drop" )%>%
    mutate_all(str_trim) %>%
    mutate( points = map( str_split(code, "[.]{2}"), strtoi, base = 16) ) %>%
    group_by(type_field)
}

parse_emoji_sequence <- function(file ){
  read_lines(file) %>%
    str_subset( "^[0-9A-F]" ) %>%
    tibble( txt = .) %>%
    separate(txt, into = c("code", "type_field", "description"), sep = "[;#]", extra = "drop" )%>%
    mutate_all(str_trim) %>%
    mutate(
      points = map( str_split(code, " "), strtoi, base = 16 ),
      nrunes = map_int(points, length),
      emoji = stri_enc_fromutf32(points)
    ) %>%
    arrange( desc(nrunes), code )
}

emoji_data <- parse_emoji_data()

emoji_sequences <- parse_emoji_sequence("data-raw/unicode-tr51/data/emoji-sequences.txt")

rx_keycap <- emoji_sequences %>%
  filter( type_field == "Emoji_Keycap_Sequence" ) %>%
  pull(points) %>%
  map_int(1) %>%
  stri_enc_fromutf32() %>%
  paste0("[", ., "]\uFE0F\u20E3")

# this can probably be simplified as there are some common runes
rx_subregion_flag <- emoji_sequences %>%
  filter(type_field == "Emoji_Tag_Sequence" ) %>%
  pull(points) %>%
  stri_enc_fromutf32() %>%
  paste0( collapse = "|")

rx_flags <- paste0( "[\U1F1E6-\U1F1FF]{2}|", rx_subregion_flag )

code_point_range <- function(x){
  if( length(x) == 1 ){
    stri_enc_fromutf32(x)
  } else {
    unicode <- map(x, stri_enc_fromutf32)
    paste0( "[", unicode[1], "-", unicode[2], "]" )
  }
}
modifier_base <- emoji_data %>%
  filter( type_field == "Emoji_Modifier_Base" ) %>%
  pull(points) %>%
  map_chr( code_point_range ) %>%
  paste0( collapse = "|" ) %>%
  paste0( "(?:", ., ")")

rx_modifier <- emoji_data %>%
  filter( type_field == "Emoji_Modifier" ) %>%
  pull(points) %>%
  extract2(1L) %>%
  map_chr(stri_enc_fromutf32 ) %>%
  { paste0("[", .[1], "-", .[2], "]") }

rx_modifier_sequence <- paste0( modifier_base, rx_modifier )


rx_sequences <- glue("{rx_modifier_sequence}|{rx_flags}|{rx_keycap}")








# dealing with kiss, family and couple separately
rx_adult <- "[\U1F468\U1F469]"
rx_kid   <- "[\U1F466\U1F467]"
zwj      <- "\u200D"

woman_sign <- "\u2640"
man_sign <- "\u2642"
rx_gender <- glue("[{woman_sign}{man_sign}]\uFE0F")

rx_couple_sequence <- glue("{rx_adult}\U200D\U2764\UFE0F\U200D{rx_adult}")
rx_kiss_sequence   <- glue("{rx_adult}\U200D\U2764\UFE0F\U200D\U1F48B\U200D{rx_adult}" )
rx_family_sequence <- glue("(?:{rx_adult}\u200D){{1,2}}(?:{rx_kid}\u200D?){{1,2}}")

# gendered roles ...
emoji_zwj_sequences <- parse_emoji_sequence("data-raw/unicode-tr51/data/emoji-zwj-sequences.txt")
# %>%
#  filter( !str_detect(description, "^(kiss|couple|family)") )

# sequences that are coded in 3 runes
roles_zwj_3 <- emoji_zwj_sequences %>% filter( nrunes == 3, str_detect(description, "^woman") ) %>% pull( points ) %>% map_int(3L)
rx_zwj_3 <- glue( "{rx_adult}{rx_modifier}?{zwj}[{roles}]",
  roles = stri_enc_fromutf32(roles_zwj_3)
)

# those coded in 4 runes - with the gender first
roles_zwj_4_1 <- emoji_zwj_sequences %>%
  filter( nrunes == 4, str_detect(description,"^woman"), !str_detect(description, ":"), str_detect(code, "^1F469") ) %>%
  pull(points) %>%
  map_int(3)
rx_zwj_4_1 <- glue( "{rx_adult}{rx_modifier}?{zwj}[{roles}]\uFE0F",
  roles = stri_enc_fromutf32(roles_zwj_4_1)
)

# those coded in 4 runes - with in 3rd position
roles_zwj_4_2 <- emoji_zwj_sequences %>%
  filter( nrunes == 4, str_detect(description,"^(blond-haired )?(mer|wo)m[ae]n"), !str_detect(description, ":"), !str_detect(code, "^1F469") ) %>%
  pull(points) %>%
  map_int(1)
rx_zwj_4_2 <- glue( "[{roles}]{rx_modifier}?{zwj}{rx_gender}",
  roles = stri_enc_fromutf32(roles_zwj_4_2)
)

rx_rainbow_flag <- filter( emoji_zwj_sequences, description == "rainbow flag" ) %>%
  pull(points) %>%
  stri_enc_fromutf32()

roles_zwj_5 <- emoji_zwj_sequences %>%
  filter( nrunes == 5, !str_detect(description, ":"), str_detect(description, "woman") ) %>%
  pull(points) %>%
  map_int(1)
rx_zwj_5 <- glue( "[{roles}](?:\uFE0F|{rx_modifier}){zwj}{rx_gender}",
  roles = stri_enc_fromutf32(roles_zwj_5)
)

rx_eye <- filter( emoji_zwj_sequences, description == "eye in speech bubble" ) %>%
  pull(points) %>%
  stri_enc_fromutf32()

rx_zwj_seq <- glue("{rx_couple_sequence}|{rx_kiss_sequence}|{rx_family_sequence}|{rx_zwj_3}|{rx_zwj_4_1}|{rx_zwj_4_2}|{rx_rainbow_flag}|{rx_zwj_5}|{rx_eye}")

