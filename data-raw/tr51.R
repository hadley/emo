library(tidyverse)
library(glue)
library(assertthat)
library(stringi)

parse_emoji_data <- function(file = "data-raw/tr51/emoji-data.txt"){
  read_lines( file) %>%
    str_subset( "^[0-9A-F]" ) %>%
    tibble( txt = .) %>%
    separate(txt, into = c("code", "type_field", "description"), sep = "[;#]", extra = "drop" )%>%
    mutate_all(str_trim) %>%
    mutate( points = map( str_split(code, "[.]{2}"), strtoi, base = 16) )
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

code_point_range <- function(x){
  if( length(x) == 1 ){
    stri_enc_fromutf32(x)
  } else {
    unicode <- map(x, stri_enc_fromutf32)
    paste0( "[", unicode[1], "-", unicode[2], "]" )
  }
}


emoji_data <- parse_emoji_data()
data_modifier_base <- emoji_data %>%
  filter( type_field == "Emoji_Modifier_Base" )

# emoji presentation

# emoji presentation not possibly affected by skin tone modifiers
data_presentation <- emoji_data %>%
  filter( type_field == "Emoji_Presentation" ) %>%
  anti_join( select(data_modifier_base, code), by = "code" )

rx_presentation <- data_presentation %>%
  pull(points) %>%
  map_chr( code_point_range ) %>%
  paste0( collapse = "|" )

emoji_data_no_presentation <- anti_join(
  filter( emoji_data, type_field == "Emoji"),
  filter( emoji_data, type_field == "Emoji_Presentation"),
  by = "code"
) %>%
  anti_join( select(data_modifier_base, code), by = "code" )

rx_emoji <- emoji_data_no_presentation %>%
  pull(points) %>%
  map_chr( code_point_range ) %>%
  paste0( collapse = "|" ) %>%
  paste0( "(?:", ., ")\UFE0F" ) %>%
  str_replace("[*]", "[*]")

emoji_data_picto <- anti_join(
  filter( emoji_data, type_field == "Extended_Pictographic"),
  filter( emoji_data, type_field == "Emoji"),
  by = "code"
)
rx_picto <- emoji_data_picto %>%
  pull(points) %>%
  map_chr( code_point_range ) %>%
  paste0( collapse = "|" ) %>%
  paste0( "(?:", ., ")\UFE0F" )


#### emoji sequences
emoji_sequences <- parse_emoji_sequence("data-raw/tr51/emoji-sequences.txt")

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

modifier_base <- data_modifier_base %>%
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

rx_modifier_sequence <- glue( "{modifier_base}\UFE0F?{rx_modifier}?" )

rx_sequences <- glue("(?#\n\n--------simple sequences with skin tone modifiers--------){rx_modifier_sequence}|(?#\n\n--------country flags--------){rx_flags}|(?#\n\n--------keycap sequences--------){rx_keycap}")

assert_that( nrow(filter( emoji_sequences, !str_detect(emoji, rx_sequences))) == 0 )




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
emoji_zwj_sequences <- parse_emoji_sequence("data-raw/tr51/emoji-zwj-sequences.txt")

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

rx_pirate_flag <- filter( emoji_zwj_sequences, description == "pirate flag" ) %>%
  pull(points) %>%
  stri_enc_fromutf32()

rx_zwj_seq <- glue("(?#\n\n--------couple with heart--------\n){rx_couple_sequence}|(?#\n\n--------kiss--------\n){rx_kiss_sequence}|(?#\n\n--------family--------\n){rx_family_sequence}|(?#\n\n--------gendered roles--------\n){rx_zwj_3}|{rx_zwj_4_1}|(?#\n\n--------gendered activities--------\n){rx_zwj_4_2}|{rx_zwj_5}|(?#\n\n--------other zwj sequences--------){rx_rainbow_flag}|{rx_eye}|{rx_pirate_flag}")
assert_that( nrow(filter( emoji_zwj_sequences, !str_detect(emoji, rx_zwj_seq))) == 0 )
####### final rx
ji_rx <- unclass(glue("{rx_zwj_seq}|{rx_sequences}|(?#\n\n--------Emoji_Presentation--------){rx_presentation}|(?#\n\n--------Emoji--------){rx_emoji}|(?#\n\n--------Extended_Pictographic--------){rx_picto}"))
cat( ji_rx )

devtools::use_data( ji_rx, overwrite = TRUE )

