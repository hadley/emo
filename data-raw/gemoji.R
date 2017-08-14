library(tidyverse)
library(jsonlite)

data <- read_json("data-raw/gemoji/db/emoji.json")
keep <- data %>% map( "emoji" ) %>% map_lgl(negate(is.null))
data <- data[keep]

gemoji <- tibble(
  emoji = map_chr(data, "emoji"),
  description = map_chr(data, "description"),
  category = map_chr(data, "category"),
  aliases = map(data, "aliases") %>% map( . %>% flatten_chr()),
  tags = map(data, "tags") %>% map( . %>% flatten_chr()),
  unicode_version = map_chr(data, "unicode_version"),
  ios_version = map_chr(data, "ios_version")
) %>%
  mutate( unicode_version = ifelse(unicode_version=="", NA, unicode_version))

