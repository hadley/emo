library(tidyverse)
library(jsonlite)

# Download & import -------------------------------------------------------

url <- "https://github.com/muan/emojilib/raw/master/emojis.json"
download.file(url, "data-raw/emojis.json", quiet = TRUE)

emoji <- read_json("data-raw/emojis.json")
View(emoji)

# Extract names and keywords ----------------------------------------------

emoji_name <- emoji %>% map_chr("char", .default = NA)

keywords <- emoji %>%
  map("keywords") %>%
  map(. %>% flatten_chr())

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

emoji_keyword <- invert(keywords)

devtools::use_data(
  emoji_name,
  emoji_keyword,
  internal = TRUE,
  overwrite = TRUE
)
