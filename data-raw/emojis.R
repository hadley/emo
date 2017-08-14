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

emoji_keyword[names(emoji_name)] <- map2(emoji_keyword[names(emoji_name)], names(emoji_name), union)

# Load in other internal data ----------------------------------------------
load("data/fisher_lst.rda")

devtools::use_data(
  emoji_name,
  emoji_keyword,
  fisher_lst,
  internal = TRUE,
  overwrite = TRUE
)
