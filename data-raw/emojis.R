library(tidyverse)
library(jsonlite)

# (keeping this for reference, but replaced by the code in jis.R)

# Download & import -------------------------------------------------------

emoji <- read_json("data-raw/emojilib/emojis.json")
View(emoji)

# Extract names and keywords ----------------------------------------------

emoji <- emoji[ map_lgl(emoji, ~!is.null(.$char)) ]
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

# devtools::use_data(
#   emoji_name,
#   emoji_keyword,
#   internal = TRUE,
#   overwrite = TRUE
# )
