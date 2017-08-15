
ji_glue_one <- function(txt){
  rx <- ":([a-zA-Z0-9_]*):"
  pos <- str_locate_all(txt, rx)[[1]]

  chunks <- str_extract_all(txt, rx)[[1]]

  emojis <- map_chr(
    chunks, ~{
      name <- str_replace(., rx, "\\1")
      if( name %in% names(emo::ji_alias) ) emo::ji_alias[name]
    }
  )

  for( i in rev(seq_along(emojis)) ){
    str_sub(txt, pos[i,1], pos[i,2]) <- emojis[i]
  }

  txt
}


#' emoji glue
#'
#' @param txt character vector in which to replace `:something:` by an
#' emoji for `something`. see [ji()] for what `something` can be.
#'
#' This is insipred from [glue::glue()] but uses `:` instead of braces
#' because that's typically how emojis are
#' [expressed](https://www.webpagefx.com/tools/emoji-cheat-sheet/)
#'
#' @examples
#' \dontrun{
#'   ji_glue( ":cat:s love to :zzz:" )
#' }
#' @export
ji_glue <- function(txt){
  structure(
    map_chr(txt, ji_glue_one),
    class = "emoji"
  )
}


