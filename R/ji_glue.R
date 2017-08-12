#' @importFrom stringr str_sub<- str_replace_all str_extract_all str_locate_all
#' @importFrom purrr map_chr
#' @export
ji_glue_one <- function(txt){
  rx <- ":[^:]+:"
  pos <- str_locate_all(txt, rx)[[1]]

  emojis <- map_chr(
    paste0( "jitsu(", str_replace_all( str_extract_all(txt, rx)[[1]], ":", "" ), ")" ),
    ~ eval( parse(text=.) )
  )

  for( i in rev(seq_along(emojis)) ){
    str_sub(txt, pos[i,1], pos[i,2]) <- emojis[i]
  }

  txt
}

#' emoji replacement
#'
#' @param txt character vector in which to replace `:something:` by an
#' emoji for `something`
#'
#' @details [jitsu] is used on each pattern wrapped in `:`
#'
#' @examples
#' \dontrun{
#'   ji_glue( "My :cat,face,love: loves to :sleep,zzz:" )
#' }
#' @export
ji_glue <- function(txt){
  structure(
    map_chr(txt, ji_glue_one),
    class = "emoji"
  )
}
