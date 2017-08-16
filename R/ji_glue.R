
#' @importFrom stringr str_locate_all str_extract_all str_replace str_sub<-
#' @importFrom purrr map_chr
ji_glue_one <- function(txt){
  rx <- ":([~a-zA-Z0-9_,]*):([*]?)"
  pos <- str_locate_all(txt, rx)[[1]]

  chunks <- str_extract_all(txt, rx)[[1]]
  emojis <- map_chr( chunks, ~{
    set   <- str_detect( str_replace( ., rx, "\\2" ), "[*]" )
    proxy <- str_replace(., rx, "\\1")

    rx    <- str_detect(proxy, "[~,]")

    tryCatch({
      if( set ){
        paste( eval( parse( text = paste( "ji_set(", proxy, ")" ) )), collapse = "" )
      } else if( rx ){
        eval( parse( text = paste( "jitsu(", proxy, ")" ) ))
      } else {
        find_emoji( proxy )
      }
    }, error = function(e){
      .
    })

  })

  for( i in rev(seq_along(emojis)) ){
    str_sub(txt, pos[i,1], pos[i,2]) <- emojis[i]
  }

  txt
}


#' emoji glue
#'
#' @param txt character vector in which to replace `:something:` by an
#' emoji for `something`.
#'
#' This is insipred from [glue::glue()] but uses `:` instead of braces
#' because that's typically how emojis are
#' [expressed](https://www.webpagefx.com/tools/emoji-cheat-sheet/)
#'
#' @examples
#' \dontrun{
#'   # simple syntax, uses `ji`
#'   ji_glue( ":cat:s love to :zzz:" )
#'
#'   # we can also use the `jitsu` syntax (with `~` and `,`)
#'   ji_glue( ":~cat,face: love to :~sleep:" )
#'
#'   # and get sets of emojis instead of just one by
#'   # using the wildcard suffix
#'   ji_glue( ":monkey,face:* love to :celebrate:" )
#'
#' }
#' @export
ji_glue <- function(txt){
  structure(
    map_chr(txt, ji_glue_one),
    class = "emoji"
  )
}


