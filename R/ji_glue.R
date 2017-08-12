#' @importFrom stringr str_sub<- str_replace_all str_extract_all str_locate_all
#' @importFrom purrr map_chr map2_chr
#' @export
ji_glue_one <- function(txt){
  rx <- ":[^:]*:[*]?"
  pos <- str_locate_all(txt, rx)[[1]]

  chunks <- str_extract_all(txt, rx)[[1]]
  list   <- str_detect( chunks, "[*]$")
  chunks <- str_replace_all( chunks,  ":[*]?", "")

  emojis <- map2_chr(
    chunks, list, ~{
      if(identical(.x, "")){
        "::"
      } else {
        tryCatch({
          if(.y){
            paste( eval( parse( text = paste0( "ji_set(", .x, ")" ) ) ), collapse = " " )
          } else {
            eval( parse( text = paste0( "jitsu(", .x, ")" ) ) )
          }
        }, error = function(e) .x)
      }
    }
  )

  for( i in rev(seq_along(emojis)) ){
    str_sub(txt, pos[i,1], pos[i,2]) <- emojis[i]
  }

  txt
}

#' emoji replacement
#'
#' @param txt character vector in which to replace `:something:` by an
#' emoji for `something`, or `:something:*` by the collapsed
#' list of all emojis relevant to `something`.
#'
#' @details
#' - For the single form `:something:` [jitsu] is used on each pattern wrapped in `:`
#' - For the list form `:something:*` [ji_set] is used
#'
#' @examples
#' \dontrun{
#'   ji_glue( "My :cat,face,love: loves to :sleep,zzz:" )
#'
#'   ji_glue( ":monkey:* loves to :dance:" )
#'
#'   ji_glue( ":technologist:* do :cool: stuff")
#' }
#' @export
ji_glue <- function(txt){
  structure(
    map_chr(txt, ji_glue_one),
    class = "emoji"
  )
}
