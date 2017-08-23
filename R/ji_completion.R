#' emoji completion
#'
#' @param token start of an emoji alias
#'
#' @examples
#' ji_completion( "key" )
#'
#' @importFrom stringr str_detect
#' @export
ji_completion <- function( token ){
  matches <- emo::ji_name[ str_detect(names(emo::ji_name), token ) ]
  matches <- matches[ !duplicated(matches) ]
  structure(
    matches ,
    class = c("emoji_completion")
  )
}

#' @importFrom purrr walk2
#' @export
print.emoji_completion <- function(x, ...){
  writeLines( paste(x, " : ", names(x)) )
  invisible(x)
}

emoji_completer <- function(env) {
  if (completeme::inside_quotes(env) && grepl(":[^[:space:]]*:?$", env$token)) {
    # The IDE does not tokenize the words like the console
    token <- sub("[^:]*:", ":", env$token)

    if (grepl("^:[^[:space:]]+:$", token)) {
      emoji <- emo::ji(gsub(":", "", token))
    } else {
      res <- ji_completion(sub(":", "", token))

      if (length(res) == 0) {
        return(FALSE)
      }

      if (length(res) == 1) {
        emoji <- emo::ji(names(res))
      } else {
        emoji <- paste0(res, " : ", names(res))
      }
    }
    env$fileName <- emoji
    env$comps <- emoji

    return(TRUE)
  }
  return(FALSE)
}

.onLoad <- function(pkg, lib) {
  completeme::register_completion(emo = emoji_completer)
}
