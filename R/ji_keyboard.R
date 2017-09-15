#' emoji keyboard add in
#'
#' @import miniUI
#' @import shiny
#' @importFrom purrr pmap map_int map2
#' @importFrom stringr str_replace_all str_to_lower
#' @export
ji_keyboard <- function() {

  emojiLink <- function(emoji, aliases){
    link <- actionLink(emoji, label = emoji, `data-emoji` = aliases[1] )
    link$attribs$class <- paste( link$attribs$class, "emoji-link")
    link
  }

  ui <- miniPage(
    includeScript(system.file( "resources", "ji_keyboard.js", package = "emo")),
    includeCSS(system.file( "resources", "ji_keyboard.css", package = "emo")),

    gadgetTitleBar("emojis"),
    miniContentPanel(
      textInput("emoji_query", "query", value = "cat", width = "100%" ),
      uiOutput("emoji_alias"),
      hr(),
      div( id = "emojis")
    )
  )

  server <- function(input, output, session) {

    completions <- reactive({
      ji_completion(input$emoji_query)
    })

    observe({
      data <- completions()
      session$sendCustomMessage( "emoji_refresh",
        list( emojis = unname(data), aliases = names(data) )
      )
    })

    output$emoji_alias <- renderUI({
      div( map( input$selected_aliases , div, style = "font-family: monospace" ) )
    })

  }

  viewer <- dialogViewer("Emoji Keyboard", width = 500, height = 500)
  runGadget(ui, server, viewer = viewer)
}
