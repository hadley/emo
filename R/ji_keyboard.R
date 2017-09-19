
#' emoji keyboard add in
#'
#' @import miniUI
#' @import shiny
#' @importFrom purrr pmap map_int map2 flatten_chr pluck
#' @importFrom stringr str_replace_all str_to_lower
#' @importFrom dplyr pull
#' @export
ji_keyboard <- function() {

  context <- rstudioapi::getActiveDocumentContext()
  selection <- pluck( context, "selection", 1, "text" ) %>%
    str_replace_all(":", "")

  ui <- miniPage(
    includeScript(system.file( "resources", "ji_keyboard.js", package = "emo")),
    includeCSS(system.file( "resources", "ji_keyboard.css", package = "emo")),

    gadgetTitleBar("emojis"),
    miniContentPanel(
      textInput("emoji_query", "query", value = selection, width = "100%" ),
      div( id = "emojis"),
      uiOutput("emoji_alias")
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
      req(input$selected_emoji)
      emo::jis %>%
        filter( emoji == str_to_lower(input$selected_emoji) ) %>%
        pull(aliases) %>%
        flatten_chr() %>%
        paste0( ":", ., ":") %>%
        map( div, class = "alias" ) %>%
        tagList()
    })

    observeEvent(input$emoji_dblclicked, {
      rstudioapi::insertText(input$emoji_dblclicked)
      invisible(stopApp())
    })

    observeEvent(input$done, {
      rstudioapi::insertText(input$selected_emoji)
      invisible(stopApp())
    })

    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  }

  viewer <- dialogViewer("Emoji Keyboard", width = 500, height = 500)
  runGadget(ui, server, viewer = viewer)
}
