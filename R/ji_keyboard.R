#' emoji keyboard add in
#'
#' @import miniUI
#' @import shiny
#' @importFrom purrr pmap map_int map2
#' @importFrom stringr str_replace_all str_to_lower
#' @export
ji_keyboard <- function() {

  ui <- miniPage(

    gadgetTitleBar("emojis"),
    miniContentPanel(
      textInput("emoji_query", "query", value = "cat", width = "100%" ),
      uiOutput("emoji_alias"),
      hr(),
      uiOutput("emojis")
    )
  )

  server <- function(input, output, session) {

    emojis_tbl <- reactive({
      query <- str_replace_all( str_to_lower( input$emoji_query ), " ", ", " )
      data  <- eval( parse( text = paste0( "emo::ji_filter( ", query , " )") ) )

      filter( data, map_int(aliases, length) > 0 )
    })

    aliases <- reactiveValues( data = NULL )

    output$emojis <- renderUI({
      data <- emojis_tbl()

      rows <- map2( data$emoji, data$aliases,  ~ {

        observeEvent( input[[.x]], {
          aliases$data <- paste( ":", .y, ":", sep = "" )
        })

        actionLink( .x, label = .x, )
      })

      div(rows)

    })

    output$emoji_alias <- renderUI({
      div( map( aliases$data, div, style = "font-family: monospace" ) )
    })

    observeEvent(input$done, {
      invisible(stopApp())
    })

    observeEvent(input$cancel, {
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer("Emoji Keyboard", width = 500, height = 500)
  runGadget(ui, server, viewer = viewer)
}
