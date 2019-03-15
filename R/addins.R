#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom rstudioapi insertText
#' @importFrom shiny actionLink observeEvent stopApp runGadget selectInput
#'
ji_selector_env <- new.env();

ji_selector <- function() {
  # if ji_selector environment was not set, then configure it
  if (length(ji_selector_env) == 0) {
    used_jis <- emo::ji_name; # emojis the selector will use
    ui <- miniPage(
      gadgetTitleBar("Emoji selector"),
      miniContentPanel(
        selectInput("in_code", label = "",
                    choices = c("Emoji", "`r emo::ji(...)`")),
        # for each emoji to select, create its actionLink
        lapply(names(used_jis), function(act_ji) {
          actionLink(act_ji, label = used_jis[[act_ji]]);
        })
      )
    )
    server <- function(input, output, session) {
      lapply(names(used_jis), function(act_ji) {
        # for each emoji to select, create its observeEvent
        observeEvent(input[[act_ji]], {
          if (input$in_code == "Emoji") {
            insertText(used_jis[[act_ji]]);
          } else {
            insertText(paste0("`r emo::ji(\"", act_ji, "\")`"));
          }
        })
      })
      # stop the app when Done button clicked
      observeEvent(input$done, {
        stopApp()
      })
    }
    ji_selector_env$ui <- ui;
    ji_selector_env$server <- server;
  }

  runGadget(ji_selector_env$ui, ji_selector_env$server)
}
