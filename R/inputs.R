#' Bootstrap 386 action button
#'
#' @inheritParams shiny::actionButton
#' @param status Button color.
#' @return A shiny tag.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386(
#'    "btn",
#'    HTML(paste("Value", textOutput("val"), sep = ":")),
#'    icon = icon("thumbs-up"),
#'    class = "btn-lg"
#'   )
#'  )
#'
#'  server <- function(input, output) {
#'    output$val <- renderText(input$btn)
#'  }
#'
#'  shinyApp(ui, server)
#' }
button_386 <- function(inputId, label, status = NULL, icon = NULL, width = NULL, ...) {

  btn_cl <- paste0(
    "btn",
    if (is.null(status)) {
      " btn-primary"
    } else {
      paste0(" btn-", status)
    },
    " action-button"
  )

  value <- restoreInput(id = inputId, default = NULL)

  # custom right margin
  if (!is.null(icon)) icon$attribs$class <- paste0(
    icon$attribs$class, " mr-1"
  )

  tags$button(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = btn_cl,
    `data-val` = value,
    list(icon, label), ...
  )
}


#' Create a Bootstrap 386 text input
#' @inheritParams shiny::textInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   textInput("caption", "Caption", "Data Summary"),
#'   verbatimTextOutput("value")
#'  )
#'
#'  server <- function(input, output, session) {
#'   output$value <- renderText({ input$caption })
#'  }
#'  shinyApp(ui, server)
#'
#' }
text_input_386 <- shiny::textInput



#' Create a Bootstrap 386 text area input
#' @inheritParams shiny::textAreaInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   textAreaInput("caption", "Caption", "Data Summary"),
#'   verbatimTextOutput("value")
#'  )
#'
#'  server <- function(input, output, session) {
#'   output$value <- renderText({ input$caption })
#'  }
#'  shinyApp(ui, server)
#'
#' }
text_area_input_386 <- shiny::textAreaInput




#' Custom Bootstrap 386 switch input
#'
#' Similar to the shiny checkbox
#'
#' @inheritParams shiny::checkboxInput
#'
#' @return A toggle input tag.
#' @export
#' @seealso \link{update_toggle_input_386}.
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   toggle_input_386("toggle", "Toggle me", TRUE),
#'   verbatimTextOutput("val")
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$val <- renderPrint(input$toggle)
#'  }
#'  shinyApp(ui, server)
#' }
toggle_input_386 <- function(inputId, label, value = FALSE, width = NULL) {
  create_checkbox_tag(inputId, label, value = FALSE, width = NULL, type = "switch")
}



#' Update \link{toggle_input_386} on the client
#'
#' @inheritParams shiny::updateCheckboxInput
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386("update", "Go!", class = "btn-lg"),
#'   toggle_input_386("toggle", "Switch", value = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$toggle))
#'    observeEvent(input$update, {
#'      update_toggle_input_386(
#'        session,
#'        "toggle",
#'        value = !input$toggle
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_toggle_input_386 <- function (session, inputId, label = NULL, value = NULL) {
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}


#' Create a Bootstrap 386 checkbox
#'
#' @inheritParams shiny::checkboxInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   checkbox_input_386("check", "Check me", TRUE),
#'   verbatimTextOutput("val")
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$val <- renderPrint(input$check)
#'  }
#'  shinyApp(ui, server)
#' }
checkbox_input_386 <- function(inputId, label, value = FALSE, width = NULL) {
  create_checkbox_tag(inputId, label, value = FALSE, width = NULL, type = "checkbox")
}


#' Update \link{checkbox_input_386} on the client
#'
#' @inheritParams shiny::updateCheckboxInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386("update", "Go!", class = "btn-lg"),
#'   checkbox_input_386("check", "Checked", value = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$check))
#'    observeEvent(input$update, {
#'      update_checkbox_input_386(
#'        session,
#'        "toggle",
#'        value = !input$check
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_checkbox_input_386 <- update_toggle_input_386
