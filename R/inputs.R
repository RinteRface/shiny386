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

  value <- restoreInput(id = inputId, default = value)
  input_tag <- tags$input(
    id = inputId,
    type = "checkbox",
    class = "custom-control-input"
  )

  if (!is.null(value) && value) {
    input_tag <- input_tag %>% tagAppendAttributes(checked = "checked")
  }

  input_wrapper <- tags$div(
    class = "custom-control custom-switch",
    style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    }
  )

  input_wrapper %>% tagAppendChildren(
    input_tag,
    tags$label(class = "custom-control-label", `for` = inputId, label)
  )
}
