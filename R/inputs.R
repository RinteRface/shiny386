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
