#' Create a Bootstrap 386 Jumbotron
#'
#' @param ... Any element.
#' @param title Jumbotron title.
#'
#' @return A shiny tag
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   jumbotron_386(
#'    title = "Jumbotron 386",
#'    p("Hello World"),
#'    button_386("btn", "More", class = "btn-lg btn-block")
#'   )
#'  )
#'
#'  server <- function(input, output, session) {}
#'
#'  shinyApp(ui, server)
#' }
jumbotron_386 <- function(..., title = NULL) {
  tags$div(
    class = "jumbotron",
    tags$h1(class = "display-3", title),
    ...
  )
}


