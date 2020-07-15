#' Create Bootstrap 386 page skeleton
#'
#' @param ... Slot for shiny386 layout elements.
#' @param title The browser window title (defaults to the host URL of the page).
#' Can also be set as a side effect of the titlePanel() function.
#'
#' @return A list of tags
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#' }
page_386 <- function(..., title = NULL) {
  shiny::tagList(
    # Head
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$title(title),
      shiny::tags$meta(
        name = "viewport",
        content = "
          width=device-width,
          initial-scale=1"
      ),
      shiny::tags$meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge")
    ),
    # body
    shiny::tags$body(
      class = "bootstra-enable-cursor",
      shiny::div(
        class = "container",
        style = "background:url('dist/fonts/grid.svg')",
        ...
      ),
      shiny::div(class = "bootstra-cursor", style = "left: 424px; top: 504px;")
    ) %>% use_bs4_deps()
  )
}
