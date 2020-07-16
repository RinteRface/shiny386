#' Create Bootstrap 386 page skeleton
#'
#' @param ... Slot for shiny386 layout elements.
#' @param title The browser window title (defaults to the host URL of the page).
#' Can also be set as a side effect of the titlePanel() function.
#'
#' @return A list of tags
#' @export
page_386 <- function(..., title = NULL) {
  tagList(
    # Head
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title(title),
      tags$meta(
        name = "viewport",
        content = "
          width=device-width,
          initial-scale=1"
      ),
      tags$meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge")
    ),
    # body
    tags$body(
      div(
        class = "container",
        ...
      )
    ) %>% use_bs4_deps()
  )
}
