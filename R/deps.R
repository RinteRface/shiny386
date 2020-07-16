bs4_deps <- htmltools::htmlDependency(
  name = "Bootstrap",
  version = "4.4.1",
  src = c(file = "bootstrap-4.4.1"),
  package = "shiny386",
  script = c(
    "js/bootstrap.bundle.js",
    "js/custom.js",
    "js/progress_handler.js",
    "js/toast_handler.js",
    "js/radioButtonsBinding.js",
    "js/tabsetPanelBinding.js"
  ),
  stylesheet = c(
    "css/bootstrap.min.css",
    "css/custom.css"
  )
)


#' Create shiny386 dependencies
#'
#' Add all necessary dependencies so that shiny386 renders well
#'
#' @param tag Tag on which to add dependencies. We usually target the body.
#' @export
#' @seealso \link{page_386}.
use_bs4_deps <- function(tag) {
  tagList(tag, bs4_deps)
}
