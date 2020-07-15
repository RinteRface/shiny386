bs4_deps <- htmltools::htmlDependency(
  name = "Bootstrap",
  version = "4.4.1",
  src = c(file = "bootstrap-4.4.1"),
  package = "shiny386",
  script = c(
    "js/bootstrap.bundle.js",
    "js/custom.js",
    "js/progress_handler.js"
  ),
  stylesheet = "css/bootstrap.min.css"
)


use_bs4_deps <- function(tag) {
  htmltools::tagList(tag, bs4_deps)
}
