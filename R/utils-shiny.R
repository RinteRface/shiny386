# dropNulls
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


#' Create checkbox/switch input based on the selected type
#'
#' Used internally by \link{toggle_input_386} and \link{checkbox_input_386}
#'
#' @inheritParams shiny::checkboxInput
#' @param type Input type. This is to be able to distinguish between switch and checkbox,
#' which have slightly different design.
#'
#' @return An input tag.
#' @export
create_checkbox_tag <- function(inputId, label, value = FALSE, width = NULL,
                                type = c("switch", "checkbox")) {

  type <- match.arg(type)

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
    class = sprintf("custom-control custom-%s", type),
    style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    }
  )

  input_wrapper %>% tagAppendChildren(
    input_tag,
    tags$label(class = "custom-control-label", `for` = inputId, label)
  )
}
