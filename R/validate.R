valid_statuses <- c(
  "primary",
  "secondary",
  "success",
  "info",
  "warning",
  "danger",
  "light",
  "dark"
)

#' Validation function
#'
#' Validate the status of a Bootstrap 386 element.
#'
#' @param status Color to validate.
#'
#' @return TRUE if the test pass.
#' @export
#'
#' @examples
#' \dontrun{
#'  validate_status("danger")
#'  validate_status("maroon")
#' }
validate_status <- function(status) {

  if (is.null(status)) {
    return(TRUE)
  } else {
    if (status %in% valid_statuses) {
      return(TRUE)
    }
  }

  stop("Invalid status: ", status, ". Valid statuses are: ",
       paste(valid_statuses, collapse = ", "), ".")
}



#' Validate a \link{progress_386} value
#'
#' @param value Value to validate.
#'
#' @return An error is raised if the value does not met expectations.
#' @export
validate_progress_value <- function(value) {
  if (!is.numeric(value)) stop("Progress value must be numeric!")
  range <- (value >= 0 && value <= 100)
  if (!(range)) stop("Progress value must be between 0 and 100.")
}
