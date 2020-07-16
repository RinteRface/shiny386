#' Create a Bootstrap 386 toast
#'
#' Display user feedback
#'
#' @param id Unique toast id.
#' @param title Toast title.
#' @param subtitle Toast subtitle.
#' @param ... Toast content.
#' @param img Toast image.
#'
#' @return A toast
#' @export
#' @seealso \link{show_toast_386}
toast_386 <- function(id, title = NULL, subtitle = NULL, ..., img = NULL) {

  toast_header <- div(
    class = "toast-header",
    if (!is.null(img)) {
      span(
        class = "avatar mr-2",
        style = sprintf("background-image: url(%s)", img)
      )
    },
    if (!is.null(title)) strong(class = "mr-2", title),
    if (!is.null(subtitle)) tags$small(subtitle)
  )

  toast_body <- div(class = "toast-body", ...)

  toast_wrapper <- div(
    id = id,
    class = "toast",
    role = "alert",
    style = "position: absolute; top: 0; right: 0;",
    `aria-live` = "assertive",
    `aria-atomic` = "true",
    `data-toggle` = "toast"
  )

  toast_wrapper %>% tagAppendChildren(toast_header, toast_body)
}




#' Show a Bootstrap 386 toast on the client
#'
#' @param id Toast id.
#' @param options Toast options: see \url{https://getbootstrap.com/docs/4.3/components/toasts/}.
#' @param session Shiny session
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   toast_386(
#'     id = "toast",
#'     title = "Hello",
#'     subtitle = "now",
#'     "Toast body",
#'     img = "https://preview-dev.tabler.io/static/logo.svg"
#'   ),
#'   button_386("launch", "Go!", class = "btn-lg")
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$toast))
#'    observeEvent(input$launch, {
#'      removeNotification("notif")
#'      show_toast_386(
#'        "toast",
#'        options = list(
#'          animation = FALSE,
#'          delay = 3000
#'        )
#'      )
#'    })
#'
#'    observeEvent(input$toast, {
#'      showNotification(
#'        id = "notif",
#'        "Toast was closed",
#'        type = "warning",
#'        duration = 1,
#'
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
show_toast_386 <- function(id, options = NULL, session = getDefaultReactiveDomain()) {
  message <- dropNulls(
    list(
      id = id,
      options = options
    )
  )
  session$sendCustomMessage(type = "tabler-toast", message)
}



#' Create a Bootstrap 386 modal
#' @inheritParams shiny::modalDialog
#' @export
#' @rdname modal
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  shinyApp(
#'   ui = page_386(
#'     button_386("show", "Show modal dialog"),
#'     verbatimTextOutput("dataInfo")
#'   ),
#'
#'   server = function(input, output) {
#'     # reactiveValues object for storing current data set.
#'     vals <- reactiveValues(data = NULL)
#'
#'    # Return the UI for a modal dialog with data selection input. If 'failed' is
#'    # TRUE, then display a message that the previous value was invalid.
#'    dataModal <- function(failed = FALSE) {
#'      modal_386(
#'        textInput("dataset", "Choose data set",
#'                  placeholder = 'Try "mtcars" or "abc"'
#'        ),
#'        span('(Try the name of a valid data object like "mtcars", ',
#'             'then a name of a non-existent object like "abc")'),
#'        if (failed)
#'          div(tags$b("Invalid name of data object", style = "color: red;")),
#'
#'        footer = tagList(
#'          modalButton("Cancel"),
#'          button_386("ok", "OK")
#'        )
#'      )
#'    }
#'
#'    # Show modal when button is clicked.
#'    observeEvent(input$show, {
#'      show_modal_386(dataModal())
#'    })
#'
#'    # When OK button is pressed, attempt to load the data set. If successful,
#'    # remove the modal. If not show another modal, but this time with a failure
#'    # message.
#'    observeEvent(input$ok, {
#'      # Check that data object exists and is data frame.
#'      if (!is.null(input$dataset) && nzchar(input$dataset) &&
#'          exists(input$dataset) && is.data.frame(get(input$dataset))) {
#'        vals$data <- get(input$dataset)
#'        remove_modal_386()
#'      } else {
#'        show_modal_386(dataModal(failed = TRUE))
#'      }
#'    })
#'
#'    # Display information about selected data
#'    output$dataInfo <- renderPrint({
#'      if (is.null(vals$data))
#'        "No data selected"
#'      else
#'        summary(vals$data)
#'    })
#'   }
#'  )
#' }
modal_386 <- shiny::modalDialog

#' Show a Bootstrap 386 modal
#' @inheritParams shiny::showModal
#' @export
#' @rdname modal
show_modal_386 <- shiny::showModal



#' Hide a Bootstrap 386 modal
#' @inheritParams shiny::removeModal
#' @export
#' @rdname modal
remove_modal_386 <- shiny::removeModal

