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




#' Create a Bootstrap 386 progress bar
#'
#' The progress bar may be updated server side. See \link{update_progress_386}.
#'
#' @param id Progress unique id.
#' @param value Progress value. Numeric between 0 and 100.
#' @param status Progress status.
#'
#' @return A progress bar tag.
#' @export
#' @seealso \link{update_progress_386}
progress_386 <- function(id = NULL, value, status = NULL) {

  validate_progress_value(value)
  if (!is.null(status)) validate_status(status)
  bar_cl <- paste0("progress-bar", if(!is.null(status)) paste0(" bg-", status))

  div(
    class = "progress",
    div(
      id = id,
      class = bar_cl,
      style = paste0("width: ", value, "%"),
      role = "progressbar",
      `aria-valuenow` = as.character(value),
      `aria-valuemin` = "0",
      `aria-valuemax` = "100",
      span(class = "sr-only", paste0(value,"% complete"))
    )
  )
}



#' Update a \link{progress_386} on the client
#'
#' @param id Progress unique id.
#' @param value New value.
#' @param session Shiny session object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'  ui <- page_386(
#'   button_386("update", "Update"),
#'   br(),
#'   progress_386(id = "progress1", 12)
#'  )
#'
#'  server <- function(input, output, session) {
#'    observeEvent(input$update, {
#'      update_progress_386(
#'        id = "progress1",
#'        sample(1:100, 1)
#'      )
#'    })
#'  }
#'  shinyApp(ui, server)
#' }
update_progress_386 <- function(id, value, session = shiny::getDefaultReactiveDomain()) {
  message <- list(id = session$ns(id), value = value)
  session$sendCustomMessage(type = "update-progress", message)
}




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



#' Create a Bootstrap 386 badge
#'
#' @param ... Text.
#' @param status Badge status.
#' @param rounded Rounded style. Default to FALSE.
#'
#' @return A shiny tags
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'  ui <- page_386(
#'   badge_386(status = "danger", "1"),
#'   badge_386(status = "info", "2"),
#'   badge_386(status = "success", "3", rounded = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {}
#'  shinyApp(ui, server)
#' }
badge_386 <- function (..., status, rounded = FALSE) {

  validate_status(status)

  shiny::tags$span(
    class = paste0(
      "badge",
      if (rounded) " badge-pill",
      " badge-", status
    ),
    ...
  )
}



#' Create a Bootstrap 386 card
#'
#' @param ... Card content.
#' @param title Card title.
#' @param status Card background status.
#' @param footer Card footer.
#'
#' @return A shiny tag
#' @export
#' @rdname card
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'  ui <- page_386(
#'   card_386(
#'    title = "My card",
#'    "This is my card",
#'    br(),
#'    card_link_386(href = "https://www.google.com", "More"),
#'    footer = "Card footer"
#'   )
#'  )
#'
#'  server <- function(input, output, session) {}
#'  shinyApp(ui, server)
#' }
card_386 <- function(..., title = NULL, status = NULL, footer = NULL) {

  if (!is.null(status)) validate_status(status)

  card_cl <- paste0(
    "card mb-3",
    if (!is.null(status)) paste0(" bg-", status)
  )


  shiny::tags$div(
    class = card_cl,
    if (!is.null(title)) shiny::tags$div(class = "card-header", title),
    shiny::tags$div(class = "card-body", ...),
    if (!is.null(footer)) shiny::tags$div(class = "card-footer text-muted", footer)
  )
}



#' Create a Bootstrap 4 card title element
#'
#' @param title Title text.
#'
#' @return A shiny tag.
#' @export
#'
#' @rdname card
card_title_386 <- function(title) {
  shiny::h4(class = "card-title", title)
}


#' Create a Bootstrap 386 card subtitle element
#'
#' @param subtitle Card subtitle.
#'
#' @return A shiny tag.
#' @export
#' @rdname card
card_subtitle_386 <- function(subtitle) {
  shiny::h6(class = "card-subtitle text-muted", subtitle)
}


#' Create a Bootstrap 386 card link element
#'
#' @param href Target url.
#' @param label Link text.
#'
#' @return A shiny tag.
#' @export
#' @rdname card
card_link_386 <- function(href, label) {
  shiny::a(href = href, class = "card-link", label)
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
