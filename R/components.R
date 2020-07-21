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
    if(!is.null(title)) tags$h1(class = "display-3", title),
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



#' Create a Bootstrap 386 list group container
#'
#' @param ... Slot for \link{list_group_item_386}.
#' @param width List group width. 4 by default. Between 1 and 12.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(shiny386)
#'
#'  shinyApp(
#'    ui = page_386(
#'       fluidRow(
#'        list_group_386(
#'        list_group_item_386(
#'         type = "basic",
#'         "Cras justo odio"
#'        ),
#'        list_group_item_386(
#'         type = "basic",
#'         "Dapibus ac facilisis in"
#'        ),
#'        list_group_item_386(
#'         type = "basic",
#'         "Morbi leo risus"
#'        )
#'       ),
#'       list_group_386(
#'        list_group_item_386(
#'         "Cras justo odio",
#'         active = TRUE,
#'         disabled = FALSE,
#'         type = "action",
#'         src = "http://www.google.fr"
#'        ),
#'        list_group_item_386(
#'         active = FALSE,
#'         disabled = FALSE,
#'         type = "action",
#'         "Dapibus ac facilisis in",
#'         src = "http://www.google.fr"
#'        ),
#'        list_group_item_386(
#'         "Morbi leo risus",
#'         active = FALSE,
#'         disabled = TRUE,
#'         type = "action",
#'         src = "http://www.google.fr"
#'        )
#'       ),
#'       list_group_386(
#'        list_group_item_386(
#'         "Donec id elit non mi porta gravida at eget metus.
#'         Maecenas sed diam eget risus varius blandit.",
#'         active = TRUE,
#'         disabled = FALSE,
#'         type = "heading",
#'         title = "List group item heading",
#'         subtitle = "3 days ago",
#'         footer = "Donec id elit non mi porta."
#'        ),
#'        list_group_item_386(
#'         "Donec id elit non mi porta gravida at eget metus.
#'         Maecenas sed diam eget risus varius blandit.",
#'         active = FALSE,
#'         disabled = FALSE,
#'         type = "heading",
#'         title = "List group item heading",
#'         subtitle = "3 days ago",
#'         footer = "Donec id elit non mi porta."
#'        )
#'       )
#'      )
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @export
list_group_386 <- function (..., width = 4) {
  listGroupTag <- tags$ul(
    class = "list-group",
    ...
  )

  tags$div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    listGroupTag
  )
}



#' Create a Bootstrap 386 list group item
#'
#' @param ... Item content.
#' @param active Whether the item is active or not. FALSE by default.
#' Only if type is "action" or "heading".
#' @param disabled Whether the item is disabled or not. FALSE by default.
#' Only if type is "action" or "heading".
#' @param type Item type. Choose between "basic", "action" and "heading".
#' @param src Item external link.
#' @param title Item title (only if type is "heading").
#' @param subtitle Item subtitle (only if type is "heading").
#' @param footer Item footer content (only if type is "heading").
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
list_group_item_386 <- function(..., active = FALSE, disabled = FALSE,
                             type = c("basic", "action", "heading"),
                             src = "#", title = NULL, subtitle = NULL,
                             footer = NULL) {

  if (isTRUE(active) && isTRUE(disabled)) {
    stop("active and disabled cannot be TRUE at the same time!")
  }
  type <- match.arg(type)

  itemCl <- switch(
    type,
    "basic" = "list-group-item d-flex justify-content-between align-items-center",
    "action" = "list-group-item list-group-item-action",
    "heading" = "list-group-item list-group-item-action flex-column align-items-start"
  )
  if (isTRUE(active)) itemCl <- paste0(itemCl, " active")
  if (isTRUE(disabled)) itemCl <- paste0(itemCl, " disabled")


  # item tag
  if (type == "basic") {
    tags$li(
      class = itemCl,
      ...
    )
  } else if (type == "action") {
    tags$a(
      class = itemCl,
      href = src,
      target = "_blank",
      ...
    )
  } else {
    tags$a(
      class = itemCl,
      href = src,
      target = "_blank",
      tags$div(
        class = "d-flex w-100 justify-content-between",
        tags$h5(class = "mb-1", title),
        tags$small(subtitle)
      ),
      tags$p(class = "mb-1", ...),
      tags$small(class = if (isTRUE(active)) NULL else "text-muted", footer)
    )
  }
}
