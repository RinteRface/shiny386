#' Bootstrap 386 action button
#'
#' @inheritParams shiny::actionButton
#' @param status Button color.
#' @return A shiny tag.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386(
#'    "btn",
#'    HTML(paste("Value", textOutput("val"), sep = ":")),
#'    icon = icon("thumbs-up"),
#'    class = "btn-lg"
#'   )
#'  )
#'
#'  server <- function(input, output) {
#'    output$val <- renderText(input$btn)
#'  }
#'
#'  shinyApp(ui, server)
#' }
button_386 <- function(inputId, label, status = NULL, icon = NULL, width = NULL, ...) {

  btn_cl <- paste0(
    "btn",
    if (is.null(status)) {
      " btn-primary"
    } else {
      paste0(" btn-", status)
    },
    " action-button"
  )

  value <- restoreInput(id = inputId, default = NULL)

  # custom right margin
  if (!is.null(icon)) icon$attribs$class <- paste0(
    icon$attribs$class, " mr-1"
  )

  tags$button(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    type = "button",
    class = btn_cl,
    `data-val` = value,
    list(icon, label), ...
  )
}



#' Create a Bootstrap 386 dropdown container for buttons/links
#'
#' @param ... Slot for \link{dropdown_item_386}.
#' @param inputId Unique input id.
#' @param label Dropdown label.
#' @param status Button status.
#' @param open Whether to open the dropdown at start. Default to FALSE.
#'
#' @return A shiny tag
#' @export
#' @rdname dropdown
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'    dropdown_386(
#'      inputId = "plop",
#'      label = "Menu",
#'      status = "danger",
#'      open = FALSE,
#'      dropdown_item_386(inputId = "btn1", label = "button 1"),
#'      dropdown_item_386(href = "https://www.google.com/", label = "More")
#'    )
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$btn1))
#'  }
#'  shinyApp(ui, server)
#' }
dropdown_386 <- function(..., inputId = NULL, label, status = NULL, open = FALSE) {

  if (!is.null(status)) validate_status(status)

  tags$div(
    class = "btn-group",
    role = "group",
    `aria-label` = "Button group with nested dropdown",
    tags$button(
      type = "button",
      `data-toggle` = "dropdown",
      class = if (!is.null(status)) sprintf("btn btn-%s", status) else NULL,
      label
    ),
    tags$div(
      class = if (open) "btn-group show" else "btn-group",
      role = "group",
      tags$button(
        id = inputId,
        type = "button",
        class = if (!is.null(status)) {
          sprintf("btn btn-%s dropdown-toggle", status)
        } else {
          "btn dropdown-toggle"
        },
        `data-toggle` = "dropdown",
        `aria-haspopup` = "true",
        `aria-expanded` = if (open) "true" else "false"
      ),
      tags$div(
        class = if (open) "dropdown-menu show" else "dropdown-menu",
        `aria-labelledby` = inputId,
        style = "
          position: absolute;
          transform: translate3d(0px, 14px, 0px);
          top: 0px;
          left: 0px;
          will-change: transform;
        ",
        `x-placement` = "bottom-start",
        ...
      )
    )
  )
}



#' Create a Bootstrap 386 dropdown container for buttons/links
#'
#' @param inputId id of the dropdown to open.
#' @param session Shiny session object.
#'
#' @return A message from R to JavaScript through the websocket.
#' @export
#' @rdname dropdown
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   fluidRow(
#'     button_386("open", "Open dropdown", class = "btn-lg"),
#'     dropdown_386(
#'       inputId = "plop",
#'       label = "Menu",
#'       dropdown_item_386(inputId = "btn1", label = "button 1"),
#'       dropdown_item_386(href = "https://www.google.com/", label = "More")
#'     )
#'   )
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$plop))
#'    observeEvent(input$open, {
#'      open_dropdown_386("plop")
#'    })
#'    observeEvent(req(input$plop), {
#'      showNotification("Dropdown opened!")
#'    })
#'  }
#'  shinyApp(ui, server)
#' }
open_dropdown_386 <- function(inputId, session = shiny::getDefaultReactiveDomain()) {
  session$sendInputMessage(inputId, message = NULL)
}



#' Create a Bootstrap 386 dropdown item
#'
#' Can be a simple link or an action button
#'
#' @param inputId If action button.
#' @param href If simple link.
#' @param label Button label.
#'
#' @return A shiny tag
#' @export
#' @rdname dropdown
dropdown_item_386 <- function(inputId = NULL, href = NULL, label) {

  if (!is.null(inputId) && !is.null(href)) {
    stop("Choose either inputId or href.")
  }

  tags$a(
    id = inputId,
    class = if (!is.null(inputId)) {
      "dropdown-item action-button"
    } else {
      "dropdown-item"
    },
    href = if (!is.null(href)) href else "#",
    label
  )
}


#' Create a Bootstrap 386 text input
#' @inheritParams shiny::textInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   textInput("caption", "Caption", "Data Summary"),
#'   verbatimTextOutput("value")
#'  )
#'
#'  server <- function(input, output, session) {
#'   output$value <- renderText({ input$caption })
#'  }
#'  shinyApp(ui, server)
#'
#' }
text_input_386 <- shiny::textInput



#' Update a Bootstrap 386 text input on the client
#' @inheritParams shiny::updateTextInput
#' @export
update_text_input_386 <- shiny::updateTextInput


#' Create a Bootstrap 386 text area input
#' @inheritParams shiny::textAreaInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   textAreaInput("caption", "Caption", "Data Summary"),
#'   verbatimTextOutput("value")
#'  )
#'
#'  server <- function(input, output, session) {
#'   output$value <- renderText({ input$caption })
#'  }
#'  shinyApp(ui, server)
#'
#' }
text_area_input_386 <- shiny::textAreaInput




#' Update a Bootstrap 386 text area input on the client
#' @inheritParams shiny::updateTextAreaInput
#' @export
update_text_area_input_386 <- shiny::updateTextAreaInput



#' Custom Bootstrap 386 switch input
#'
#' Similar to the shiny checkbox
#'
#' @inheritParams shiny::checkboxInput
#'
#' @return A toggle input tag.
#' @export
#' @seealso \link{update_toggle_input_386}.
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   toggle_input_386("toggle", "Toggle me", TRUE),
#'   verbatimTextOutput("val")
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$val <- renderPrint(input$toggle)
#'  }
#'  shinyApp(ui, server)
#' }
toggle_input_386 <- function(inputId, label, value = FALSE, width = NULL) {
  create_checkbox_tag(inputId, label, value = FALSE, width = NULL, type = "switch")
}



#' Update \link{toggle_input_386} on the client
#'
#' @inheritParams shiny::updateCheckboxInput
#' @export
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386("update", "Go!", class = "btn-lg"),
#'   toggle_input_386("toggle", "Switch", value = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$toggle))
#'    observeEvent(input$update, {
#'      update_toggle_input_386(
#'        session,
#'        "toggle",
#'        value = !input$toggle
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_toggle_input_386 <- function (session, inputId, label = NULL, value = NULL) {
  message <- dropNulls(list(label = label, value = value))
  session$sendInputMessage(inputId, message)
}


#' Create a Bootstrap 386 checkbox
#'
#' @inheritParams shiny::checkboxInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   checkbox_input_386("check", "Check me", TRUE),
#'   verbatimTextOutput("val")
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$val <- renderPrint(input$check)
#'  }
#'  shinyApp(ui, server)
#' }
checkbox_input_386 <- function(inputId, label, value = FALSE, width = NULL) {
  create_checkbox_tag(inputId, label, value = FALSE, width = NULL, type = "checkbox")
}


#' Update \link{checkbox_input_386} on the client
#'
#' @inheritParams shiny::updateCheckboxInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   button_386("update", "Go!", class = "btn-lg"),
#'   checkbox_input_386("check", "Checked", value = TRUE)
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe(print(input$check))
#'    observeEvent(input$update, {
#'      update_checkbox_input_386(
#'        session,
#'        "toggle",
#'        value = !input$check
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_checkbox_input_386 <- update_toggle_input_386


#' Create a Bootstrap 386 radio buttons
#'
#' @inheritParams shiny::radioButtons
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   radio_input_386("dist", "Distribution type:",
#'                   c("Normal" = "norm",
#'                     "Uniform" = "unif",
#'                     "Log-normal" = "lnorm",
#'                     "Exponential" = "exp")),
#'   plotOutput("distPlot")
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$distPlot <- renderPlot({
#'      dist <- switch(input$dist,
#'                     norm = rnorm,
#'                     unif = runif,
#'                     lnorm = rlnorm,
#'                     exp = rexp,
#'                     rnorm)
#'
#'      hist(dist(500))
#'    })
#'  }
#'  shinyApp(ui, server)
#'
#' }
radio_input_386 <- function(inputId, label, choices = NULL, selected = NULL,
                            width = NULL, choiceNames = NULL, choiceValues = NULL) {

  args <- normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")
  options <- generateOptions(
    inputId,
    selected,
    "radio",
    args$choiceNames,
    args$choiceValues
  )
  divClass <- "form-group shiny-input-radiogroup"
  tags$div(
    id = inputId,
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    class = divClass,
    tags$legend(label, `for` = inputId),
    options
  )
}



#' Change the value of a radio input on the client
#'
#' @inheritParams radio_input_386
#' @param session The session object passed to function given to shinyServer.
#'
#' @seealso [radio_input_386()]
#'
#' @examples
#' if (interactive()) {
#'
#'  ui <- page_386(
#'    p("The first radio button group controls the second"),
#'    radio_input_386("inRadioButtons", "Input radio buttons",
#'      c("Item A", "Item B", "Item C")),
#'    radio_input_386("inRadioButtons2", "Input radio buttons 2",
#'      c("Item A", "Item B", "Item C"))
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe({
#'      x <- input$inRadioButtons
#'
#'     # Can also set the label and select items
#'     update_radio_input_386(session, "inRadioButtons2",
#'       label = paste("radioButtons label", x),
#'       choices = x,
#'       selected = x
#'     )
#'   })
#'  }
#'
#'  shinyApp(ui, server)
#' }
#' @export
update_radio_input_386 <- function(session, inputId, label = NULL, choices = NULL,
                                   selected = NULL, choiceNames = NULL,
                                   choiceValues = NULL) {
  if (is.null(selected)) {
    if (!is.null(choices))
      selected <- choices[[1]]
    else if (!is.null(choiceValues))
      selected <- choiceValues[[1]]
  }
  updateInputOptions(
    session,
    inputId,
    label,
    choices,
    selected,
    "radio",
    choiceNames,
    choiceValues
  )
}



#' Create a Bootstrap 386 select input
#' @inheritParams shiny::selectInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   select_input_386("variable", "Variable:",
#'    c("Cylinders" = "cyl",
#'      "Transmission" = "am",
#'      "Gears" = "gear")),
#'    tableOutput("data")
#'  )
#'
#'  server <- function(input, output, session) {
#'   output$data <- renderTable({
#'    mtcars[, c("mpg", input$variable), drop = FALSE]
#'   }, rownames = TRUE)
#'  }
#'  shinyApp(ui, server)
#'
#' }
select_input_386 <- shiny::selectInput



#' Update a Bootstrap 386 select input on the client
#' @inheritParams shiny::updateSelectInput
#' @export
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(shiny386)
#'
#'  ui <- page_386(
#'   p("The radio group controls the select input"),
#'   radio_input_386("inCheckboxGroup", "Input checkbox",
#'                      c("Item A", "Item B", "Item C")),
#'   select_input_386("inSelect", "Select input",
#'              c("Item A", "Item B", "Item C"))
#'  )
#'
#'  server <- function(input, output, session) {
#'    observe({
#'      x <- input$inCheckboxGroup
#'
#'      # Can use character(0) to remove all choices
#'      if (is.null(x))
#'        x <- character(0)
#'
#'      # Can also set the label and select items
#'      update_select_input_386(session, "inSelect",
#'                        label = paste("Select input label", length(x)),
#'                        choices = x,
#'                        selected = tail(x, 1)
#'      )
#'    })
#'  }
#'
#'  shinyApp(ui, server)
#' }
update_select_input_386 <- shiny::updateSelectInput
