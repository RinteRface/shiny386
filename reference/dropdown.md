# Create a Bootstrap 386 dropdown container for buttons/links

Can be a simple link or an action button

## Usage

``` r
dropdown_386(..., inputId = NULL, label, status = NULL, open = FALSE)

open_dropdown_386(inputId, session = shiny::getDefaultReactiveDomain())

dropdown_item_386(inputId = NULL, href = NULL, label)
```

## Arguments

- ...:

  Slot for dropdown_item_386.

- inputId:

  If action button.

- label:

  Button label.

- status:

  Button status.

- open:

  Whether to open the dropdown at start. Default to FALSE.

- session:

  Shiny session object.

- href:

  If simple link.

## Value

A shiny tag

A message from R to JavaScript through the websocket.

A shiny tag

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
   dropdown_386(
     inputId = "plop",
     label = "Menu",
     status = "danger",
     open = FALSE,
     dropdown_item_386(inputId = "btn1", label = "button 1"),
     dropdown_item_386(href = "https://www.google.com/", label = "More")
   )
 )

 server <- function(input, output, session) {
   observe(print(input$btn1))
 }
 shinyApp(ui, server)
}
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  fluidRow(
    button_386("open", "Open dropdown", class = "btn-lg"),
    dropdown_386(
      inputId = "plop",
      label = "Menu",
      dropdown_item_386(inputId = "btn1", label = "button 1"),
      dropdown_item_386(href = "https://www.google.com/", label = "More")
    )
  )
 )

 server <- function(input, output, session) {
   observe(print(input$plop))
   observeEvent(input$open, {
     open_dropdown_386("plop")
   })
   observeEvent(req(input$plop), {
     showNotification("Dropdown opened!")
   })
 }
 shinyApp(ui, server)
}
```
