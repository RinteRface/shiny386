# Custom Bootstrap 386 switch input

Similar to the shiny checkbox

## Usage

``` r
toggle_input_386(inputId, label, value = FALSE, width = NULL)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- value:

  Initial value (`TRUE` or `FALSE`).

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

## Value

A toggle input tag.

## See also

[update_toggle_input_386](https://rinterface.github.io/shiny386/reference/update_toggle_input_386.md).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  toggle_input_386("toggle", "Toggle me", TRUE),
  verbatimTextOutput("val")
 )

 server <- function(input, output, session) {
   output$val <- renderPrint(input$toggle)
 }
 shinyApp(ui, server)
}
```
