# Create a Bootstrap 386 checkbox

Create a Bootstrap 386 checkbox

## Usage

``` r
checkbox_input_386(inputId, label, value = FALSE, width = NULL)
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

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  checkbox_input_386("check", "Check me", TRUE),
  verbatimTextOutput("val")
 )

 server <- function(input, output, session) {
   output$val <- renderPrint(input$check)
 }
 shinyApp(ui, server)
}
```
