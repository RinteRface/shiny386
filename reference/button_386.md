# Bootstrap 386 action button

Bootstrap 386 action button

## Usage

``` r
button_386(inputId, label, status = NULL, icon = NULL, width = NULL, ...)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  The contents of the button or link–usually a text label, but you could
  also use any other HTML, like an image.

- status:

  Button color.

- icon:

  An optional [`icon()`](https://rdrr.io/pkg/shiny/man/icon.html) to
  appear on the button.

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

- ...:

  Named attributes to be applied to the button or link.

## Value

A shiny tag.

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  button_386(
   "btn",
   HTML(paste("Value", textOutput("val"), sep = ":")),
   icon = icon("thumbs-up"),
   class = "btn-lg"
  )
 )

 server <- function(input, output) {
   output$val <- renderText(input$btn)
 }

 shinyApp(ui, server)
}
```
