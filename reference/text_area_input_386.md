# Create a Bootstrap 386 text area input

Create a Bootstrap 386 text area input

## Usage

``` r
text_area_input_386(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::textAreaInput()`](https://rdrr.io/pkg/shiny/man/textAreaInput.html).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  textAreaInput("caption", "Caption", "Data Summary"),
  verbatimTextOutput("value")
 )

 server <- function(input, output, session) {
  output$value <- renderText({ input$caption })
 }
 shinyApp(ui, server)

}
```
