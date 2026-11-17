# Create a Bootstrap 386 text input

Create a Bootstrap 386 text input

## Usage

``` r
text_input_386(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::textInput()`](https://rdrr.io/pkg/shiny/man/textInput.html).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  textInput("caption", "Caption", "Data Summary"),
  verbatimTextOutput("value")
 )

 server <- function(input, output, session) {
  output$value <- renderText({ input$caption })
 }
 shinyApp(ui, server)

}
```
