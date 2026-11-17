# Create a Bootstrap 386 Jumbotron

Create a Bootstrap 386 Jumbotron

## Usage

``` r
jumbotron_386(..., title = NULL)
```

## Arguments

- ...:

  Any element.

- title:

  Jumbotron title.

## Value

A shiny tag

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  jumbotron_386(
   title = "Jumbotron 386",
   p("Hello World"),
   button_386("btn", "More", class = "btn-lg btn-block")
  )
 )

 server <- function(input, output, session) {}

 shinyApp(ui, server)
}
```
