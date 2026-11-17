# Create a Bootstrap 386 badge

Create a Bootstrap 386 badge

## Usage

``` r
badge_386(..., status, rounded = FALSE)
```

## Arguments

- ...:

  Text.

- status:

  Badge status.

- rounded:

  Rounded style. Default to FALSE.

## Value

A shiny tags

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)
 ui <- page_386(
  badge_386(status = "danger", "1"),
  badge_386(status = "info", "2"),
  badge_386(status = "success", "3", rounded = TRUE)
 )

 server <- function(input, output, session) {}
 shinyApp(ui, server)
}
```
