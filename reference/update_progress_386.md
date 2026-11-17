# Update a [progress_386](https://rinterface.github.io/shiny386/reference/progress_386.md) on the client

Update a
[progress_386](https://rinterface.github.io/shiny386/reference/progress_386.md)
on the client

## Usage

``` r
update_progress_386(id, value, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- id:

  Progress unique id.

- value:

  New value.

- session:

  Shiny session object.

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)
 ui <- page_386(
  button_386("update", "Update"),
  br(),
  progress_386(id = "progress1", 12)
 )

 server <- function(input, output, session) {
   observeEvent(input$update, {
     update_progress_386(
       id = "progress1",
       sample(1:100, 1)
     )
   })
 }
 shinyApp(ui, server)
}
```
