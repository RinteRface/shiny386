# Update [toggle_input_386](https://rinterface.github.io/shiny386/reference/toggle_input_386.md) on the client

Update
[toggle_input_386](https://rinterface.github.io/shiny386/reference/toggle_input_386.md)
on the client

## Usage

``` r
update_toggle_input_386(session, inputId, label = NULL, value = NULL)
```

## Arguments

- session:

  The `session` object passed to function given to `shinyServer`.
  Default is
  [`getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html).

- inputId:

  The id of the input object.

- label:

  The label to set for the input object.

- value:

  Initial value (`TRUE` or `FALSE`).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  button_386("update", "Go!", class = "btn-lg"),
  toggle_input_386("toggle", "Switch", value = TRUE)
 )

 server <- function(input, output, session) {
   observe(print(input$toggle))
   observeEvent(input$update, {
     update_toggle_input_386(
       session,
       "toggle",
       value = !input$toggle
     )
   })
 }

 shinyApp(ui, server)
}
```
