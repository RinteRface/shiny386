# Show a Bootstrap 386 toast on the client

Show a Bootstrap 386 toast on the client

## Usage

``` r
show_toast_386(id, options = NULL, session = getDefaultReactiveDomain())
```

## Arguments

- id:

  Toast id.

- options:

  Toast options: see
  <https://getbootstrap.com/docs/4.3/components/toasts/>.

- session:

  Shiny session

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  toast_386(
    id = "toast",
    title = "Hello",
    subtitle = "now",
    "Toast body",
    img = "https://preview-dev.tabler.io/static/logo.svg"
  ),
  button_386("launch", "Go!", class = "btn-lg")
 )

 server <- function(input, output, session) {
   observe(print(input$toast))
   observeEvent(input$launch, {
     removeNotification("notif")
     show_toast_386(
       "toast",
       options = list(
         animation = FALSE,
         delay = 3000
       )
     )
   })

   observeEvent(input$toast, {
     showNotification(
       id = "notif",
       "Toast was closed",
       type = "warning",
       duration = 1,

     )
   })
 }

 shinyApp(ui, server)
}
```
