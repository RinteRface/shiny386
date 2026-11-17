# Update a Bootstrap 386 tabset panel on the client

Update a Bootstrap 386 tabset panel on the client

## Usage

``` r
update_tabset_panel_386(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::updateTabsetPanel()`](https://rdrr.io/pkg/shiny/man/updateTabsetPanel.html).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(sidebarLayout(
 sidebarPanel(
   radio_input_386("controller", "Controller", choices = c(1, 2, 3))
 ),
 mainPanel(
   tabset_panel_386(id = "inTabset",
               tab_panel_386(title = "Panel 1", value = "panel1", "Panel 1 content"),
               tab_panel_386(title = "Panel 2", value = "panel2", "Panel 2 content"),
               tab_panel_386(title = "Panel 3", value = "panel3", "Panel 3 content")
   )
 )
 ))

 server <- function(input, output, session) {
   observeEvent(input$controller, {
     update_tabset_panel_386(session, "inTabset",
                       selected = paste0("panel", input$controller)
     )
   })
 }

 shinyApp(ui, server)
}
```
