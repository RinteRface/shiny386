# Create a Bootstrap 386 tabset panel

Create a Bootstrap 386 tabset panel

## Usage

``` r
tabset_panel_386(
  ...,
  id = NULL,
  selected = NULL,
  type = c("tabs", "pills"),
  position = NULL
)
```

## Arguments

- ...:

  [`tabPanel()`](https://rdrr.io/pkg/shiny/man/tabPanel.html) elements
  to include in the tabset

- id:

  If provided, you can use `input$`*`id`* in your server logic to
  determine which of the current tabs is active. The value will
  correspond to the `value` argument that is passed to
  [`tabPanel()`](https://rdrr.io/pkg/shiny/man/tabPanel.html).

- selected:

  The `value` (or, if none was supplied, the `title`) of the tab that
  should be selected by default. If `NULL`, the first tab will be
  selected.

- type:

  `"tabs"`

  :   Standard tab look

  `"pills"`

  :   Selected tabs use the background fill color

  `"hidden"`

  :   Hides the selectable tabs. Use `type = "hidden"` in conjunction
      with
      [`tabPanelBody()`](https://rdrr.io/pkg/shiny/man/tabPanel.html)
      and
      [`updateTabsetPanel()`](https://rdrr.io/pkg/shiny/man/updateTabsetPanel.html)
      to control the active tab via other input controls. (See example
      below)

- position:

  Tabs position (left or right).

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  tabset_panel_386(
    id = "tabset",
    selected = "Tab 2",
    tabPanel("Tab 1", "Content 1"),
    tabPanel("Tab 2", "Content 2")
  )
 )

 server <- function(input, output, session) {
   observe(print(input$tabset))
 }
 shinyApp(ui, server)

}
```
