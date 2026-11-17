# Create a Bootstrap 386 navbar page

Create a Bootstrap 386 navbar page

Create a Bootstrap 386 navbar menu

Update a Bootstrap 386 navbar on the client

## Usage

``` r
navbar_page_386(
  title,
  ...,
  id = NULL,
  selected = NULL,
  position = c("static-top", "fixed-top", "fixed-bottom"),
  header = NULL,
  footer = NULL,
  inverse = FALSE,
  windowTitle = title
)

navbar_menu_386(...)

update_navbar_page_386(...)
```

## Arguments

- title:

  The title to display in the navbar

- ...:

  For `navbar_page_386()`,
  [`tab_panel_386()`](https://rinterface.github.io/shiny386/reference/tab_panel_386.md)
  and `navbar_menu_386()` elements. For `navbar_menu_386()` and
  `update_navbar_page_386()`, passed to
  [`shiny::navbarMenu()`](https://rdrr.io/pkg/shiny/man/navbarPage.html)
  and
  [`shiny::updateNavbarPage()`](https://rdrr.io/pkg/shiny/man/updateTabsetPanel.html)
  respectively.

- id:

  If provided, you can use `input$`*`id`* in your server logic to
  determine which of the current tabs is active. The value will
  correspond to the `value` argument that is passed to
  [`tabPanel()`](https://rdrr.io/pkg/shiny/man/tabPanel.html).

- selected:

  The `value` (or, if none was supplied, the `title`) of the tab that
  should be selected by default. If `NULL`, the first tab will be
  selected.

- position:

  Determines whether the navbar should be displayed at the top of the
  page with normal scrolling behavior (`"static-top"`), pinned at the
  top (`"fixed-top"`), or pinned at the bottom (`"fixed-bottom"`). Note
  that using `"fixed-top"` or `"fixed-bottom"` will cause the navbar to
  overlay your body content, unless you add padding, e.g.:
  `tags$style(type="text/css", "body {padding-top: 70px;}")`

- header:

  Tag or list of tags to display as a common header above all tabPanels.

- footer:

  Tag or list of tags to display as a common footer below all tabPanels

- inverse:

  `TRUE` to use a dark background and light text for the navigation bar

- windowTitle:

  the browser window title (as a character string). The default value,
  `NA`, means to use any character strings that appear in `title` (if
  none are found, the host URL of the page is displayed by default).

## Value

A shiny tag

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- navbar_page_386(
  "App Title",
  id = "tabset",
  tab_panel_386(
    "Tab 1",
    radio_input_386(
      "dist", "Distribution type:",
      c("Normal" = "norm",
        "Uniform" = "unif",
        "Log-normal" = "lnorm",
        "Exponential" = "exp")
    ),
    plotOutput("distPlot")
  ),
  tab_panel_386(
    "Tab 2",
    select_input_386(
      "variable", "Variable:",
      c("Cylinders" = "cyl",
        "Transmission" = "am",
        "Gears" = "gear")
    ),
    tableOutput("data")
  ),
  navbar_menu_386(
   "More",
   tab_panel_386("Summary", "Extra content 1"),
   "----",
   "Section header",
   tab_panel_386("Table", "Extra content 2")
  )
 )

 server <- function(input, output, session) {
   output$distPlot <- renderPlot({
     dist <- switch(input$dist,
                    norm = rnorm,
                    unif = runif,
                    lnorm = rlnorm,
                    exp = rexp,
                    rnorm)

     hist(dist(500))
   })

   output$data <- renderTable({
     mtcars[, c("mpg", input$variable), drop = FALSE]
   }, rownames = TRUE)

   observe(print(input$tabset))

 }
 shinyApp(ui, server)
}
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- navbar_page_386(
  "App Title",
  id = "tabset",
  selected = "Tab 2",
  header = radio_input_386("controller", "Update tab", 1:4),
  tabPanel(
    "Tab 1",
    "Content 1"
  ),
  tabPanel(
    "Tab 2",
    "Content 2"
  ),
  navbar_menu_386(
    "More",
    tab_panel_386("Tab 3", "Extra content 1"),
    "----",
    "Section header",
    tab_panel_386("Tab 4", "Extra content 2")
  )
 )

 server <- function(input, output, session) {
   observeEvent(input$controller, {
     update_navbar_page_386(session, "tabset",
                            selected = paste("Tab", input$controller)
     )
   })
 }
 shinyApp(ui, server)

}
```
