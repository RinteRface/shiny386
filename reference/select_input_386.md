# Create a Bootstrap 386 select input

Create a Bootstrap 386 select input

## Usage

``` r
select_input_386(
  inputId,
  label,
  choices,
  selected = NULL,
  multiple = FALSE,
  selectize = FALSE,
  width = NULL,
  size = NULL
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to select from. If elements of the list are named, then
  that name — rather than the value — is displayed to the user. It's
  also possible to group related inputs by providing a named list whose
  elements are (either named or unnamed) lists, vectors, or factors. In
  this case, the outermost names will be used as the group labels
  (leveraging the `<optgroup>` HTML tag) for the elements in the
  respective sublist. See the example section for a small demo of this
  feature.

- selected:

  The initially selected value (or multiple values if
  `multiple = TRUE`). If not specified then defaults to the first value
  for single-select lists and no values for multiple select lists.

- multiple:

  Is selection of multiple items allowed?

- selectize:

  Whether to use selectize.js or not.

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

- size:

  Number of items to show in the selection box; a larger number will
  result in a taller box. Not compatible with `selectize=TRUE`.
  Normally, when `multiple=FALSE`, a select input will be a drop-down
  list, but when `size` is set, it will be a box instead.

## Note

Incompatible with selectize. Set to FALSE by default to have correct CSS
rendering.

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  select_input_386("variable", "Variable:",
   c("Cylinders" = "cyl",
     "Transmission" = "am",
     "Gears" = "gear")),
   tableOutput("data")
 )

 server <- function(input, output, session) {
  output$data <- renderTable({
   mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
 }
 shinyApp(ui, server)

}
```
