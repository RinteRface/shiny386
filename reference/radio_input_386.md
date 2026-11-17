# Create a Bootstrap 386 radio buttons

Create a Bootstrap 386 radio buttons

## Usage

``` r
radio_input_386(
  inputId,
  label,
  choices = NULL,
  selected = NULL,
  width = NULL,
  choiceNames = NULL,
  choiceValues = NULL
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to select from (if elements of the list are named then
  that name rather than the value is displayed to the user). If this
  argument is provided, then `choiceNames` and `choiceValues` must not
  be provided, and vice-versa. The values should be strings; other types
  (such as logicals and numbers) will be coerced to strings.

- selected:

  The initially selected value. If not specified, then it defaults to
  the first item in `choices`. To start with no items selected, use
  `character(0)`.

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

- choiceNames, choiceValues:

  List of names and values, respectively, that are displayed to the user
  in the app and correspond to the each choice (for this reason,
  `choiceNames` and `choiceValues` must have the same length). If either
  of these arguments is provided, then the other *must* be provided and
  `choices` *must not* be provided. The advantage of using both of these
  over a named list for `choices` is that `choiceNames` allows any type
  of UI object to be passed through (tag objects, icons, HTML code,
  ...), instead of just simple text. See Examples.

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)

 ui <- page_386(
  radio_input_386("dist", "Distribution type:",
                  c("Normal" = "norm",
                    "Uniform" = "unif",
                    "Log-normal" = "lnorm",
                    "Exponential" = "exp")),
  plotOutput("distPlot")
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
 }
 shinyApp(ui, server)

}
```
