# Create checkbox/switch input based on the selected type

Used internally by
[toggle_input_386](https://rinterface.github.io/shiny386/reference/toggle_input_386.md)
and
[checkbox_input_386](https://rinterface.github.io/shiny386/reference/checkbox_input_386.md)

## Usage

``` r
create_checkbox_tag(
  inputId,
  label,
  value = FALSE,
  width = NULL,
  type = c("switch", "checkbox")
)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- value:

  Initial value (`TRUE` or `FALSE`).

- width:

  The width of the input, e.g. `'400px'`, or `'100%'`; see
  [`validateCssUnit()`](https://rstudio.github.io/htmltools/reference/validateCssUnit.html).

- type:

  Input type. This is to be able to distinguish between switch and
  checkbox, which have slightly different design.

## Value

An input tag.
