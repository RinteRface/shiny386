# Change the value of a checkbox group input on the client

Change the value of a checkbox group input on the client

## Usage

``` r
update_checkbox_group_input_386(
  session,
  inputId,
  label = NULL,
  choices = NULL,
  selected = NULL,
  choiceNames = NULL,
  choiceValues = NULL
)
```

## Arguments

- session:

  The session object passed to function given to shinyServer.

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to show checkboxes for. If elements of the list are
  named then that name rather than the value is displayed to the user.
  If this argument is provided, then `choiceNames` and `choiceValues`
  must not be provided, and vice-versa. The values should be strings;
  other types (such as logicals and numbers) will be coerced to strings.

- selected:

  The values that should be initially selected, if any.

- choiceNames, choiceValues:

  List of names and values, respectively, that are displayed to the user
  in the app and correspond to the each choice (for this reason,
  `choiceNames` and `choiceValues` must have the same length). If either
  of these arguments is provided, then the other *must* be provided and
  `choices` *must not* be provided. The advantage of using both of these
  over a named list for `choices` is that `choiceNames` allows any type
  of UI object to be passed through (tag objects, icons, HTML code,
  ...), instead of just simple text. See Examples.

## See also

[`checkbox_group_input_386()`](https://rinterface.github.io/shiny386/reference/checkbox_group_input_386.md)

## Examples

``` r
if (interactive()) {

 ui <- page_386(
   p("The first radio button group controls the second"),
   checkbox_group_input_386("inCheckboxGroup", "Input radio buttons",
     c("Item A", "Item B", "Item C")),
   checkbox_group_input_386("inCheckboxGroup2", "Input radio buttons 2",
     c("Item A", "Item B", "Item C"))
 )

 server <- function(input, output, session) {
   observe({
    x <- input$inCheckboxGroup

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    update_checkbox_group_input_386(session, "inCheckboxGroup2",
                             label = paste("Checkboxgroup label", length(x)),
                             choices = x,
                             selected = x
    )
   })
 }

 shinyApp(ui, server)
}
```
