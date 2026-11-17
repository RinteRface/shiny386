# Create a Bootstrap 386 list group container

Create a Bootstrap 386 list group container

## Usage

``` r
list_group_386(..., width = 4)
```

## Arguments

- ...:

  Slot for
  [list_group_item_386](https://rinterface.github.io/shiny386/reference/list_group_item_386.md).

- width:

  List group width. 4 by default. Between 1 and 12.

## Author

David Granjon, <dgranjon@ymail.com>

## Examples

``` r
if(interactive()){
 library(shiny)
 library(shiny386)

 shinyApp(
   ui = page_386(
      fluidRow(
       list_group_386(
       list_group_item_386(
        type = "basic",
        "Cras justo odio"
       ),
       list_group_item_386(
        type = "basic",
        "Dapibus ac facilisis in"
       ),
       list_group_item_386(
        type = "basic",
        "Morbi leo risus"
       )
      ),
      list_group_386(
       list_group_item_386(
        "Cras justo odio",
        active = TRUE,
        disabled = FALSE,
        type = "action",
        src = "http://www.google.fr"
       ),
       list_group_item_386(
        active = FALSE,
        disabled = FALSE,
        type = "action",
        "Dapibus ac facilisis in",
        src = "http://www.google.fr"
       ),
       list_group_item_386(
        "Morbi leo risus",
        active = FALSE,
        disabled = TRUE,
        type = "action",
        src = "http://www.google.fr"
       )
      ),
      list_group_386(
       list_group_item_386(
        "Donec id elit non mi porta gravida at eget metus.
        Maecenas sed diam eget risus varius blandit.",
        active = TRUE,
        disabled = FALSE,
        type = "heading",
        title = "List group item heading",
        subtitle = "3 days ago",
        footer = "Donec id elit non mi porta."
       ),
       list_group_item_386(
        "Donec id elit non mi porta gravida at eget metus.
        Maecenas sed diam eget risus varius blandit.",
        active = FALSE,
        disabled = FALSE,
        type = "heading",
        title = "List group item heading",
        subtitle = "3 days ago",
        footer = "Donec id elit non mi porta."
       )
      )
     )
   ),
   server = function(input, output) {}
 )
}
```
