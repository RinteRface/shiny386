# Create a Bootstrap 386 card

Create a Bootstrap 386 card

Create a Bootstrap 4 card title element

Create a Bootstrap 386 card subtitle element

Create a Bootstrap 386 card link element

## Usage

``` r
card_386(..., title = NULL, status = NULL, footer = NULL)

card_title_386(title)

card_subtitle_386(subtitle)

card_link_386(href, label)
```

## Arguments

- ...:

  Card content.

- title:

  Title text.

- status:

  Card background status.

- footer:

  Card footer.

- subtitle:

  Card subtitle.

- href:

  Target url.

- label:

  Link text.

## Value

A shiny tag

A shiny tag.

A shiny tag.

A shiny tag.

## Examples

``` r
if (interactive()) {
 library(shiny)
 library(shiny386)
 ui <- page_386(
  card_386(
   title = "My card",
   "This is my card",
   br(),
   card_link_386(href = "https://www.google.com", "More"),
   footer = "Card footer"
  )
 )

 server <- function(input, output, session) {}
 shinyApp(ui, server)
}
```
