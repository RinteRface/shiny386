
# shiny386

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/shiny386)](https://CRAN.R-project.org/package=shiny386)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build status](https://github.com/RinteRface/shiny386/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/shiny386/actions)
<!-- badges: end -->

The goal of shiny386 is to provide an old school Bootstrap 4 template for Shiny. It is built on top of the [Bootstrap 386](http://kristopolous.github.io/BOOTSTRA.386/demo.html) HTML template.

## Installation

You can install the released version of shiny386 from Github with:

``` r
remotes::install_github("RinteRface/shiny386")
```

## Demo

<iframe width="100%" src="https://dgranjon.shinyapps.io/shiny386Demo/" allowfullscreen="" frameborder="0" scrolling="no" height="822px"></iframe>

The original app may be found [here](https://shiny.rstudio.com/gallery/bus-dashboard.html)

## Example

This is a basic example which shows you how to solve a common problem:

``` r
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
```
