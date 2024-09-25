
# shiny386

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/shiny386)](https://CRAN.R-project.org/package=shiny386)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/RinteRface/shiny386/workflows/R-CMD-check/badge.svg)](https://github.com/RinteRface/shiny386/actions)
<!-- badges: end -->

The goal of shiny386 is to provide an old school Bootstrap 4 template
for Shiny. It is built on top of the [Bootstrap
386](http://kristopolous.github.io/BOOTSTRA.386/demo.html) HTML
template.

## Installation

You can install the released version of shiny386 from Github with:

``` r
pak::pak("RinteRface/shiny386")
```

## Demo

<div class="card bslib-card bslib-mb-spacing html-fill-item html-fill-container" data-bslib-card-init data-full-screen="false" data-require-bs-caller="card()" data-require-bs-version="5" id="bslib-card-2409" style="margin: 0 auto; float: none;">
<div class="card-body bslib-gap-spacing html-fill-item html-fill-container" style="margin-top:auto;margin-bottom:auto;flex:1 1 auto;">
<iframe class="html-fill-item" src="https://shinylive.io/r/editor/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAdzgCMAnRRASwgGdSoAbbgCgA6EAATCBYdgAs2ATwDMADgBs43ENEM4qIu2EBeYQUFhJpUqnaIA9FYZtyDAGZQCcDAHMWpSQFc6GFiJbezgnFzgAWmoodhgIggYoCCtVMRMzC2tbLSIMBiiYmAwiBndxAEohSoghbhZGKAYZPilZarqGppbpCHllap8WYQAeCOFUKHc4AH1FJUERAkaAE1nlBdFSL244fTSAWRlDFdV1NIAVaV0WXRgjpYZl05FhRj5ytReH1bqIAGs1vNJJpHHtxKZzJYbNQYR4iER3DsMMQYKlxPsSnAKp9RI54Q4wWAAMIrYR4ogOcTqarVITsUIAN1CIzGjh8EAIWxIfDYqB8pFwwiI-L5AuE9PY7ECEHKwhAAF86T0ZABBdB8QaC+kMJkMcpgeUAXSAA" height="800" width="100%" style="border: 1px solid rgba(0,0,0,0.175); border-radius: .375rem;" allowfullscreen="" allow="autoplay" data-external="1"></iframe>
</div>
<bslib-tooltip placement="auto" bsOptions="[]" data-require-bs-version="5" data-require-bs-caller="tooltip()">
<template>Expand</template>
<button aria-expanded="false" aria-label="Expand card" class="bslib-full-screen-enter badge rounded-pill"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" style="height:1em;width:1em;fill:currentColor;" aria-hidden="true" role="img"><path d="M20 5C20 4.4 19.6 4 19 4H13C12.4 4 12 3.6 12 3C12 2.4 12.4 2 13 2H21C21.6 2 22 2.4 22 3V11C22 11.6 21.6 12 21 12C20.4 12 20 11.6 20 11V5ZM4 19C4 19.6 4.4 20 5 20H11C11.6 20 12 20.4 12 21C12 21.6 11.6 22 11 22H3C2.4 22 2 21.6 2 21V13C2 12.4 2.4 12 3 12C3.6 12 4 12.4 4 13V19Z"/></svg></button>
</bslib-tooltip>
<script data-bslib-card-init>bslib.Card.initializeAllCards();</script>
</div>

The original app may be found
[here](https://shiny.rstudio.com/gallery/bus-dashboard.html)

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
